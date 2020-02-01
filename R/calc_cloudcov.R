#' Calculate the cloud cover of Sentinel, Landsat or MODIS in an aoi before large data download
#' 
#' \code{calc_cloudcov} calculates the aoi cloud cover and optionally saves raster cloud
#' masks, all based on preview images. The previews are requested through \link{get_previews}.
#' Cloud cover is computed currently using one of the following options:
#' \itemize{
#' \item Haze-Optimal-Transformation (HOT) (Zhu & Helmer, 2018).
#' }
#' 
#' #' @details Using the Haze-optimal transformation (HOT), the cloud cover estimation is done on the red and blue information of the input RGB. HOT procedure is applied based on 
#' Zhu & Helmer (2018) [2]. Orignally, the base computation was introduced by Zhang et al. (2002) [1].
#' HOT seperates clear-sky pixels first from a threshold, calculates a least alternate deviation (LAD) regression from these pixels and exposes cloud pixels by the deviation of all pixels from this clear-sky line.
#' 
#' @references 
#' [1] Zhang, Y., Guindon, B., Cihlar, J., 2002. An image transform to characterize and compensate for spatial variations in thin cloud contamination of Landsat images.
#' Remote Sensing of Environment 82 (2-3), 173-187.
#'   
#' [2] Zhu, X., Helmer, E.H., 2018. An automatic method for screening clouds and cloud shadows in opticalsatellite image time series in cloudy regions.
#' Remote Sensing of Environment 214 (2018), 135-153.
#' 
#' @note if a \code{dir_out} is given cloud mask rasters and a record csv for each record is saved in \code{dir_out}.
#' 
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{get_records}.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param maxDeviation numeric, the maximum allowed deviation of calculated scene cloud cover from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover \% given by the data distributor. Default is \code{maxDeviation = 5}.
#' @param dir_out character, optional. Full path to target directory where to save the cloud masks. If \code{NULL}, cloud masks are not saved.
#' @param username character, a valid user name to the ESA Copernicus Open Access Hub. If \code{NULL} (default), the session-wide login credentials are used (see \link{login_CopHub} for details on registration).
#' @param password character, the password to the specified user account. If \code{NULL} (default) and no seesion-wide password is defined, it is asked interactively ((see \link{login_CopHub} for details on registration).
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#' 
#' @return \code{records} data.frame holding three added columns:
#' \enumerate{
#' \item cloud_mask_file: character path to the cloud mask file of the record
#' \item aoi_HOT_cloudcov_percent: numeric percentage of the calculated aoi cloud cover.
#' \item scene_HOT_cloudcov_percent: numeric percentage of the calculated scene cloud cover.
#' }
#' 
#' @author Henrik Fisser
#' 
#' @importFrom utils object.size
#' @importFrom sf st_as_sf
#' @importFrom readr read_csv write_csv cols
#' @importFrom raster stack
#' 
#' @examples
#' ## Load packages
#' library(getSpatialData)
#' library(raster)
#' library(sf)
#' library(sp)
#'
#' ## Define an AOI (either matrix, sf or sp object)
#' data("aoi_data") # example aoi
#'
#' aoi <- aoi_data[[3]] # AOI as matrix object, or better:
#' aoi <- aoi_data[[2]] # AOI as sp object, or:
#' aoi <- aoi_data[[1]] # AOI as sf object
#' # or, simply call set_aoi() without argument to interactively draw an AOI
#'
#' ## set AOI for this session
#' set_aoi(aoi)
#' view_aoi() #view AOI in viewer
#'
#' ## Define time range and platform
#' time_range <-  c("2017-08-01", "2017-08-30")
#' platform <- "Sentinel-2"
#'
#' ## set login credentials and an archive directory
#' \dontrun{
#' login_CopHub(username = "username") #asks for password or define 'password'
#' set_archive("/path/to/archive/")
#'
#' ## Use getSentinel_query to search for data (using the session AOI)
#' records <- getSentinel_query(time_range = time_range, platform = platform)
#'
#' ## Get an overview of the records
#' View(records) #get an overview about the search records
#' colnames(records) #see all available filter attributes
#' unique(records$processinglevel) #use one of the, e.g. to see available processing levels
#' 
#' ## Calculate cloud cover within the aoi
#' records_cloudcov <- calcSentinel_aoi_cloudcov(records = records, aoi = aoi) # cloud cov. calc.
#' 
#' ## Filter the records
#' records_filtered <- records_cloudcov[which(records_cloudcov$processinglevel == "Level-1C"),]
#'
#' ## Preview a single record
#' getSentinel_preview(record = records_filtered[5,])
#'
#' ## Download some datasets
#' datasets <- getSentinel_data(records = records_filtered[c(4,5,6),])
#'
#' ## Make them ready to use
#' datasets_prep <- prepSentinel(datasets, format = "tiff")
#'
#' ## Load them to R
#' r <- stack(datasets_prep[[1]][[1]][1]) #first dataset, first tile, 10m resoultion
#' }
#' 
#' @export

calc_cloudcov <- function(records, aoi = NULL,  maxDeviation = 5,
                          dir_out = NULL, username = NULL, password = NULL, verbose = TRUE) {
  
  ## Check input
  options("gSD.verbose"=verbose)
  dir_out <- .check_dir_out(dir_out,which="cloud_masks")
  classNumErr <- " has to be of class 'numeric'. But is: "
  aoi <- .check_aoi(aoi,"sp")
  records <- .check_records(records,as_df=T)
  params <- list("cloudPrbThreshold"=cloudPrbThreshold,"slopeDefault"=slopeDefault,"interceptDefault"=interceptDefault)
  check_num <- sapply(1:length(params),function(i) {
    if (!is.numeric(params[[i]]) && !is.integer(params[[i]])) {out(paste0(names(params)[[i]],classNumErr,class(params[[i]])),type=3)}
  })
  rm(check_num)
  
  cols_initial <- colnames(records)
  numRecords <- NROW(records)
  out(paste0(sep(),"\n\nProcessing ",numRecords," records\nStarting HOT\n",sep(),"\n"),verbose=verbose)
  processingTime <- c()
  previewSize <- c()
  
  # create temp dir
  tmp_dir_orig <-  base::tempdir() # in order to change it to default at the end of function
  tmp_dir <- .tmp_dir(dir_out,1,change_raster_tmp=TRUE)
  rm(tmp_dir)
  v <- verbose
  identifier <- "record_id"
  
  ## Do HOT cloud cover assessment consecutively
  records <- as.data.frame(do.call(rbind,lapply(1:numRecords,function(i) {
    
    out_status <- paste0("[Aoi cloudcov calc ",i,"/",numRecords,"] ")
    startTime <- Sys.time()
    record <- as.data.frame(records[i,])
    id <- record[[identifier]]
    sensor <- record$product
    
    if (any(is.na(c(id,sensor)))) {
      return(.handle_cc_skip(record,FALSE,dir_out))
    }
    
    is_SAR <- sensor == "Sentinel-1"

    # check if record csv exists already and if TRUE check if cloud mask exists. If both TRUE return
    # otherwise run HOT afterwards
    csv_path <- file.path(dir_out,paste0(id,".csv"))[1]
    if (file.exists(csv_path)) {
      out(paste0(out_status,"Loading (yet processed): ",id),msg=T,verbose=v)
      record <- as.data.frame(read_csv(csv_path,col_types=cols()))
      record <- .df_dates_to_chars(record)
      nms <- names(record)
      if ("cloud_mask_file" %in% nms && "preview_file" %in% nms &&
          NROW(record) > 0) {
        if (file.exists(record$cloud_mask_file)) {
          return(record)
        }
      }
    } else {
      out(paste0(out_status,"Processing: ",id),msg=T,verbose=v)
    }
    
    # if preview exists not yet get it, then run HOT
    if (sensor == "Sentinel-1") {
      record_preview <- NULL
    } else {
      
      prev_col_given <- "preview_file" %in% names(record)
      if (prev_col_given) {
        
        pfile <- record$preview_file
        preview_exists <- ifelse(is.na(pfile) || pfile == "NONE",FALSE,file.exists(pfile))
        if (preview_exists) {
          record_preview <- record
        } else {
          .check_login(records=record)
          record_preview <- tryCatch({
            get_previews(record,dir_out=dir_out,verbose=F)
          },
          error=function(err) {
            return(err)
          })
        }
      } else {
        .check_login(records=record)
        record_preview <- tryCatch({
          get_previews(record,dir_out=dir_out,verbose=F)
        },
        error=function(err) {
          return(err)
        })
      }
      
      if (inherits(record_preview,"error")) {
        .check_http_error(record_preview,record,username,password,verbose=v)
        record_preview <- tryCatch({
          get_previews(record,dir_out=dir_out,verbose=F)
        },
        error=function(err) {
          return(NULL)
        })
      }
    }
    
    options("gSD.verbose"=v) # reset verbose to original value after supressing verbose in get_previews
    verbose <- v
    
    preview_exists <- ifelse(inherits(record_preview,"data.frame"),
                             ifelse("preview_file" %in% names(record_preview),
                                    file.exists(record_preview$preview_file),FALSE),FALSE)
    
    get_preview_failed <- is.null(record_preview) || class(record_preview) %in% c("error","try-error") || isFALSE(preview_exists)
    if (get_preview_failed) {
      record[["preview_file_jpg"]] <- "NONE"
      record[["preview_file"]] <- "NONE"
      record_preview <- record
    } else {
      # pass preview to HOT function
      record_preview <- as.data.frame(record_preview)
      preview <- stack(record_preview$preview_file)
      record_cc <- try(getSpatialData:::calc_hot_cloudcov(record=record_preview,
                                         preview=preview,
                                         aoi=aoi,
                                         cloudPrbThreshold=cloudPrbThreshold,
                                         maxDeviation=maxDeviation,
                                         slopeDefault=slopeDefault,
                                         interceptDefault=interceptDefault,
                                         cols=.cloudcov_colnames(),
                                         dir_out=dir_out,
                                         verbose=verbose))
    }
    
    if (isTRUE(get_preview_failed) || class(record_cc) != "data.frame" || is.na(record_cc)) {
      record_cc <- getSpatialData:::.handle_cc_skip(record_preview,is_SAR,dir_out)
    }
    
    previewSize <- c(previewSize,object.size(preview))
    endTime <- Sys.time()
    if (i <= 10) {
      elapsed <- as.numeric(difftime(endTime,startTime,units="mins"))
      processingTime <- c(processingTime,elapsed)
    }
    if (numRecords >= 15 && i == 10) {
      .calcHOTProcTime(numRecords=numRecords,i=i,processingTime=processingTime,previewSize=previewSize)
    }
    
    # write csv if desired
    if (!is.null(dir_out)) {
      if ("footprint" %in% names(record_cc)) {
        # remove footprint list column for writing csv
        cols_remain <- setdiff(1:NCOL(record_cc),c(which(names(record_cc) == "footprint")))
        # unlist columns for writing csv
        record_cc <- record_cc[,cols_remain]
        record_cc <- .unlist_df(record_cc)
      }
      write_csv(record_cc,csv_path)
    }
    
    record_cc <- .unlist_df(record_cc)
    return(record_cc)
    
  })),stringsAsFactors=F)
  
  out(paste0("\n",sep(),"\nFinished aoi cloud cover calculation\n",sep(),"\n"))
  .tmp_dir(dir_out,2,TRUE,tmp_dir_orig)
  records <- .column_summary(records,cols_initial)
  return(records)

}


