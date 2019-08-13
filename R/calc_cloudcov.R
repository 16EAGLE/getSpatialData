#' Calculate the cloud cover of Sentinel, Landsat or MODIS in an aoi before large data download
#' 
#' \code{calc_cloudcov} requests previews using \code{get_preview} and calculates the aoi cloud cover
#' upon these images. Cloud cover is computed currently using one of the following options:
#' \itemize{
#' \item Haze-Optimal-Transformation (HOT) (Zhu & Helmer (2018)).
#' }
#' 
#' #' @details When using HOT the estimation of the cloud cover is done on the red and blue information of the input RGB. Haze-optimal transformation (HOT) procedure is applied based on 
#' Zhu & Helmer (2018), https://data.fs.usda.gov/research/pubs/iitf/ja_iitf_2018_Zhu.pdf. Orignally, the algorithm was introduced by Zhang et al. (2002)
#' "An image transform to characterize and compensate for spatial variations in thin cloud contamination of Landsat images", Remote Sensing of Environment 82, 2-3.
#' HOT seperates clear-sky pixels first from a threshold, calculates a least alternate deviation (LAD) regression from these pixels and exposes cloud pixels by the deviation of all pixels from this clear-sky line.
#' 
#' @note if a \code{dir_out} is given cloud mask rasters and a record csv for each record is saved in \code{dir_out}.
#' 
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{get_records}.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param maxDeviation numeric, the maximum allowed deviation of calculated scene cloud cover from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover \% given by the data distributor. Default is \code{maxDeviation = 20}.
#' @param cloudPrbThreshold numeric, the threshold of the HOT cloud probability layer (0-100 \%) below which pixels are considered as clear sky. Default is \code{cloudPrbThreshold = 40}. It will be dynamically adjusted according to the input in \code{maxDeviation}.
#' @param slopeDefault numeric, value taken as slope ONLY if least-alternate deviation regression fails.  Default is 1.4, proven to work well for common land surfaces.f default values. In this case cloud cover will be set to 9999 \% for the given record.
#' @param interceptDefault numeric, value taken as intercept ONLY if least-alternate deviation regression fails. Default is -10, proven to work well for common land surfaces.
#' @param dir_out character, optional. Full path to target directory where to save the cloud masks. If \code{NULL}, cloud masks are not saved.
#' @param username character, a valid user name to the ESA Copernicus Open Access Hub. If \code{NULL} (default), the session-wide login credentials are used (see \link{login_CopHub} for details on registration).
#' @param password character, the password to the specified user account. If \code{NULL} (default) and no seesion-wide password is defined, it is asked interactively ((see \link{login_CopHub} for details on registration).
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#' 
#' @return \code{records} data.frame holding three added columns:
#' \enumerate{
#' \item cloud_mask_file: character path to the cloud mask file of the record
#' \item aoi_HOT_cloudcov_percent: numeric percentage of the calculated aoi cloud cover.
#' \item aoi_HOT_mean_probability: numeric mean aoi cloud cover probability (0-100).
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

calc_cloudcov <- function(records, aoi = NULL,  maxDeviation = 20,
                          cloudPrbThreshold = 35, slopeDefault = 1.4, interceptDefault = -10, 
                          dir_out = NULL, username = NULL, password = NULL, verbose = TRUE) {
  
  ## Check input
  options("gSD.verbose"=verbose)
  aoiClass <- class(aoi)
  classNumErr <-  "has to be of class 'numeric'. But is: "
  if (aoiClass[1] != "sf" && aoiClass != "SpatialPolygonsDataFrame" && aoiClass != "SpatialPolygons" && aoiClass != "matrix") {out(paste0("Aoi has to be of class 'sp' or 'sf' or 'matrix' but is of class:\n",aoiClass),type=3)}
  .check_records(records)
  params <- list("cloudPrbThreshold"=cloudPrbThreshold,"slopeDefault"=slopeDefault,"interceptDefault"=interceptDefault)
  check_num <- sapply(1:length(params),function(i) {
    if (!is.numeric(params[[i]])) {out(paste0(names(params)[[i]],classNumErr,class(params[[i]])),type=3)}
  })
  .check_login()
  cols_initial <- colnames(records)
  numRecords <- nrow(records)
  footprint <- records$footprint
  out(paste0(sep(),"\n\n",numRecords," records to be processed\nStarting HOT...\n",sep(),"\n"),verbose=verbose)
  processingTime <- c()
  previewSize <- c()
  ## Do HOT cloud cover assessment consecutively
  records <- as.data.frame(do.call(rbind,lapply(1:numRecords,function(i) {
    out(paste0("[Aoi cloudcov calc ",i,"/",numRecords,"]"),msg=T,verbose=verbose)
    startTime <- Sys.time()
    record <- records[i,]
    identifier <- "record_id"
    id <- record[[identifier]]
    sensor <- record$product
    is_SAR <- sensor == "Sentinel-1"
    v <- verbose
    # check if record csv exists already and if TRUE check if cloud mask exists. If both TRUE return
    # otherwise run HOT afterwards
    csv_path <- file.path(dir_previews,paste0(id,".csv"))[1]   
    if (file.exists(csv_path)) {
      out(paste0("Loading because already processed: ",id),msg=T)
      record_cc <- as.data.frame(read_csv(csv_path,col_types=cols()))
      nms <- names(record_cc)
      if ("cloud_mask_file" %in% nms && "preview_file" %in% nms) {
        if (file.exists(record_cc$cloud_mask_file)) {
          return(record_cc)
        }
      }
    } else {
      out(paste0("Processing: ",id),msg=T)
    }
    # if preview exists not yet get it, then run HOT
    if (sensor == "Sentinel-1") {
      record_preview <- NULL
    } else {
      prev_col_given <- "preview_file" %in% names(record)
      if (prev_col_given) {
        pfile <- record$preview_file
        preview_exists <- ifelse(is.na(pfile),FALSE,file.exists(pfile))
        if (preview_exists) {
          record_preview <- record
        } else {
          record_preview <- try(get_previews(record,dir_out=dir_out,verbose=F))
        }
      } else {
        record_preview <- try(get_previews(record,dir_out=dir_out,verbose=F))
      }
    }
    options("gSD.verbose"=v) # reset verbose to original value after supressing verbose in get_previews
    verbose <- v
    # pass preview to HOT function
    cond <- !is.null(record_preview) && 
      !inherits(record_preview,"try-error") && 
      !is.na(record_preview$preview_file) &&
      file.exists(record_preview$preview_file)
    if (cond) {
      preview <- stack(record_preview$preview_file)
      record_cc <- try(calc_hot_cloudcov(record=record_preview,
                                         preview=preview,
                                         aoi=aoi,
                                         cloudPrbThreshold=cloudPrbThreshold,
                                         maxDeviation=maxDeviation,
                                         slopeDefault=slopeDefault,
                                         interceptDefault=interceptDefault,
                                         dir_out=dir_out,
                                         verbose=verbose))
    }
    if (isFALSE(cond) || class(record_cc)[1] != "sf" || is.na(record_cc)) {
      record_cc <- .handle_cc_skip(record_preview,is_SAR,dir_out)
    }
    previewSize <- c(previewSize,object.size(preview))
    endTime <- Sys.time()
    if (i <= 5) {
      elapsed <- as.numeric(difftime(endTime,startTime,units="mins"))
      processingTime <- c(processingTime,elapsed)
    }
    if (numRecords >= 10 && i == 5) {
      .calcHOTProcTime(numRecords=numRecords,i=i,processingTime=processingTime,previewSize=previewSize)
    }
    if (!is.null(dir_out)) write_csv(record_cc,csv_path)
    return(record_cc)
  })))
  out(paste0("\n",sep(),"\nFinished preview cloud cover calculation\n",sep(),"\n"))
  records <- .column_summary(records,cols_initial)
  records$footprint <- footprint
  records <- st_as_sf(records)
  return(records)
}