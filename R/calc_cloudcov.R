#' Calculate the cloud cover of optical Sentinel, Landsat or MODIS data in an aoi based on small previews
#' 
#' \code{calc_cloudcov} calculates the aoi cloud cover and optionally saves raster cloud
#' masks, all based on preview images. The previews are requested through \link{get_previews}. 
#' You may call \link{get_previews} before \code{calc_cloudcov}. In this case the previews will be reloaded.
#' If one or more records have been processed in \code{calc_cloudcov} and in the same \code{dir_out} before 
#' they will be reloaded. 
#' 
#' @details Using the Haze-optimal transformation (HOT), the cloud cover estimation is done on the 
#' red and blue information of the input RGB. HOT procedure is applied to the red and blue bands [1-3]. 
#' Originally, the base computation was introduced by Zhang et al. (2002) [2]. 
#' The computation done in \code{calc_cloudcov} includes the following steps:
#' \enumerate{
#' \item Binning: extract low red values and their highest blue values
#' \item Regression: calculate linear regression of these values
#' \item HOT layer: compute haze-optimal transformation cloud likelihood layer
#' \item Iterative thresholding: Find a HOT threshold by iterative comparison with
#' the provider scene cloud cover.
#' \item Aoi cloud cover calculation: Calculate the aoi cloud cover from the binary
#' cloud mask.
#' }
#' 
#' HOT separates clear-sky 
#' pixels first from a threshold, calculates a linear regression from these pixels and exposes 
#' cloud pixels by the deviation of all pixels from this clear-sky line.
#' 
#' @references 
#' [1] Chen, S, Chen, X., Chen, J., Jia, P., 2015. An Iterative Haze Optimized Transformation for 
#' Automatic Cloud/Haze
#' Detection of Landsat Imagery. IEEE Transactions on Geoscience and Remote Sensing 54 (5), 2682-2694.
#' 
#' [2] Zhang, Y., Guindon, B., Cihlar, J., 2002. An image transform to characterize and compensate 
#' for spatial variations in thin cloud contamination of Landsat images.
#' Remote Sensing of Environment 82 (2-3), 173-187.
#'   
#' [3] Zhu, X., Helmer, E.H., 2018. An automatic method for screening clouds and cloud shadows in 
#' opticalsatellite image time series in cloudy regions.
#' Remote Sensing of Environment 214 (2018), 135-153.
#'  
#' @param records data.frame, one or multiple records (each represented by one row), as it is 
#' returned by \link{get_records}.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point 
#' (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to 
#' have two columns (longitude and latitude) and at least three rows (each row representing one 
#' #corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, 
#' it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally 
#' for all queries within the running session. If \code{aoi} is undefined, the AOI that has been 
#' set using \link{set_aoi} is used.
#' @param write_records logical specifies if the records (row by row) shall be written.
#' @param write_cloud_masks logical specifies if the cloud mask tifs shall be written.
#' @param max_deviation numeric, the maximum allowed deviation of calculated scene cloud cover 
#' from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover 
#' \% given by the data distributor. Default is 2.
#' @param dir_out character. If \code{dir_out} is not NULL the given cloud mask rasters and 
#' a record file for each record will be saved in \code{dir_out}. If it is NULL, the session 
#' \code{dir_out} is used.
#' If no session \code{dir_out} is set through \link{set_archive} an error is thrown.
#' @param username character, a valid user name to the ESA Copernicus Open Access Hub. If \code{NULL} 
#' (default), the session-wide login credentials are used (see \link{login_CopHub} for details on 
#' registration).
#' @param password character, the password to the specified user account. If \code{NULL} (default) 
#' and no seesion-wide password is defined, it is asked interactively ((see \link{login_CopHub} for 
#' details on registration).
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile 
#' on the console. Default is TRUE.
#' @param ... further arguments that can be passed to \link{write_records} for writing record files.
#' Can be: driver, append.
#' 
#' @inheritParams get_records
#' 
#' @return \code{records} data.frame with three added columns:
#' \enumerate{
#' \item cloud_mask_file: character path to the cloud mask file of the record
#' \item aoi_HOT_cloudcov_percent: numeric percentage of the calculated aoi cloud cover.
#' \item scene_HOT_cloudcov_percent: numeric percentage of the calculated scene cloud cover.
#' }
#' 
#' @author Henrik Fisser
#' 
#' @importFrom raster stack
#' 
#' @export

calc_cloudcov <- function(records, max_deviation = 2,
                          aoi = NULL,
                          write_records = TRUE, write_cloud_masks = TRUE,
                          dir_out = NULL,
                          username = NULL, password = NULL, as_sf = TRUE, verbose = TRUE, ...) {
  
  TRY_ERROR <- TRY_ERROR()
  DF <- DATAFRAME()
  NONE <- "NONE"
  
  ## Check input
  .check_verbose(verbose)
  .check_as_sf(as_sf)
  aoi <- .check_aoi(aoi, SF())
  .check_numeric(max_deviation, "max_deviation")
  dir_out <- .check_dir_out(dir_out, which="cloud_masks")

  .check_character(username, "username")
  .check_character(password, "password")
  records <- .check_records(records, .cloudcov_get_needed_cols(), as_sf=FALSE)
  # additional args to be passed to write_records
  dots <- list(...)
  driver <- dots$driver
  append <- dots$append
  
  name_footprint <- name_footprint()
  preview_file <- name_preview_file()
  preview_file_jpg <- name_preview_file_jpg()
  cloud_mask_file <- name_cloud_mask_file()
  cols_initial <- colnames(records)
  n_records <- NROW(records)
  
  out(paste0(sep(),"\nProcessing ",
             n_records,
             " records", 
             sep(), "\n"), verbose=verbose)
  processingTime <- c()

  v <- verbose
  identifier <- name_record_id()

  ## Do HOT cloud cover assessment
  records <- do.call(rbind, lapply(1:n_records, function(i) {
    
    out_status <- paste0("[Aoi cloudcov calc ", i, "/", n_records, "] ")
    startTime <- Sys.time()
    record <- records[i,]
    id <- record[[identifier]]
    sensor <- record[[name_product()]]
    
    if (any(is.na(c(id, sensor)))) {
      return(.cloudcov_handle_skip(record))
    }
    
    cloudcov_supported <- .cloudcov_supported(record)

    # check if record file exists already and if TRUE check if cloud mask exists. If both TRUE return
    # otherwise run HOT afterwards
    record_path <- .generate_records_filename(file_name = id, dir_out = dir_out, driver = driver)
    if (.check_file_exists(record_path)) {
      out(paste0(out_status,"Reloading processed record: ", id), msg = T, verbose = v)
      record <- read_records(record_path, as_sf = F, verbose = FALSE)
      nms <- names(record)
      if (cloud_mask_file %in% nms && preview_file %in% nms &&
          NROW(record) > 0) {
        if (.check_file_exists(record[[cloud_mask_file]])) {
          return(record)
        }
      }
    } else {
      out(paste0(out_status,"Processing: ",id), msg=T, verbose=v)
    }

    # if preview exists not yet: get it. Then run HOT
    if (cloudcov_supported) {
      preview_given <- preview_file %in% names(record)
      if (preview_given) {
        pfile <- record[[preview_file]]
        preview_exists <- ifelse(is.na(pfile) || pfile == NONE, FALSE, file.exists(pfile))
        if (preview_exists) {
          record_preview <- record
        } else {
          preview_given <- FALSE
        }
      } 
      if (!preview_given) {
        .check_login(records=record)
        record_preview <- tryCatch({
          get_previews(record, username = username, password = password, dir_out = dir_out, verbose = F)
        },
        error=function(err) {
          return(err)
        })
      }
      
      if (inherits(record_preview, ERROR())) {
        record_preview <- tryCatch({
          get_previews(record, username = username, password = password, dir_out = dir_out, verbose = F)
        },
        error=function(err) {
          return(err)
        })
      }
    } else {
      record_preview <- NULL
    }
    
    .set_verbose(v) # reset verbose to original value after supressing verbose in get_previews
    verbose <- v
    
    preview_worked <- inherits(record_preview, DF)
    if (preview_worked && preview_file %in% names(record_preview)) {
      preview_worked <- !is.na(record_preview[[preview_file]])
      if (preview_worked) {
        preview_worked <- .check_file_exists(record_preview[[preview_file]])
      }
    }
    if (preview_worked) {
      # pass preview to HOT function
      preview <- stack(record_preview[[preview_file]])
      record_cc <- try(calc_hot_cloudcov(record = record_preview,
                                         preview = preview,
                                         aoi = aoi,
                                         max_deviation = max_deviation,
                                         cols = .cloudcov_colnames(),
                                         write_cmasks = write_cloud_masks,
                                         dir_out = dir_out,
                                         verbose = verbose))
    } else {
      record[[preview_file_jpg]] <- NONE
      record[[preview_file]] <- NONE
      record_preview <- record
    }
    
    if (!preview_worked || !inherits(record_cc, DF) || is.na(record_cc)) {
      record_cc <- .cloudcov_handle_skip(record_preview)
    }
    
    record_cc <- .unlist_df(record_cc) # ensure no column as a whole is a list before writing
    
    col_names <- names(record_cc)
    footprint_tmp <- "footprint_tmp"
    names(record_cc)[which(col_names == name_footprint)] <- footprint_tmp
    record_cc[[name_footprint]] <- record_cc[[footprint_tmp]]
    record_cc[[footprint_tmp]] <- NULL
    
    if (write_records) {
      # write record
      write_records(record_cc, file = record_path, append = append, verbose = FALSE)
    }
    verbose <- v
    .set_verbose(verbose)
    return(as.data.frame(record_cc))
    
  }))
  
  # when we get a matrix from do.call rbind convert to data.frame
  # otherwise it is already sf data.frame
  if (is.matrix(records)) records <- as.data.frame(records)
  records <- .unlist_df(records)

  # ensure spatial footprints
  records <- .eval_records_footprints(records, as_sf = as_sf)
  records <- .check_records(records, as_sf = as_sf)
  out(paste0(sep(),"\nFinished aoi cloud cover calculation",
             sep(),"\n"))
  records <- .column_summary(records, cols_initial)
  return(records)

}
