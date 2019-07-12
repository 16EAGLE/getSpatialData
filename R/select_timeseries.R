#' Selects records for a time series according to aoi cloud cover and temporal characteristics
#' 
#' @details For running the selection you have to process \link{calc_cloudcov} first.
#'  
#' \code{select_timeseries} is selection mode "TS" from \link{select_records}
#' 
#' @param records data.frame as returned by \link{calc_cloudcov}, either complete or subsetted. Records will be selected from these records.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param dir_out character directory where to save the cloud mask mosaics and the RGB preview mosaic.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps. 
#' For example, if a scene from 20th May 2019 is selected for a timestamp and \code{min_distance == 10} then the next timestamp will not include scenes 
#' in <= 10 days after 20th May 2019. The first scene the next timestamp could include would be the 31st May 2019 thus.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed. This determines how close together
#' the selected records for one timestamp are temporally (if mosaicking is needed).
#' @param max_cloudcov numeric maximum cloud cover (\%) the 'cloud-free' mosaic is allowed to have. Default is 1.
#' @param max_cloudcov_tile numeric maximum aoi cloud cover (\%) a selected tile is allowed to have. 
#' The assumption is that a high cloud cover in scene makes it unlikely that theoretically non-cloudy pixels are free from haze
#' or shadows. Default is 80. 
#' 
#' @return \code{records} data.frame holding four additional columns:
#' \enumerate{
#' \item selected_for_timestamp: numeric indicating for which timestamp the record was selected. If 0 the record was selected for no timestamp.
#' \item rgb_mosaic: character path where the RGB preview mosaic is saved on disk
#' \item (if Landsat or MODIS) tileid: a column holding a tileid created from WRSPath and WRSRow
#' }
#' 
#' @importFrom plyr compact
#' 
#' @author Henrik Fisser
#' 
#' @export

select_timeseries <- function(records, aoi, dir_out = NULL, 
                              num_timestamps, min_distance, max_sub_period, max_cloudcov = 1, max_cloudcov_tile = 80) {
  
  if (is.null(dir_out) || class(dir_out) != "character") {out("Argument 'dir_out' has to be provided as directory of class 'character'")}
  if (class(max_cloudcov) != "numeric") {out("Argument 'max_cloudcov' has to be of class 'numeric'")}
  if (num_timestamps < 2) {
    out(paste0("Argument 'num_timestamps' is: ",num_timestamps,". The minimum number for select_timeseries is: 3"),2)
  }
  selected_col_name <- "selected_for_timeseries" # logical column if a record is selected at all
  pmos_col <- "preview_mosaic_file" # path to the RGB mosaic tif where record is included
  cmos_col <- "cmask_mosaic_file" # path to the cloud mask mosaic tif where record is included
  timestamp_col <- "selected_for_timestamp" # the timestamp number for which the record is selected
  identifier <- 1
  sensor <- unique(records$sensor)
  par <- .get_pars(records)
  .select_handle_revisit(par$sensor,par$period,num_timestamps) # communicate to the user in case max_period is in conflict with sensor revisit time
  identifier <- .select_handle_landsat(records,par$sensor)[[2]]
  records <- .select_handle_landsat(records,par$sensor)[[1]]
  records <- .select_sub_periods(records,par$period,num_timestamps) # calculates the Sub_period column
  par$tileids <- unique(records$tileid)
  par$aoi_HOT_col <- "Aoi_HOT_cloudcov_percent"
  par$tileid_col <- "tile_id"
  par$preview_col <- "preview_file"
  par$cloud_mask_col <- "Cloud_mask_path"
  selected <- list() # list to be filled by all selected 'entity_id' ids
  # select per sub-period best mosaic 
  # Select as the first record of the first sub-period the records with lowest aoi cloud cover that are still within max_period 
  first <- new.env()
  first$records <- records[records$Sub_period == 1,]
  first$period <- .identify_period(first$records[[date_col]])
  # run the selection process
  first$selected <- .select_process_sub(records=first$records,first$period,par=par,
                                        tileid_col=tileid_col,
                                        max_cloudcov_tile=max_cloudcov_tile,
                                        cloud_mask_col=par$cloud_mask_col,
                                        dir_out=dir_out,identifier=identifier)
  first$records_sel <- records[match(first$selected$ids,records$entityId),]
  first$period_new <- .identify_period(first$records_sel[[date_col]])
  selected[[1]]$timestamp <- 1
  selected[[1]] <- first$selected # insert 'selected' object into selected list
  # select next sub-period selections
  for (i in 2:length(unique(records$Sub_period))) {
    other <- new.env()
    other$records <- records[records$Sub_period==i,]
    other$first_date <- .select_force_distance(first$period_new,min_distance)
    other$period_inital <- .identify_period(other$records[[date_col]])
    # create an adjusted period (not the pre-defined sub-period) according to min_distance from previously
    # selected records
    other$period <- .select_handle_next_sub(first_date=other$first_date,
                                            period_initial=other$period_inital,
                                            min_distance,max_sub_period)
    other$records <- .within_period(records,other$period,date_col)
    # run the selection process
    other$selected <- .select_process_sub(other$records,other$period,par=par,
                                        max_cloudcov_tile=max_cloudcov_tile,
                                        cloud_mask_col=par$cloud_mask_col,
                                        dir_out=dir_out,identifier=identifier)
    selected[[i]]$timestamp <- i
    selected[[i]] <- other$selected # insert 'selected' object into selected list
  }
  #A Create and save final cloud mask mosaic
  #B Create and save final RGB preview mosaic
  #C Add 3 columns to records data.frame:
  #1 logical column if a record is selected at all
  #2 path to the RGB mosaic tif where record is included
  #3 the timestamp number for which the record is selected
  console_info <- c()
  for (i in 1:length(selected)) {
    s <- selected[[i]]
    id_sel <- sapply(s$ids,function(x) {which(records[,identifier]==x)})
    #A cloud mask mosaic
    save_path_cmos <- file.path(dir_out,paste0("cloud_mask_mosaic_timestamp",s$timestamp,".tif"))
    cMask_mosaic <- .select_bridge_mosaic(s$cMask_paths,aoi,dir_out)
    #B RGB preview mosaic
    save_path_pmos <- file.path(dir_out,paste0("preview_mosaic_timestamp",s$timestamp,".tif"))
    # mask preview tiles by cloud masks
    preview_paths <- sapply(id_sel,function(i) {
      preview <- raster(records[i,par$preview_col])
      cMask <- raster(records[i,par$])
      preview_masked <- .mask_raster(preview,cMask)
    })
    preview_mosaic <- .select_bridge_mosaic(preview_paths,aoi,dir_out,mode="rgb")
    #C add 3 columns to records
    records[id_sel,selected_col_name] <- s$ids
    records[id_sel,timestamp_col] <- s$timestamp
    records[id_sel,pmos_col] <- save_path_pmos
    records[id_sel,cmos_col] <- save_path_cmos
    # get print info
    console_info[i] <- .select_final_info(s)
  }
  each_timestamp <- .out_vector(console_info)
  csw <- .select_summary_ts(selected)
  w <- csw[2:3] # warnings
  w <- compact(w)
  summary <- .out_vector(csw[[1]])
  warn_out <- sapply(w,function(x) {
    warning <- .out_vector(x,type=2)
  })
  
  return(records)
}
