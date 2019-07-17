#' Selects records for a time series according to aoi cloud cover and temporal characteristics
#' 
#' @details For running the selection you have to process \link{calc_cloudcov} first.
#'  
#' \code{select_timeseries} is selection mode "TS" from \link{select_records}
#' 
#' @param records data.frame as returned by \link{calc_cloudcov}, either complete or subsetted. Records will be selected from these records.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) 
#' polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param dir_out character directory where to save the cloud mask mosaics and the RGB preview mosaic.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps. 
#' For example, if a scene from 20th May 2019 is selected for a timestamp and \code{min_distance == 10} then the next timestamp will not include scenes 
#' in <= 10 days after 20th May 2019. The first scene the next timestamp could include would be the 31st May 2019 thus.
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' The value is the percentage of not yet covered area that shall be covered additionally when adding the record. This protects. Default is 8.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed. This determines how close together
#' the selected records for one timestamp are temporally (if mosaicking is needed).
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
                              num_timestamps, min_distance, min_improvement = 8, max_sub_period, max_cloudcov_tile = 80) {
  
  if (is.null(dir_out) || class(dir_out) != "character") {out("Argument 'dir_out' has to be provided as directory of class 'character'")}
  if (num_timestamps < 2) {
    out(paste0("Argument 'num_timestamps' is: ",num_timestamps,". The minimum number for select_timeseries is: 3"),2)
  }
  #### Parameters
  sep <- "\n----------------------------------------------------------------"
  selected_col_name <- "selected_for_timeseries" # logical column if a record is selected at all
  pmos_col <- "preview_mosaic_file" # path to the RGB mosaic tif where record is included
  cmos_col <- "cmask_mosaic_file" # path to the cloud mask mosaic tif where record is included
  timestamp_col <- "selected_for_timestamp" # the timestamp number for which the record is selected
  identifier <- 1
  sensor <- unique(records$sensor)
  par <- list()
  par$date_col <- "acquisitionDate"
  par$aoi_cc_col <- "aoi_HOT_cloudcov_percent"
  par$tileid_col <- "tile_id"
  par$preview_col <- "preview_file"
  par$cloud_mask_col <- "cloud_mask_file"
  par$id_col <- "entityId"
  par$aoi_cc_prb_col <- "aoi_HOT_mean_probability"
  par$cc_index_col <- "cc_index"
  par$period <- .identify_period(records[[par$date_col]])
  par$sensor <- records$sensor[1]
  identifier <- .select_handle_landsat(records,par$sensor)[[2]]
  records <- .select_handle_landsat(records,par$sensor)[[1]]
  records <- .select_sub_periods(records,par$period,num_timestamps) # calculates the sub_period column
  # calculate the synthesis of absolute aoi cloud cover and mean aoi cloud cover probability
  records <- .select_cc_index(records,aoi_cc_col=par$aoi_cc_col,aoi_cc_prb_col=par$aoi_cc_prb_col,
                              cc_index_col=par$cc_index_col,ratio=0.8) 
  par$tileids <- unique(records[[par$tileid_col]])
  
  #### Start Process
  out(paste0(sep,"\n-- Starting Selection Process -- ",sep))
  .select_handle_revisit(par$sensor,par$period,num_timestamps) # communicate to the user in case max_period is in conflict with sensor revisit time
  selected <- list() # list to be filled by all selected 'entity_id' ids, the valid coverage percentage per timestamp and the cloud mask paths
  period_new <- c()
  sub_periods <- unique(records$sub_period)
  # select per sub-period best mosaic
  for (t in 1:length(sub_periods[!is.na(sub_periods)])) {
    selected_ts <- .select_main(records,period_new=period_new,min_distance=min_distance, 
                             max_sub_period=max_sub_period,max_cloudcov_tile=max_cloudcov_tile, 
                             min_improvement=min_improvement,par=par,dir_out=dir_out,identifier=identifier,timestamp=t)
    records_sel <- records[match(selected_ts$ids,records[[par$id_col]]),]
    period_new <- .identify_period(records_sel[[par$date_col]])
    selected_ts$timestamp <- t
    selected[[t]] <- selected_ts # insert 'selected' list into selected list
  }
  
  #### Create resulting mosaics and add columns to records
  #A Create and save final cloud mask mosaic
  #B Create and save final RGB preview mosaic
  #C Add 3 columns to records data.frame:
  #1 logical column if a record is selected at all
  #2 path to the RGB mosaic tif where record is included
  #3 the timestamp number for which the record is selected
  out(paste0(sep,"\nSelection Process Summary per timestamp",sep))
  # create final mosaics for each timestamp and summary message per timestamp
  records <- .select_save_mosaics(records,selected=selected,aoi=aoi,selected_col_name=selected_col_name, 
                                  pmos_col=pmos_col,cmos_col=cmos_col,identifier=identifier,dir_out=dir_out)
  # create optional warning(s) and overall summary message
  csw <- .select_summary_ts(selected)
  w <- csw[2:3] # warnings
  w <- w[which(w!="NULL")]
  summary <- .out_vector(csw[[1]])
  if (length(w) > 0) {
    to_cons <- sapply(w,function(x) .out_vector(x,type=2))
  }

  records <- subset(records,select=-sub_period) # remove sub-period column
  return(records)
}




