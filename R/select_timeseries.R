#' Selects records for a time series according to aoi cloud cover and temporal characteristics
#' 
#' @details For running the selection you have to process \link{calc_cloudcov} first.
#'  
#' \code{select_timeseries} is selection mode "TS" from \link{select_records}
#' 
#' @param records data.frame as returned by \link{calc_cloudcov}, either complete or subsetted but with all columns. 
#' Records will be selected from these records.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) 
#' polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) 
#' and at least three rows (each row representing one corner coordinate). 
#' If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. 
#' Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. 
#' If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps. 
#' For example, if a scene from 20th May 2019 is selected for a timestamp and \code{min_distance == 10} 
#' then the next timestamp will not include scenes in <= 10 days after 20th May 2019. 
#' The first scene the next timestamp could include would be the 31st May 2019 thus.
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' The value is the percentage of not yet covered area that shall be covered additionally if adding the record. This protects from
#' adding masses of records that improve coverage by only a few pixels. Default is 5.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed. 
#' This determines how temporally close together the selected records for one timestamp are (if mosaicking is needed).
#' @param max_cloudcov_tile numeric maximum aoi cloud cover (\%) a selected tile is allowed to have. 
#' The assumption is that a high cloud cover in scene makes it unlikely that theoretically non-cloudy pixels are free from haze
#' or shadows. Default is 80.
#' @param prio_sensor
#' @param dir_out character directory where to save the cloud mask mosaics and the RGB preview mosaic.
#' Note: Below this dir_out a tmp_dir will be created where temporary files will be saved during selection.This folder is
#' deleted before returning \code{records}.
#' @param verbose	logical, whether to display details on the function's progress or output on the console.
#'
#' @return \code{records} data.frame holding maximum four additional columns:
#' \enumerate{
#' \item selected_for_timeseries logical column indicating for each record if it was selected (==TRUE).
#' \item selected_for_timestamp: numeric indicating for which timestamp the record was selected. If NA the record was selected for no timestamp.
#' \item rgb_mosaic_file: character path where the RGB preview mosaic is saved on disk. (not added if only SAR in \code{records})
#' \item cmask_mosaic_file: character path where the cloud mask mosaic is saved on disk. (not added if only SAR in \code{records})
#' }
#' 
#' @author Henrik Fisser
#' 
#' @export

select_timeseries <- function(records, aoi,
                              num_timestamps, min_distance, min_improvement = 5, 
                              max_sub_period, max_cloudcov_tile = 80, prio_sensors = NULL, 
                              dir_out = NULL, verbose = TRUE) {
  
  if (is.null(dir_out) || class(dir_out) != "character") {out("Argument 'dir_out' has to be provided as directory of class 'character'")}
  if (num_timestamps < 2) {
    out(paste0("This time it's ok, Henrik. Your number of timestamps is only: ",num_timestamps,"\n;-)"))
    #out(paste0("Argument 'num_timestamps' is: ",num_timestamps,". The minimum number for select_timeseries is: 3"),3)
  }
  if (!is.null(prio_sensors)) .select_check_prio_sensors(prio_sensors)
  check <- sapply(list(preview_file=records$preview_file,cloud_mask_file=records$cloud_mask_file),function(x) {
    .select_catch_files(x,names(x))
  })
  if (any(!is.na(check))) out("Cannot find files on disk",3)
  
  #### Parameters
  par <- .select_params(mode="TS",records)
  identifier <- ifelse(par$sensor_group == "Landsat" || par$sensor_group == "MODIS",15,1)
  records <- .select_prep(records,num_timestamps,par,identifier) 
  par$period <- .identify_period(records[[par$date_col]])
  has_SAR <- .has_SAR(par$sensor) # check if SAR records in records (1 for TRUE, 0 for FALSE or 100 for "all"). If 100 selection is done only for SAR
  if (has_SAR %in% c(0,1)) { # if has some or none SAR records, so optical is given as well
    # calculate the synthesis of absolute aoi cloud cover and mean aoi cloud cover probability
    records <- .select_cc_index(records,aoi_cc_col=par$aoi_cc_col,aoi_cc_prb_col=par$aoi_cc_prb_col,
                                cc_index_col=par$cc_index_col,ratio=0.8)
  }
  
  # if all are SAR records
  if (has_SAR == 100) { 
    selected_SAR <- .select_SAR(records,period_new = NULL,max_sub_period,min_distance,
                                num_timestamps,par,identifier)
    records <- .select_finish_SAR(records, selected_SAR, par)
    return(records)
  }
  
  #### Start Process for optical selection
  out(paste0(par$sep,"\n           Starting Time Series Selection Process           ",par$sep))
  .select_handle_revisit(par$sensor,par$period,num_timestamps) # communicate to the user in case max_period is in conflict with sensor revisit time
  selected <- list() # list to be filled by all selected 'entity_id' ids, the valid coverage percentage per timestamp and the cloud mask paths
  period_new <- c()
  sub_periods <- unique(records$sub_period)
  # select per sub-period (=timestamp) best mosaic. The sub-periods are adjusted dynamically according to min_distance, max_sub_period
  for (t in 1:length(sub_periods[!is.na(sub_periods)])) {
    selected_ts <- .select_main(records,min_distance=min_distance, 
                                max_sub_period=max_sub_period,max_cloudcov_tile=max_cloudcov_tile, 
                                min_improvement=min_improvement,
                                par=par,dir_out=dir_out,identifier=identifier,timestamp=t)
    selected_ts$timestamp <- t
    selected[[t]] <- selected_ts # insert 'selected_ts' list into selected list
  }
  
  # if some are SAR records
  if (has_SAR == 1) { 
    # SAR records shall be searched within max_sub_period combined
    # with the periods selected for optical 
    period_new_all <- lapply(selected,function(x) return(x[["period"]]))
    selected_SAR <- .select_SAR(records,period_new_all,max_sub_period,min_distance,
                                num_timestamps,par,identifier)
    # add selected ids to selected list of optical records
    for (ts in 1:length(selected)) {
      optical_ids <- selected[[ts]][["ids"]]
      selected[[ts]][["ids"]] <- append(optical_ids,x[["ids"]])
    }
  }
  
  #### Create resulting mosaics and add columns to records
  #A Create and save final cloud mask mosaic
  #B Create and save final RGB preview mosaic
  #C Add 3 columns to records data.frame:
  #1 logical column if a record is selected at all
  #2 path to the RGB mosaic tif where record is included
  #3 the timestamp number for which the record is selected
  out(paste0(par$sep,"\nSelection Process Summary per timestamp",par$sep))
  # create final mosaics for each timestamp and summary message per timestamp
  records <- .select_save_mosaics(records,selected=selected,aoi=aoi,selected_col=par$selected_col, 
                                  pmos_col=par$pmos_col,cmos_col=par$cmos_col,identifier=identifier,dir_out=dir_out)
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




