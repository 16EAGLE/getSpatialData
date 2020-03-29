#' Selects remote sensing records uni-temporally
#' 
#' @description Selection is done according to aoi cloud cover (in case of optical data) 
#' and temporal characteristics. Both optical and SAR records are supported as well as
#' combined selection for different sensors across systems and data providers. 
#' 
#' @details For running the selection you have to process \link{calc_cloudcov} first.
#' 
#' @note This functionality creates a 'tmp' folder below \code{dir_out} where
#' temporary files are saved. This folder is being deleted at the end of the function call.
#' 
#' @inheritParams select_timeseries
#' 
#' @return \code{records} data.frame holding three additional columns:
#' \enumerate{
#' \item selected_for_unitemporal logical column indicating for each record if it was selected (==TRUE).
#' \item rgb_mosaic_file: character path where the RGB preview mosaic is saved on disk.
#' \item cmask_mosaic_file: character path where the cloud mask mosaic is saved on disk.
#' }
#' 
#' @author Henrik Fisser
#' 
#' @export
select_unitemporal <- function(records,
                               max_sub_period,
                               min_improvement = 5, 
                               max_cloudcov_tile = 80, satisfaction_value = 98,
                               prio_sensors = c(),
                               aoi = NULL, dir_out = NULL, verbose = TRUE) {
  #### Pre-checks
  records <- .check_records(records, .get_needed_cols_select(), as_df=T)
  aoi <- .check_aoi(aoi,"sp")
  cols_initial <- colnames(records)
  
  #### Prep
  num_timestamps <- 1
  prep <- .select_prep_wrap(records,num_timestamps,"UT")
  records <- prep$records
  params <- prep$params

  #### Main checks
  .select_checks(records,aoi,params$period,num_timestamps,prio_sensors,params,dir_out,verbose)
  
  #### Main Process
  .select_start_info(mode="Uni-Temporal",params$sep)
  records <- .select_main(records,
                          aoi,
                          prep$has_SAR,
                          num_timestamps,
                          min_distance=NULL,
                          min_improvement,
                          max_sub_period,
                          max_cloudcov_tile,
                          satisfaction_value,
                          prio_sensors,
                          dir_out,
                          params,
                          cols_initial)

  return(records)
  
}
