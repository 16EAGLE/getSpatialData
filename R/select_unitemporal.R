#' Selects remote sensing records uni-temporally
#' 
#' @description Selection is done according to aoi cloud cover (in case of optical data) 
#' and temporal characteristics. Both optical and SAR records are supported as well as
#' combined selection for different products across systems and data providers. 
#' 
#' @inherit select_timeseries details
#' 
#' @note This functionality creates a 'tmp' folder below \code{dir_out} where
#' temporary files are saved. This folder will be deleted at the end of the function call.
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
                               min_improvement = 5, max_cloudcov_tile = 80, satisfaction_value = 98,
                               prio_products = c(),
                               aoi = NULL, 
                               write_cmask_mosaic = TRUE, write_preview_mosaic = TRUE,
                               dir_out = NULL, as_sf = TRUE, verbose = TRUE) {
  #### Pre-checks
  records <- .select_check_records(records)
  .check_numeric(max_sub_period, "max_sub_period")
  .check_numeric(min_improvement, "min_improvement")
  .check_numeric(max_cloudcov_tile, "max_cloudcov_tile")
  .check_numeric(satisfaction_value, "satisfaction_value")
  .check_as_sf(as_sf)
  .check_verbose(verbose)
  aoi <- .check_aoi(aoi, SF())
  cols_initial <- colnames(records)
  
  #### Prep
  num_timestamps <- 1
  prep <- .select_prep_wrap(records, num_timestamps, "UT")
  records <- prep$records
  params <- prep$params
  
  #### Main checks
  .select_checks(records, aoi, params$period, num_timestamps, prio_products, params, write_preview_mosaic, dir_out, verbose)
  
  #### Main Process
  .select_start_info(mode="Unitemporal", params$sep)
  records <- .select_main(records,
                          aoi,
                          prep$has_SAR,
                          num_timestamps,
                          min_distance=NULL,
                          min_improvement,
                          max_sub_period,
                          max_cloudcov_tile,
                          satisfaction_value,
                          prio_products,
                          write_cmask_mosaic,
                          write_preview_mosaic,
                          dir_out,
                          params,
                          cols_initial)
  
  records <- .check_records(records, as_sf = as_sf)
  return(records)
  
}
