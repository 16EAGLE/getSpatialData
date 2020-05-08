#' Selects remote sensing records bi-temporally
#' 
#' @description Selection is done according to aoi cloud cover (in case of optical data) 
#' and temporal characteristics. Both optical and SAR records are supported as well as
#' combined selection for different products across systems and data providers.
#'  
#' @inherit select_timeseries note details
#' 
#' @inheritParams select_timeseries
# 
#' @return \code{records} data.frame holding four additional columns:
#' \enumerate{
#' \item selected_for_bitemporal logical column indicating for each record if it was selected (==TRUE).
#' \item selected_for_timestamp: numeric indicating for which timestamp the record was selected. If NA the record was selected for no timestamp.
#' \item rgb_mosaic_file: character path where the RGB preview mosaic is saved on disk.
#' \item cmask_mosaic_file: character path where the cloud mask mosaic is saved on disk.
#' }
#' 
#' @author Henrik Fisser
#' 
#' @export
select_bitemporal <- function(records,
                              min_distance, max_sub_period,
                              min_improvement = 5, max_cloudcov_tile = 80, satisfaction_value = 98,
                              prio_products = c(),
                              aoi = NULL, dir_out = NULL, as_sf = TRUE, verbose = TRUE) {
  
  #### Pre-checks
  records <- .check_records(records, .get_needed_cols_select(), as_df = TRUE)
  aoi <- .check_aoi(aoi, SF())
  cols_initial <- colnames(records)
  
  #### Prep
  num_timestamps <- 2
  prep <- .select_prep_wrap(records,num_timestamps,"BT")
  records <- prep$records
  params <- prep$params

  #### Main checks
  .select_checks(records,aoi,prio_products,params,dir_out,verbose)
  
  #### Main Process
  .select_start_info(mode="Bi-Temporal",params$sep)
  records <- .select_main(records,
                          aoi,
                          prep$has_SAR,
                          num_timestamps,
                          min_distance,
                          min_improvement,
                          max_sub_period,
                          max_cloudcov_tile,
                          satisfaction_value,
                          prio_products,
                          dir_out,
                          params,
                          cols_initial)

  records <- .check_records(records, as_df = !as_sf)
  return(records)

}



