#' Selects remote sensing records for a time series
#' 
#' @description Selection is done according to aoi cloud cover (in case of optical data) 
#' and temporal characteristics. Both optical and SAR records are supported as well as
#' combined selection for different products across systems and data providers.
#'  
#' @details For running the selection you have to process \link{calc_cloudcov} first.
#' Generally, the following products can be processed in \code{select_*} functionalities:
#' \itemize{
#' \item Sentinel-1
#' \item Sentinel-2 A/B
#' \item Sentinel-3 OLCI
#' \item Landsat 5-8
#' \item MODIS
#' }
#' For the precise supported product names call \link{get_select_supported}.
#' When aiming at mixing two or more optical products you may order them by priority
#' through \code{prio_products}.
#' Coupled selection of optical and SAR sensors is possible. Optical records will always
#' be selected first, SAR records second, in temporal accordance with the selected
#' optical records.
#' 
#' @note This functionality creates a 'tmp' folder below \code{dir_out} where
#' temporary files are saved. This folder will be deleted at the end of the function call.
#' 
#' @param records data.frame as returned by \link{calc_cloudcov}, either complete or subsetted but with all columns. 
#' Records will be selected from these records. If \code{prio_products} is provided only these products will be handled.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps. 
#' For example, if a scene from 20th May 2019 is selected for a timestamp and \code{min_distance == 10} 
#' then the next timestamp will not include scenes in <= 10 days after 20th May 2019. 
#' The first scene that the next timestamp could include would be the 31st May 2019, thus.
#' @param max_sub_period numeric maximum number of days to be used for creating a mosaic per timestamp if mosaicking is needed. 
#' This determines how temporally close together the selected records for one timestamp are (if mosaicking is needed).
#' @param min_improvement numeric the minimum increase of valid pixels percentage in a tile when adding a record.
#' This protects from adding large amounts of records that improve coverage by only a few pixels. Default is 100.
#' @param max_cloudcov_tile numeric maximum aoi cloud cover (\%) a selected tile is allowed to have. 
#' The assumption is that a high cloud cover in a scene makes it unlikely that theoretically non-cloudy pixels are free from haze
#' or shadows. Default is 80.
#' @param satisfaction_value numeric percentage value at which mosaic is considered as cloud-free. Default is 98.
#' @param prio_products character vector optional. Product names ordered by priority. Selection is done in the order
#' of prio_products starting with the first product. Following products are included consecutively in case
#' selection was not fullfilled by previous product. Product names must be provided as returned by \link{get_select_supported}.
#' Landsat and MODIS can be summarized by 'Landsat' respectively 'MODIS' if no further differentiation demanded.
#' If prio_products is empty, given products in \code{records} will be selected in random order in case several are given in \code{records}.
#' @inheritParams calc_cloudcov
#'
#' @return \code{records} data.frame holding four additional columns:
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

select_timeseries <- function(records,
                              num_timestamps, min_distance, max_sub_period,
                              min_improvement = 5, max_cloudcov_tile = 80, satisfaction_value = 98,
                              prio_products = c(), 
                              aoi = NULL, dir_out = NULL, as_sf = TRUE, verbose = TRUE) {
  
  #### Pre-checks
  records <- .check_records(records, .get_needed_cols_select(), as_df = TRUE)
  aoi <- .check_aoi(aoi, SF())
  cols_initial <- colnames(records)
  
  .check_numeric(num_timestamps, "num_timestamps")
  if (num_timestamps < 3) {
    out(paste0("Argument 'num_timestamps' is: ",num_timestamps,". 
The minimum number for select_timeseries is: 3"),3)
  }
  
  #### Prep
  prep <- .select_prep_wrap(records,num_timestamps,"TS")
  records <- prep$records
  params <- prep$params

  #### Main checks
  .select_checks(records,aoi,params$period,num_timestamps,prio_products,params,dir_out,verbose)
  
  #### Main Process
  .select_start_info(mode="Time Series",params$sep)
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




