#' Selects remote sensing records for a time series
#' 
#' @description Selection is done according to aoi cloud cover (in case of optical data) 
#' and temporal characteristics. Both optical and SAR records are supported as well as
#' combined selection for different sensors across systems and data providers.
#'  
#' @details For running the selection you have to process \link{calc_cloudcov} first.
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
#' @param prio_sensors character vector optioal. Sensor names ordered by priority. Selection is done in the order
#' of prio_sensors starting with the first sensor. Following sensors are included consecutively in case
#' selection was not fullfilled by previous sensor.
#' @param dir_out character directory where to save the cloud mask mosaics and the RGB preview mosaic.
#' Note: Below this dir_out a tmp_dir will be created where temporary files will be saved during selection.This folder is
#' deleted before returning \code{records}.
#' @param verbose	logical, whether to display details on the function's progress or output on the console.
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

select_timeseries <- function(records, aoi,
                              num_timestamps, min_distance, min_improvement = 5, 
                              max_sub_period, max_cloudcov_tile = 80, 
                              prio_sensors = NULL, 
                              dir_out = NULL, verbose = TRUE) {
  
  .check_records(records,.cloudcov_colnames())
  cols_initial <- colnames(records)
  if (!is.numeric(num_timestamps)) out("Argument 'num_timestamps' has to be of class numeric")
  if (num_timestamps < 3) {
    out(paste0("Argument 'num_timestamps' is: ",num_timestamps,". 
               The minimum number for select_timeseries is: 3"),3)
  }
  
  #### Prep
  prep <- .select_prep_wrap(records,num_timestamps,"TS")
  records <- prep$records
  par <- prep$par
  has_SAR <- prep$has_SAR
  
  #### Checks
  .select_checks(records,aoi,prio_sensors,par,dir_out,verbose)
  
  #### Main Process
  .select_start_info(mode="time series",par$sep)
  records <- .select_main(records,
                          aoi,
                          has_SAR,
                          num_timestamps,
                          min_distance,
                          min_improvement,
                          max_sub_period,
                          max_cloudcov_tile,
                          prio_sensors,
                          dir_out,
                          par,
                          cols_initial)
  
  return(records)
  
}




