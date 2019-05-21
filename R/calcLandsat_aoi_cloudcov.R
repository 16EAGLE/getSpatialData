#' Calculate the cloud cover of Landsat data within an aoi
#' 
#' \code{calcLandsat_aoi_cloudcov} estimates the cloud cover of Landsat data based on preview images using the haze-optimal transformation (HOT)
#' 
#' @details Cloud cover of optical remote sensing data is normally provided scene-wide by the data dissiminator. This function uses HOT in order to derive a local cloud cover \% within an aoi. The estimation of the cloud cover is done on the red and blue information of the preview images provided by the respective data dissiminator.
#' Haze-optimal transformation (HOT) procedure is applied based on 
#' Zhu & Helmer (2018), https://data.fs.usda.gov/research/pubs/iitf/ja_iitf_2018_Zhu.pdf. Orignally, the algorithm was introduced by Zhang et al. (2002)
#' "An image transform to characterize and compensate for spatial variations in thin cloud contamination of Landsat images", Remote Sensing of Environment 82, 2-3.
#' HOT seperates clear-sky pixels first from a threshold, calculates a least alternate deviation (LAD) regression from these pixels and exposes cloud pixels by the deviation of all pixels from this clear-sky line.
#' 
#' @inheritParams calcSentinel_aoi_cloudcov
#' 
#' @return A data.frame as the records input with one additional column holding the estimated cloud cover within the aoi.
#'
#' @author Henrik Fisser
#' 
#' @examples
#'
#' ## Load packages
#' library(getSpatialData)
#' library(sf)
#'
#' ## set aoi and time range for the query
#' set_aoi(aoi_data[[1]])
#' time_range <-  c("2017-08-01", "2017-08-30")
#'
#' ## Login to USGS ERS
#' \dontrun{
#' login_USGS("username")
#'
#' ## set archive directory
#' set_archive("/path/to/archive/")
#'
#' ## get available products and select one
#' product_names <- getLandsat_names()
#'
#' ## query for records for your AOI, time range and product
#' records <- getLandsat_query(time_range = time_range, name = product_names[7])
#' 
#' ## Calculate cloud cover within the aoi
#' records_cloudcov <- calcLandsat_aoi_cloudcov(records = records, aoi = aoi) #cloud cov. calc.
#' 
#' ## preview a record
#' getLandsat_preview(records_cloudcov[5,])
#'
#' #print available levels for a record
#' query[5,]$levels_available
#'
#' ## download record 5 with level "l1" (will direct to AWS automaticaly)
#' files <- getLandsat_data(records = query[5,], level = "l1", source = "auto")
#'
#' ## download record 5 with level "sr" (will be processed on demand by ESPA)
#' files <- getLandsat_data(records = query[5,], level = "sr", source = "auto")
#' # this can take very long, since the function will wait,
#' # until the processing by ESPA is done
#'
#' ## you can abort the function while it is waiting for ESPA and resume later:
#' files <- getLandsat_data(espa_order = "espa-XYZA@host.com-YOUR-ORDER-ID")
#' # the order IDs are displayed and send by mail, use them to resume the task
#' }
#' 
#' @seealso \link{calc_hot_cloudcov}, \link{getLandsat_query}, \link{getLandsat_preview}, \link{getLandsat_data}
#'  
#' @export

calcLandsat_aoi_cloudcov <- function(records, aoi = NULL,  maxDeviation = 20, cloudPrbThreshold = 40, 
                                     slopeDefault = 1.4, interceptDefault = -10, dir_out = NULL, 
                                     username = NULL, password = NULL, verbose = TRUE) {
  
  sceneCloudCoverCol <- "SceneCloudCover" # for later use the column name holding the scene cloud cover
  sensor <- "Landsat"
  
  records <- .hotBridge(sensor=sensor,sceneCloudCoverCol=sceneCloudCoverCol,records=records,aoi=aoi,maxDeviation=maxDeviation,
                        cloudPrbThreshold=cloudPrbThreshold,slopeDefault=slopeDefault,
                        interceptDefault=interceptDefault,dir_out=dir_out,username=username,password=password,verbose=verbose)
  
  return(records)
}