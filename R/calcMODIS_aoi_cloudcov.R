#' Calculate the cloud cover of MODIS data within an aoi
#' 
#' \code{calcMODIS_aoi_cloudcov} estimates the cloud cover of Landsat data based on preview images using the haze-optimal transformation (HOT)
#' 
#' @details Cloud cover of optical remote sensing data is normally provided scene-wide by the data dissiminator. This function uses HOT in order to derive a local cloud cover \% within an aoi. The estimation of the cloud cover is done on the red and blue information of the preview images provided by the respective data dissiminator.
#' Haze-optimal transformation (HOT) procedure is applied based on 
#' Zhu & Helmer (2018), https://data.fs.usda.gov/research/pubs/iitf/ja_iitf_2018_Zhu.pdf. Orignally, the algorithm was introduced by Zhang et al. (2002)
#' "An image transform to characterize and compensate for spatial variations in thin cloud contamination of Landsat images", Remote Sensing of Environment 82, 2-3.
#' HOT seperates clear-sky pixels first from a threshold, calculates a least alternate deviation (LAD) regression from these pixels and exposes cloud pixels by the deviation of all pixels from this clear-sky line.
#'
#' @inheritParams calcSentinel_aoi_cloudcov
#' 
#' @return A data.frame as the records input with one additional column holding the estimated cloud cover within the aoi
#'
#' @author Henrik Fisser
#' 
#' @examples
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
#' product_names <- getMODIS_names()
#' product <- grep("MOD13Q1", product_names, value = T)
#'
#' ## query for records for your AOI, time range and product
#' records <- getMODIS_query(time_range = time_range, name = product)
#' 
#' ## Calculate cloud cover within the aoi
#' records_cloudcov <- calcMODIS_aoi_cloudcov(records = records, aoi = aoi) # cloud cov. calc.
#'
#' ## preview a record
#' getMODIS_preview(records_cloudcov[1,])
#'
#' ## download records 1 and 2
#' files <- getMODIS_data(records_cloudcov[1:2,])
#' }
#' 
#' @seealso \link{calc_hot_cloudcov}, \link{getMODIS_query}, \link{getMODIS_preview}, \link{getMODIS_data}
#'  
#' @export

calcMODIS_aoi_cloudcov <- function(records, aoi = NULL,  maxDeviation = 20, cloudPrbThreshold = 40, 
                                     slopeDefault = 1.4, interceptDefault = -10, dir_out = NULL, 
                                     username = NULL, password = NULL, verbose = TRUE) {
  
  sceneCloudCoverCol <- "SceneCloudCover" # for later use the column name holding the scene cloud cover
  sensor <- "MODIS"
  
  records <- .cloudcov_bridge(sensor=sensor,sceneCloudCoverCol=sceneCloudCoverCol,records=records,aoi=aoi,maxDeviation=maxDeviation,
                        cloudPrbThreshold=cloudPrbThreshold,slopeDefault=slopeDefault,
                        interceptDefault=interceptDefault,dir_out=dir_out,username=username,password=password,verbose=verbose)
  
  return(records)
}