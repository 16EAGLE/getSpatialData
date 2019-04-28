#' Calculates the haze-optimal transformation cloud cover based on the red and blue band
#' 
#' \code{calc_hot_cloudcov} estimates the cloud cover of a satellite image raster using the haze-optimal transformation (HOT) within an aoi
#' 
#' @details The estimation of the cloud cover is done on the red and blue information of the preview images provided by the respective data distributor. Haze-optimal transformation (HOT) procedure is applied based on 
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
#' @importFrom raster
#' 
#' @export

calc_hot_cloudcov <- function(records, aoi = NULL, save_masks = TRUE, dir_out = NULL, verbose = TRUE) {
  
  error <- "try-error"
  
  
}