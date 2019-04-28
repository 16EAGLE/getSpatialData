#' Calculates the haze-optimal transformation cloud cover based on the red and blue band
#' 
#' \code{calc_hot_cloudcov} estimates the cloud cover of a satellite image raster using the haze-optimal transformation (HOT) within an aoi
#' The algorithm was implemented in this function for cloud cover estimation of DN values from preview images.
#' 
#' @details The estimation of the cloud cover is done on the red and blue information of the input RGB. Haze-optimal transformation (HOT) procedure is applied based on 
#' Zhu & Helmer (2018), https://data.fs.usda.gov/research/pubs/iitf/ja_iitf_2018_Zhu.pdf. Orignally, the algorithm was introduced by Zhang et al. (2002)
#' "An image transform to characterize and compensate for spatial variations in thin cloud contamination of Landsat images", Remote Sensing of Environment 82, 2-3.
#' HOT seperates clear-sky pixels first from a threshold, calculates a least alternate deviation (LAD) regression from these pixels and exposes cloud pixels by the deviation of all pixels from this clear-sky line.
#'
#' @param record data.frame, one line holding a single record from a records data.frame.
#' @param preview raster, the preview image (RGB) of \code{record}. Should be: layer 1 = red, layer 2 = green, layer 3 = blue. If only two layers are provided, it is assumed: layer 1 = red, layer 2 = blue.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param save_masks lgoical, if \code{TRUE}, the cloud masks are saved in \code{dir_out} directory. Default is TRUE.
#' @param dir_out character, full path to target directory where to save the cloud masks. Optional, only used if \code{save_masks = TRUE}. If not set, \code{getLandsat_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData  archive folder.
#' @param num_records numeric, indicating how many records are going to be processed when calling the function iteratively. It is used for processing time estimation. Default is 1.
#' @param slopeDefault numeric, value taken as slope ONLY if least-alternate deviation regression fails.  Default is 1.4, proven to work well for common land surfaces. Enter 0 if you would like to disable the use of default values. In this case a warning message will occur in case of failing regression.
#' @param interceptDefault, value taken as intercept ONLY if least-alternate deviation regression fails. Default is -10, proven to work well for common land surfaces.
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#'    
#' @return A data.frame, one line as the input with one additional column holding the estimated cloud cover within the aoi
#' 
#' @author Henrik Fisser
#' 
#' @importFrom st_crs sf st_as_sf st_transform
#' @importFrom raster projectRaster nlayers stack values na.omit
#' 
#' @export

calc_hot_cloudcov <- function(record, preview, aoi = NULL, save_masks = TRUE, dir_out = NULL, num_records = 1, slopeDefault = 1.4, interceptDefault = -10, verbose = TRUE) {
  
  AOIcloudcoverpercentage <- "AOIcloudcoverpercentage" # for aoi cloud cover column
  error <- "try-error"
  crsError <- " Desired coordinate system:\n"
  crs <- st_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # ensure correct projections
  if(class(aoi)[[1]] != "sf") {aoi <- st_as_sf(aoi)}
  if (st_crs(aoi) != crs) {
    try(aoi <- st_transform(aoi,crs))
    if (inherits(aoi,error)) {
      out("Reprojection of aoi failed.",crsError,crs,type=3) 
    }
  }
  if (raster::projection(preview) != crs) {
    try(preview <- raster::projectRaster(preview,crs=crs))
    if (inherits(preview,error)) {
      out("Reprojection of preview image failed.",crsError,crs,type=3)
    }
  }
  
  ##### HOT
  ## Prepare RGB or RB stack
  nlyrs <- nlayers(preview)
  if (nlyrs == 3) {
    bBand <- preview[[3]]
    rBand <- preview[[1]]
  } else if (nlayers) {
    bBand <- preview[[2]]
    rBand <- preview[[1]]
  }
  prvStck <- stack(bBand,rBand)
  ## Calculate least-alternate deviation (LAD) regression
  # this step computes first safe clear-sky pixels adpated from the bins method from Zhu & Helmer 2018
  # the procedure was adapted slightly because here it is being computed with discontinuous DN values. Suitable values for
  # r and b were investigated systemetically. For HOT slope values of more than 1.5 were investigated to be most
  # suitable. The given r and b value are thus values that lead to slope and intercept that have the highest capability
  # to safely discriminate clouds from non-cloud when calculating HOT. Low slope values of < 1 may lead for example to 
  # a confusion of bright or reddish land surfaces with clouds
  bThresh <- 80 # for dividing the 80 lowest blue DN values into equal interval bins
  rThresh <- 40 # from the 80 blue bins take the 40 highest red values
  valDf <- data.frame(na.omit(values(prvStck)))
  valDf <- valDf[intersect(which(valDf[,1] != 0),which(valDf[,2] != 0)),] # exclude 0 values besides NA values because large amounts besides the scene pixels can spoil the regression
  bBins <- lapply(2:bThresh,function(x){which(df[[2]]==x)}) # these are the bins of interest for blue DNs
  bBins <- lapply(bBins,function(x){data.frame(red=df[x,1],blue=df[x,2])}) # get the red and blue DNs where bins are valid
  redMax <- lapply(1:length(bBins),function(x){bBins[[x]][order(bBins[[x]][["red"]]),]}) # order the data.frames by red
  redMax <- lapply(redMax,function(x){
    if (NROW(x)>=rThresh) {
      x[NROW(x):(NROW(x)-rThresh),] # in case less than rThresh values are available take the maximum number of available values
    } else {
      r <- NROW(x)
      x[NROW(x):(NROW(x)-r),]
    }
  })
  meanRed <- sapply(redMax,function(x){mean(x[["red"]])})
  meanBlue <- sapply(redMax,function(x){mean(x[["blue"]])})
  lad <- try(L1pack::lad(meanRed ~ meanBlue,method="BR"))
  if (inherits(lad,error)) {
    regrVals <- c(interceptDefault,slopeDefault) # if LAD did not work take default intercept and slope values
  } else {
    regrVals <- c(lad$coefficients[1],lad$coefficients[2]) # intercept and slope
  }
  
  
  
  
}