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
#' @note Mainly thought for internal use and only tested for preview images. 
#' 
#' @param record data.frame, single line representing one record from a records data.frame.
#' @param preview raster, subject of cloud cover calculation. Either two layers: layer 1 = red, layer 2 = blue. Or three layers: layer 1 = red, layer 2 = something, layer 3 = blue.
#' @param aoi sp or sf, the aoi.
#' @param identifier numeric, column number where a unique identifier of the scenes is located, sensor-specific
#' @param maxDeviation numeric, the maximum allowed deviation of calculated scene cloud cover from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover % given by the data distributor. Default is \code{maxDeviation = 20}.
#' @param sceneCloudCoverCol character, the clear name of the column in the record data.frame where the cloud cover estimation of the data dissiminator is found.
#' @param cloudPrbThreshold numeric, the threshold of the HOT cloud probability layer (0-100 %) below which pixels are considered as clear sky. Default is \code{cloudPrbThreshold = 40}. It will be dynamically adjusted according to the input in \code{maxDeviation}
#' @param slopeDefault numeric, value taken as slope ONLY if least-alternate deviation regression fails.  Default is 1.4, proven to work well for common land surfaces.f default values. In this case cloud cover will be set to 9999 % for the given record.
#' @param interceptDefault, value taken as intercept ONLY if least-alternate deviation regression fails. Default is -10, proven to work well for common land surfaces.
#' @param dir_out character, optional. Full path to target directory where to save the cloud masks. If \code{NULL}, cloud masks are not saved.
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#'        
#' @return A data.frame, one line as the input with one additional column holding the estimated cloud cover within the aoi
#' 
#' @author Henrik Fisser
#' 
#' @importFrom raster crs projectRaster nlayers stack values mask maxValue minValue as.matrix writeRaster
#' @importFrom L1pack lad
#' @importFrom stats na.omit
#' 
#' @export

calc_hot_cloudcov <- function(record, preview, aoi = NULL, identifier = NULL, maxDeviation = 20, sceneCloudCoverCol, cloudPrbThreshold = 40, slopeDefault = 1.4, interceptDefault = -10, dir_out = NULL, num_records = 1, verbose = TRUE) {
  
  AOIcloudcoverpercentage <- "AOIcloudcoverpercentage" # for aoi cloud cover column
  error <- "try-error"
  crsError <- " Desired coordinate system:\n"
  currTitle <- record[[identifier]]
  cloudPrbThresh <- 40 # this threshold
  maxTry <- 30 # how often HOT calculation should be repeated with adjusted threshold
  
  crs <- raster::crs(preview)
  if (is.na(crs)) {
    out("Preview seems not to be geo-referenced. No projection found.",type=3)
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
  bBins <- lapply(2:bThresh,function(x){which(valDf[[2]] == x)}) # these are the bins of interest for blue DNs
  bBins <- lapply(bBins,function(x){data.frame(red=valDf[x,1],blue=valDf[x,2])}) # get the red and blue DNs where bins are valid
  redMax <- lapply(1:length(bBins),function(x){bBins[[x]][order(bBins[[x]][["red"]]),]}) # order the data.frames by red
  redMax <- lapply(redMax,function(x){
    if (NROW(x) >= rThresh) {
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
    if (slopeDefault == 0) {
      hotFailed <- TRUE # shit
    } else {
      hotFailed <- FALSE
    }
    regrVals <- c(interceptDefault,slopeDefault) # if LAD did not work take default intercept and slope values
  } else {
    hotFailed <- FALSE
    regrVals <- c(lad$coefficients[1],lad$coefficients[2]) # intercept and slope
  }
  
  ## Check if valid pixels are found within aoi
  # non-valid pixels appear as 0 DNs in preview images, not as NAs
  prevMasked <- mask(preview,aoi)
  prevMasked[is.na(prevMasked)] <- 0 # to be sure set possible NAs also to 0
  maxValPrevMasked <- maxValue(prevMasked)
  if (maxValPrevMasked == 0) {
    record[[AOIcloudcoverpercentage]] <- 100
    out(paste0("\nThe following record has no observations within aoi, cloud cover percentage is set to 100 thus: \n",record[[identifier]]),msg=TRUE)
    return(record)
  }
  ## Calculate cloud probability layer for the whole scene
  intercept <- as.numeric(regrVals[1])
  slope <- as.numeric(regrVals[2])
  try(nominator <- abs(slope * bBand - rBand + intercept))
  try(denominator <- sqrt(1 + slope^2))
  try(HOT <- nominator / denominator)
  if (inherits(HOT,error) || inherits(nominator,error) || inherits(denominator,error)) {
    hotFailed <- TRUE
  }
  HOT <- (HOT - minValue(HOT)) / (maxValue(HOT) - minValue(HOT)) * 100 # rescale to 0-100
  # calculate scene cc % while deviation between HOT cc % and provided cc % larger maximum deviation from provider (positive or negative)
  numTry <- 1
  ccDeviationFromProvider <- 10 # this value does not matter at the beginning. Start with random value
  while (numTry <= maxTry && (ccDeviationFromProvider >= 3 || ccDeviationFromProvider <= -3)) { # iterate maximum 30 times
    if (numTry > 1) {cloudPrbThreshold <- cloudPrbThreshold + 1}
    cMask <- try(HOT < cloudPrbThreshold) # threshold to seperate cloud pixels
    if (inherits(cMask,error)) {
      hotFailed <- TRUE
    }
    cMaskMat <- raster::as.matrix(cMask)
    cPercent <- (length(which(cMaskMat == 0)) / length(which(!is.na(cMaskMat)))) * 100 # calculate cloud percentage within whole scene for comparison with actual cloud cover for whole scene calculated by data provider
    try(ccDeviationFromProvider <- as.numeric(records[1,sceneCloudCoverCol]) - as.numeric(cPercent)) # difference between scene cloud cover from HOT and from data provider
    if (inherits(ccDeviationFromProvider,error) || is.na(ccDeviationFromProvider) || is.null(ccDeviationFromProvider)) {
      ccDeviationFromProvider <- maxDeviation - 1 # escape the loop by setting the value artificially because calculation failed
      hotFailed <- TRUE
    } else {
      if (ccDeviationFromProvider >= maxDeviation) { # if deviation is larger positive maxDeviation
        cloudPrbThreshold <- cloudPrbThreshold - 1 # decrease threshold value because HOT cc % is lower than provided cc %
      } else if (ccDeviationFromProvider <= -maxDeviation) { # if deviation is smaller negative maxDeviation
        cloudPrbThreshold <- cloudPrbThreshold + 1 # increase threshold value because HOT cc % is higher than provided
      }
    }
    numTry <- numTry + 1
  }
  ## Calculate cloud cover percentage
  if (isFALSE(hotFailed)) {
    cMask <- mask(cMask,aoi)
    if (!is.null(dir_out)) { # save cloud mask if desired
      maskFilename <- paste0(dir_out,"\\",record[1,identifier],"_cloud_mask.tif")
      writeRaster(cMask,maskFilename,"GTiff",overwrite=T)
    }
    cMaskMatAoi <- as.matrix(cMask)
    cPercent <- (length(which(cMaskMatAoi==0)) / length(which(!is.na(cMaskMatAoi)))) * 100 # aoi cc %
    
    ##### Add aoi cloud cover percentage to record data.frame
    record[[AOIcloudcoverpercentage]] <- cPercent
  } else {
    record[[AOIcloudcoverpercentage]] <- 9999
    out(paste0("\nHOT could not be calculated for this record:\n",currTitle),type=2)
  }

  return(record)

}