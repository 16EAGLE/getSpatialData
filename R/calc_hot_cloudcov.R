#' Calculates the haze-optimal transformation cloud cover based on the red and blue band
#' 
#' \code{calc_hot_cloudcov} estimates the cloud cover of a satellite image raster using the haze-optimal transformation (HOT) within an aoi
#' The algorithm was implemented in this function for cloud cover estimation of DN values from preview images.
#' 
#' @param record data.frame, single line representing one record from a records data.frame.
#' @param preview raster, subject of cloud cover calculation. Either two layers: layer 1 = red, layer 2 = blue. Or three layers: layer 1 = red, layer 2 = something, layer 3 = blue.
#' @param aoi sp or sf, the aoi.
#' @param maxDeviation numeric between 0 and 100. The maximum allowed deviation of calculated scene cloud cover from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover \% given by the data distributor. Default is \code{maxDeviation = 5}.
#' @param cloudPrbThreshold numeric the threshold of the HOT cloud probability layer (0-100, 100 = highest prob.) below which pixels are denoted as clear sky. Default is \code{cloudPrbThreshold = 40}. 
#' It will be dynamically adjusted according to the input in \code{maxDeviation} if \code{maxDeviation < 100}.
#' @param slopeDefault numeric, value taken as slope ONLY if least-alternate deviation regression fails.  Default is 1.4, proven to work well for common land surfaces.f default values. In this case cloud cover will be set to 9999 \% for the given record.
#' @param interceptDefault numeric, value taken as intercept ONLY if least-alternate deviation regression fails. Default is -10, proven to work well for common land surfaces.
#' @param dir_out character, optional. Full path to target directory where to save the cloud masks. If \code{NULL}, cloud masks are not saved.
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#'        
#' @return A data.frame, one line as the input with one additional column holding the estimated cloud cover within the aoi.
#' 
#' @author Henrik Fisser
#' 
#' @importFrom raster NAvalue crs projectRaster nlayers stack values mask maxValue minValue as.matrix writeRaster cellStats raster
#' @importFrom L1pack lad
#' @importFrom stats na.omit qexp
#' @importFrom sf st_as_sf st_transform
#' @importFrom sp proj4string
#' 
#' @keywords internal
#' @noRd

calc_hot_cloudcov <- function(record, preview, aoi = NULL, maxDeviation = 5, 
                              cloudPrbThreshold = 40, slopeDefault = 1.4, 
                              interceptDefault = -10, dir_out = NULL, verbose = TRUE) {
  
  identifier <- "record_id"
  dir_given <- !is.null(dir_out)
  cols <- .cloudcov_colnames()
  sceneCloudCoverCol <- "cloudcov"
  error <- "try-error"
  currTitle <- record[[identifier]]
  # for dividing the blue DNs with values between this value and the value below into equal interval bins
  bThresh_low <- 20 
  bThresh_high <- 160 # below this value a pixel cannot be classified as cloud
  hotFailWarning <- paste0("\nHOT could not be calculated for this record:\n",currTitle)
  maxTry <- 30 # how often HOT calculation should be repeated with adjusted threshold
  maskFilename <- file.path(dir_out,paste0(record[[identifier]],"_cloud_mask.tif"))
  if (file.exists(maskFilename)) {
    cMask <- raster(maskFilename)
    # when re-loading the existing cMask the HOT scene_cPercent cannot be calculated because already aoi masked
    out("Loading existing HOT aoi cloud mask but no HOT scene cloud cover can be calculated from it, only for aoi",msg=T)
    record <- .record_cloudcov_finish(record,aoi,cMask,HOT=NULL,
                                      scene_cPercent=9999,maskFilename,cols,dir_given,reload=T)
    return(record)
  }
  
  crs <- try(proj4string(preview))
  if (is.na(crs) || inherits(crs,error)) {
    record <- .handle_cc_skip(record,dir_out=dir_out)
    return(record)
  }
  
  aoi <- .handle_aoi(aoi,crs)
  
  # Handle broken preview (indicated by non-existence of DNs > 40)
  broken_check <- preview > 40
  cond <- ifelse(any(maxValue(broken_check) == 0),TRUE,FALSE)
  # Check for valid observations in aoi
  prevMasked <- mask(preview,aoi)
  maxValPrevMasked <- maxValue(prevMasked)
  cond <- maxValPrevMasked[1] < 50 || is.na(maxValPrevMasked[1]) # max value smaller 50, no valid observations
  # If preview is broken or nor valid observations in aoi return
  if (isTRUE(cond)) {
    record <- .handle_cc_skip(record,dir_out=dir_out)
    return(NA)
  }
  
  # Mask NA values in preview (represented as 0 here)
  NA_mask <- (preview[[1]] > 0) * (preview[[2]] > 0) * (preview[[3]] > 0)
  preview <- mask(preview,NA_mask,maskvalue=0)

  # in case of Landsat the tiles have bad edges not represented as zeros that have to be masked as well
  if (record$product_group %in% c("Landsat")) preview <- .preview_mask_edges(preview)
  
  ## Prepare RGB or RB stack
  nlyrs <- nlayers(preview)
  if (nlyrs == 3) {
    bBand <- preview[[3]]
    rBand <- preview[[1]]
  } else if (nlyrs == 2) {
    bBand <- preview[[2]]
    rBand <- preview[[1]]
  } else {
    out(paste0("RGB (3 layers) or RB (2 layers) image stack has to be provided as 'preview'. The number of layers of the given stack is: ",nlyrs,".\nHOT could not be calculated for record: ",currTitle),type=3)
  }
  prvStck <- stack(bBand,rBand)
  # create a mask where blue band has values below 160
  # at the end these values will be switched to clear-sky where since it is assumed they cannot be cloud
  safe_clear <- prvStck[[1]] <= 160
  ## Calculate least-alternate deviation (LAD) regression
  # this step computes first safe clear-sky pixels adpated from the bins method from Zhu & Helmer 2018
  # the procedure was adapted slightly because here it is being computed with discontinuous DN values. Suitable values for
  # r and b were investigated systemetically. For HOT slope values of more than 1.4 were investigated to be most
  # suitable. The given r and b value are thus values that lead to slope and intercept that have the highest capability
  # to safely discriminate clouds from non-cloud when calculating HOT. Low slope values close to 1 may lead for example to 
  # a confusion of bright or reddish land surfaces with clouds
  rThresh <- 20 # from the blue bins take the 20 highest red values
  valDf <- data.frame(na.omit(values(prvStck)))
  bBins <- lapply(bThresh_low:bThresh_high,function(x){which(valDf[[2]] == x)}) # these are the bins of interest for blue DNs
  bBins <- lapply(bBins,function(x){data.frame(red=valDf[x,1],blue=valDf[x,2])}) # get the red and blue DNs where bins are valid
  redMax <- lapply(1:length(bBins),function(x){bBins[[x]][order(bBins[[x]][["red"]],decreasing=T),]}) # order the data.frame by red values (ascending!)
  redMax <- lapply(redMax,function(x){
    redNrow <- NROW(x)
    if (redNrow >= rThresh) {
      x <- try(x[redNrow:(redNrow-rThresh),]) # more or as many red values as rThresh are available. Take values from last value (highest in order) to last value minus rThresh
    } else {
      # in case less than rThresh values are available take the maximum number of available values
      x <- try(x[redNrow:1,]) # do it complicated in order to order from high to low
    }
  })
  meanRed <- sapply(redMax,function(x){mean(x[["red"]])})
  meanBlue <- sapply(redMax,function(x){mean(x[["blue"]])})
  
  # run least-alternate deviation regression
  lad <- tryCatch({
    L1pack::lad(meanBlue ~ meanRed,method="BR")
    },
    error=function(err) {
      return(err)
    }
  )
  if (inherits(lad,"simpleError")) {
    if (slopeDefault == 0) {
      hotFailed <- TRUE # shit. Remember this and handle at the end
    } else {
      hotFailed <- FALSE
    }
    regrVals <- c(interceptDefault,slopeDefault) # if LAD did not work take default intercept and slope values
  } else {
    hotFailed <- FALSE
    regrVals <- c(lad$coefficients[1],lad$coefficients[2]) # intercept and slope
  }

  ## Calculate cloud probability layer for the whole scene
  intercept <- as.numeric(regrVals[1])
  slope <- as.numeric(regrVals[2])
  ## Calculate HOT cloud probablity layer
  try(nominator <- abs(slope * bBand - rBand + intercept))
  try(denominator <- sqrt(1 + slope^2))
  HOT <- try(nominator / denominator)
  if (inherits(HOT,error) || inherits(nominator,error) || inherits(denominator,error)) {
    hotFailed <- TRUE
  }
  HOT <- (HOT - minValue(HOT)) / (maxValue(HOT) - minValue(HOT)) * 100 # rescale to 0-100
  # calculate scene cc \% while deviation between HOT cc \% and provided cc \% larger maximum deviation from provider (positive or negative)
  numTry <- 1
  ccDeviationFromProvider <- 101 # start with 101 to enter loop
  while (isFALSE(hotFailed)
         && numTry <= maxTry 
         && abs(ccDeviationFromProvider) > maxDeviation 
         && (ccDeviationFromProvider >= 2 || ccDeviationFromProvider <= -2)) { # tolerance 2
    cMask <- try(HOT < cloudPrbThreshold) # threshold to seperate cloud pixels
    # values that are considered as safe clear according to bThresh_high
    # are all set to 1. This reduces an error e.g. where dark surfaces have high
    # values in the HOT layer because they deviate too strongly from relatively bright
    # reference pixels (e.g. in desert areas)
    cMask[safe_clear] <- 1
    if (inherits(cMask,error)) hotFailed <- TRUE
    # calculate current cloud coverage
    cPercent <- .raster_percent(cMask)
    try(ccDeviationFromProvider <- as.numeric(record[[sceneCloudCoverCol]][1]) - as.numeric(cPercent)) # difference between scene cloud cover from HOT and from data provider
    if (inherits(ccDeviationFromProvider,error) 
        || is.na(ccDeviationFromProvider) 
        || is.null(ccDeviationFromProvider)) {
      hotFailed <- TRUE
    } else {
      if (ccDeviationFromProvider >= maxDeviation) { # if deviation is larger positive maxDeviation
        cloudPrbThreshold <- cloudPrbThreshold - 1 # decrease threshold value because HOT cc \% is lower than provided cc \%
      } else if (ccDeviationFromProvider <= -maxDeviation) { # if deviation is smaller negative maxDeviation
        cloudPrbThreshold <- cloudPrbThreshold + 1 # increase threshold value because HOT cc \% is higher than provided
      }
    }
    numTry <- numTry + 1
  }
  
  # calc scene cc percentage 
  scene_cPercent <- .raster_percent(cMask)
  ## Calculate aoi cloud cover percentage
  if (isFALSE(hotFailed)) {
    record <- .record_cloudcov_finish(record,cMask,HOT,scene_cPercent,maskFilename,cols,dir_given)
  } else {
    record <- .handle_cc_skip(record,dir_out=dir_out)
    out(hotFailWarning,type=2)
    return(NA)
  }
  return(record)
  
}