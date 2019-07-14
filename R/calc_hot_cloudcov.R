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
#' @note Mainly thought for internal use in getSpatialData and only tested for preview images of Sentinel, Landsat, MODIS.
#' 
#' @param record data.frame, single line representing one record from a records data.frame.
#' @param preview raster, subject of cloud cover calculation. Either two layers: layer 1 = red, layer 2 = blue. Or three layers: layer 1 = red, layer 2 = something, layer 3 = blue.
#' @param aoi sp or sf, the aoi.
#' @param identifier numeric, column number where a unique identifier of the scenes is located, sensor-specific.
#' @param maxDeviation numeric, the maximum allowed deviation of calculated scene cloud cover from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover \% given by the data distributor. Default is \code{maxDeviation = 5}.
#' @param sceneCloudCoverCol character, the clear name of the column in the record data.frame where the cloud cover estimation of the data dissiminator is found.
#' @param cloudPrbThreshold numeric, the threshold of the HOT cloud probability layer (0-100 \%) below which pixels are considered as clear sky. Default is \code{cloudPrbThreshold = 40}. It will be dynamically adjusted according to the input in \code{maxDeviation}.
#' @param slopeDefault numeric, value taken as slope ONLY if least-alternate deviation regression fails.  Default is 1.4, proven to work well for common land surfaces.f default values. In this case cloud cover will be set to 9999 \% for the given record.
#' @param interceptDefault numeric, value taken as intercept ONLY if least-alternate deviation regression fails. Default is -10, proven to work well for common land surfaces.
#' @param dir_out character, optional. Full path to target directory where to save the cloud masks. If \code{NULL}, cloud masks are not saved.
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#'        
#' @return A data.frame, one line as the input with one additional column holding the estimated cloud cover within the aoi.
#' 
#' @author Henrik Fisser
#' 
#' @importFrom raster NAvalue crs projectRaster nlayers stack values mask maxValue minValue as.matrix writeRaster cellStats
#' @importFrom L1pack lad
#' @importFrom stats na.omit qexp
#' @importFrom hashmap hashmap
#' @importFrom sf st_as_sf st_transform
#' @importFrom sp proj4string
#' 
#' @export

calc_hot_cloudcov <- function(record, preview, aoi = NULL, identifier = NULL, maxDeviation = 5, 
                              sceneCloudCoverCol = NULL, cloudPrbThreshold = 40, slopeDefault = 1.4, 
                              interceptDefault = -10, dir_out = NULL, verbose = TRUE) {
  out(paste0("Processing: ",record$entityId),msg=T)
  scene_hot_cc_percent <- "scene_HOT_cloudcov_percent"
  aoi_hot_cc_percent <- "aoi_HOT_cloudcov_percent"
  cloud_mask_path <- "cloud_mask_file"
  aoi_hot_cloud_mask <- "aoi_HOT_cloud_mask"
  aoi_HOT_mean_probability <- "aoi_HOT_mean_probability"
  na_case <- "NONE"
  error <- "try-error"
  currTitle <- record[[identifier]]
  hotFailWarning <- paste0("\nHOT could not be calculated for this record:\n",currTitle)
  cloudPrbThresh <- 40 # this threshold
  maxTry <- 30 # how often HOT calculation should be repeated with adjusted threshold
  
  crs <- proj4string(preview)
  if (is.na(crs)) {
    out("Preview seems not to be geo-referenced. No projection found.",type=3)
  }
  aoi <- .handle_aoi(aoi,crs)
  
  # Handle broken preview (indicated by non-existence of DNs > 40)
  broken_check <- preview > 40
  if (any(maxValue(broken_check)) == 0) cond <- TRUE
  
  # Check for valid observations in aoi
  prevMasked <- mask(preview,aoi)
  maxValPrevMasked <- maxValue(prevMasked)
  cond <- maxValPrevMasked[1] < 50 || is.na(maxValPrevMasked[1]) # max value smaller 50, no valid observations
  # If preview is broken or nor valid observations in aoi return
  if (isTRUE(cond)) {
    record[[aoi_hot_cc_percent]] <- 100
    record[[scene_hot_cc_percent]] <- 9999
    record[[aoi_HOT_mean_probability]] <- 100
    if (!is.null(dir_out)) {record[[cloud_mask_path]] <- na_case}
    out(paste0("\nThe following record has no observations within aoi, cloud cover percentage is set to 100 thus: \n",record[[identifier]]),msg=TRUE)
    return(record)
  }
    
  # Mask NA values in preview (represented as 0 here)
  NA_mask <- (preview[[1]] > 0) * (preview[[2]] > 0) * (preview[[3]] > 0)
  preview <- mask(preview,NA_mask,maskvalue=0)
  # in case of Landsat the tiles have bad edges not represented as zeros that have to be masked as well
  preview <- .preview_mask_edges(preview)
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
  ## Calculate least-alternate deviation (LAD) regression
  # this step computes first safe clear-sky pixels adpated from the bins method from Zhu & Helmer 2018
  # the procedure was adapted slightly because here it is being computed with discontinuous DN values. Suitable values for
  # r and b were investigated systemetically. For HOT slope values of more than 1.4 were investigated to be most
  # suitable. The given r and b value are thus values that lead to slope and intercept that have the highest capability
  # to safely discriminate clouds from non-cloud when calculating HOT. Low slope values close to 1 may lead for example to 
  # a confusion of bright or reddish land surfaces with clouds
  bThresh <- 20 # for dividing the 20 lowest blue DN values into equal interval bins
  rThresh <- 20 # from the 20 blue bins take the 20 lowest red values
  valDf <- data.frame(na.omit(values(prvStck)))
  bBins <- lapply(1:bThresh,function(x){which(valDf[[2]] == x)}) # these are the bins of interest for blue DNs
  bBins <- lapply(bBins,function(x){data.frame(red=valDf[x,1],blue=valDf[x,2])}) # get the red and blue DNs where bins are valid
  redMax <- lapply(1:length(bBins),function(x){bBins[[x]][order(bBins[[x]][["red"]]),]}) # order the data.frame by red values (ascending!)
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
  lad <- try(L1pack::lad(meanBlue ~ meanRed,method="BR")) # run least-alternate deviation regression
  if (inherits(lad,error)) {
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
  
  ## Handle slope values close to 1
  # Sometimes the slope value is very close to 1. This may result from the bad quality of the preview
  # It is handled by adding an adjustment power a to the slope in the linear function (nominator). This adjustment
  # power is determined linear to the distance from 1. For slope values of <= 0.5 >= -0.5 and >= 1.5 a is 1 because this
  # are safe values. If slope is 1, the slope default value is taken
  t <- -0.8 # threshold between this value and -1 respectively both
  t_high <- 1+(1-abs(t))
  step <- 0.01
  s <- round(slope,2) # round the slope value to two digits in order to look-up the closest adjustment power
  a_vals <- (qexp((10:63)/63) * 5)[3:(length(seq(abs(t),1,step)))] # use an exponential increase of a towards 1
  a_vals_decr <- sort(a_vals,decreasing=TRUE)
  u <- rep(1,length(seq(-100,-t_high,step)))
  v <- rep(1,length(seq(t,-t,step)))
  a_vec <- c(u,a_vals,1,a_vals_decr,v,a_vals,1,a_vals_decr,u)
  s_vec <- round(seq(-100,100,step),2)
  a_hashmap <- hashmap(s_vec,a_vec)
  a <- a_hashmap$find(s)
  if (s == 1) {slope <- slopeDefault}
  
  ## Calculate HOT cloud probablity layer
  try(nominator <- abs(slope^a * bBand - rBand + intercept))
  try(denominator <- sqrt(1 + slope^2))
  try(HOT <- nominator / denominator)
  if (inherits(HOT,error) || inherits(nominator,error) || inherits(denominator,error)) {
    hotFailed <- TRUE
  }
  HOT <- (HOT - minValue(HOT)) / (maxValue(HOT) - minValue(HOT)) * 100 # rescale to 0-100
  # calculate scene cc \% while deviation between HOT cc \% and provided cc \% larger maximum deviation from provider (positive or negative)
  numTry <- 1
  ccDeviationFromProvider <- 100 # start with 100 to enter loop
  while (numTry <= maxTry && abs(ccDeviationFromProvider) > maxDeviation && (ccDeviationFromProvider >= 2 || ccDeviationFromProvider <= -2)) { # tolerance 2
    #if (numTry > 1) {cloudPrbThreshold <- cloudPrbThreshold + 1}
    cMask <- try(HOT < cloudPrbThreshold) # threshold to seperate cloud pixels
    if (inherits(cMask,error)) {
      hotFailed <- TRUE
    }
    cPercent <- .raster_percent(cMask)
    try(ccDeviationFromProvider <- as.numeric(record[1,sceneCloudCoverCol]) - as.numeric(cPercent)) # difference between scene cloud cover from HOT and from data provider
    if (inherits(ccDeviationFromProvider,error) || is.na(ccDeviationFromProvider) || is.null(ccDeviationFromProvider)) {
      ccDeviationFromProvider <- maxDeviation - 1 # escape the loop by setting the value artificially because calculation failed
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
  # mask preview to aoi
  prevMasked <- mask(preview,aoi)
  ## Calculate aoi cloud cover percentage
  if (isFALSE(hotFailed)) {
    cMask <- mask(cMask,aoi) 
    HOT_masked <- mask(HOT,aoi)
    aoi_cPercent <- .raster_percent(cMask) # calculate the absolute HOT cloud cover in aoi
    aoi_cProb <- raster::cellStats(HOT_masked,mean) # calculate the mean HOT cloud probability in aoi
    cMask[cMask==0] <- NAvalue(cMask)
    if (!is.null(dir_out)) { # save cloud mask if desired
      maskFilename <- file.path(dir_out,paste0(record[1,identifier],"_cloud_mask.tif"))
      writeRaster(cMask,maskFilename,"GTiff",overwrite=T)
      record[[cloud_mask_path]] <- maskFilename
    }
    ##### Add scene, aoi cloud cover percentage and mean aoi cloud cover probability to data.frame
    record[[aoi_hot_cc_percent]] <- as.numeric(aoi_cPercent)
    record[[aoi_HOT_mean_probability]] <- as.numeric(aoi_cProb)
    record[[scene_hot_cc_percent]] <- as.numeric(scene_cPercent)
  } else {
    record[[aoi_hot_cc_percent]] <- 100
    record[[aoi_HOT_mean_probability]] <- 100
    record[[scene_hot_cc_percent]] <- 9999
    out(hotFailWarning,type=2)
  }
  return(record)
  
}