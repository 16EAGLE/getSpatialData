#' ---------------------------------------------------------------------
#' @name internal_cloudcov
#' @description This is the main backend of the cloudcov functionality.
#' Frontend and processing (incl. reload actions) are located in calc_cloudcov.
#' @keywords internal
#' ---------------------------------------------------------------------

#' checks if a record is supported by calc_cloudcov() or not
#' @param record with one row
#' @return \code{is_supported} logical
#' @keywords internal
#' @noRd
.cloudcov_supported <- function(record) {
  record_id <- tolower(record[[name_record_id()]])
  product_id <- tolower(record[[name_product()]])
  supported_modis <- .cloudcov_get_supported_modis()
  supported_modis <- tolower(paste0(name_product_group_modis(), "_", supported_modis))
  if (startsWith(product_id, tolower(name_product_group_modis()))){
    return(any(startsWith(supported_modis, substr(product_id, 1, 13))))
  } else if (startsWith(product_id, tolower(name_product_group_landsat())) || product_id == tolower(name_product_sentinel2())) {
    return(TRUE)
  } else if (product_id == tolower(name_product_sentinel3())) {
    return(strsplit(record_id, "_")[[1]][2] == "ol")
  } else {
    return(FALSE)
  }
}

#' fills the record data.frame aoi cloud cover columns with NA cases if cc calculation failed or SAR is given
#' @param record data.frame with one row.
#' @param is_SAR logical if the record is a SAR acquisition. Default is FALSE.
#' @return record data.frame with one row but added columns.
#' @keywords internal
#' @noRd
.handle_cc_skip <- function(record, is_SAR = FALSE, dir_out = NULL) {
  
  if (!is.null(dir_out)) record[name_cloud_mask_file()] <- "NONE"
  record[name_aoi_hot_cloudcov_percent()] <- ifelse(is_SAR,NA,100)
  record[name_scene_hot_cloudcov_percent()] <- ifelse(is_SAR,NA,9999)
  return(record)
  
}

#' creates new columns and fills a one line records data.frame with calc_hot_cloudcov results
#' finalizes the cloud mask and saves it
#' @param record data.frame.
#' @param aoi aoi.
#' @param cMask raster cloud mask.
#' @param HOT raster HOT cloud probabilitiy layer.
#' @param scene_cPercent numeric calculated HOT scene cloud cover.
#' @param maskFilename character file path where to save the cloud mask.
#' @param cols list of character column names.
#' @param dir_given logical if a dir_out is given as argument.
#' @return record data.frame with additional columns.
#' @importFrom raster cellStats writeRaster mask
#' @keywords internal
#' @noRd
.record_cloudcov_finish <- function(record, aoi, cMask, HOT, scene_cPercent,
                                    mask_path, cols, dir_given, reload=F) {
  
  aoi_cPercent <- .raster_percent(cMask,aoi=aoi) # calculate the absolute HOT cloud cover in aoi
  if (is.null(HOT)) {
    aoi_cProb <- 9999
  } else {
    HOT_masked <- mask(HOT,aoi)
    aoi_cProb <- cellStats(HOT_masked,mean) # calculate the mean HOT cloud probability in aoi
  }
  if (isFALSE(reload)) {
    cMask <- mask(cMask,aoi)
    cMask[cMask==0] <- NA
  }
  if (dir_given) { # save cloud mask if desired
    mask_path <- mask_path
    if (!file.exists(mask_path)) writeRaster(cMask,mask_path,overwrite=T,
                                             datatype="INT2S")
    record[cols$cloud_mask_path] <- mask_path
  }
  
  ##### Add scene, aoi cloud cover percentage and mean aoi cloud cover probability to data.frame
  record[cols$aoi_hot_cc_percent] <- as.numeric(aoi_cPercent)
  record[cols$scene_hot_cc_percent] <- as.numeric(scene_cPercent)
  return(record)
  
}

#' Calculates the haze-optimal transformation cloud cover based on the red and blue band
#' 
#' \code{calc_hot_cloudcov} estimates the cloud cover of a satellite image raster using the haze-optimal transformation (HOT) within an aoi
#' The algorithm was implemented in this function for cloud cover estimation of DN values from preview images.
#' 
#' @param record data.frame, single line representing one record from a records data.frame.
#' @param preview raster, subject of cloud cover calculation. Either two layers: layer 1 = red, layer 2 = blue. Or three layers: layer 1 = red, layer 2 = something, layer 3 = blue.
#' @param aoi sp or sf, the aoi.
#' @param maxDeviation numeric between 0 and 100. The maximum allowed deviation of calculated scene cloud cover from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover \% given by the data distributor. Default is \code{maxDeviation = 5}.
#' @param cols character vector of column names.
#' @param dir_out character, optional. Full path to target directory where to save the cloud masks. If \code{NULL}, cloud masks are not saved.
#' @param tmp_dir character directory the temp dir.
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#'        
#' @return A data.frame, one line as the input with one additional column holding the estimated cloud cover within the aoi.
#' 
#' @author Henrik Fisser
#' 
#' @importFrom raster nlayers stack values mask maxValue minValue raster
#' @importFrom L1pack lad
#' @importFrom stats na.omit qexp
#' 
#' @keywords internal
#' @noRd

calc_hot_cloudcov <- function(record, preview, aoi = NULL, maxDeviation = 5, 
                              cols = NULL, dir_out = NULL, tmp_dir = NULL, verbose = TRUE) {
  
  ## Check input
  .check_dataframe(record, "record")
  .check_rasterStack(preview, "preview")
  aoi <- .check_aoi(aoi, "sp")
  dir_out <- .check_dir_out(dir_out, which="cloud_masks")
  .check_numeric(maxDeviation, "maxDeviation")
  .check_character(cols, "cols")
  .check_character(tmp_dir, "tmp_dir")
  .check_verbose(verbose)
  dir_given <- !is.null(dir_out)
  identifier <- "record_id"
  mask_path <- file.path(dir_out, paste0(record[[identifier]],"_cloud_mask.tif"))
  currTitle <- record[[identifier]]
  
  if (file.exists(mask_path)) {
    cMask <- raster(mask_path)
    # when re-loading the existing cMask the HOT scene_cPercent cannot be calculated because already aoi masked
    out("Loading existing HOT aoi cloud mask but no HOT scene cloud cover can be calculated from it, only for aoi",msg=T)
    record <- .record_cloudcov_finish(record,aoi,cMask,HOT=NULL,
                                      scene_cPercent=9999,mask_path,cols,dir_given,reload=T)
    return(record)
  }
  
  hotFailWarning <- paste0("\nHOT could not be calculated for this record:\n",currTitle)
  maxTry <- 30 # how often threshold adjustment should be repeated with adjusted threshold
  safe_cloud_thresh <- 210
  sceneCloudCoverCol <- "cloudcov"
  error <- "try-error"
  
  # Check if preview is broken (has no observations with DN >= 20)
  prev_vals <- as.integer(as.vector(values(preview)))
  is_broken <- all(prev_vals < 20)
  
  # Check for valid observations in aoi
  if (!is_broken) {
    prevMasked <- mask(preview,aoi)
    maxValPrevMasked <- maxValue(prevMasked)
    not_valid_in_aoi <- maxValPrevMasked[1] < 20 || is.na(maxValPrevMasked[1]) # max value smaller 50, no valid observations
  }
  
  # If preview is broken or nor valid observations in aoi return NA
  if (isTRUE(any(c(is_broken,not_valid_in_aoi)))) {
    return(NA)
  }
  
  # Mask NA values in preview (represented as RGB DN < 5 here)
  NA_mask <- (preview[[1]] > 5) * (preview[[2]] > 5) * (preview[[3]] > 5)
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
    out(paste0("RGB (3 layers) or RB (2 layers) image stack has to be provided as 'preview'. 
               Number of layers of the given stack is: ",nlyrs,".\nHOT could not be calculated for
               record: ", currTitle),type=3)
  }
  prvStck <- stack(bBand,rBand)
  
  ## Calculation
  
  # for dividing the blue DNs with values between these threshold values into equal interval bins
  # to be generic over different land surface we calculate the thresholds from the given image
  mean_blue_red <- c(mean(preview[[3]][preview[[3]] < safe_cloud_thresh]),
                     mean(preview[[1]][preview[[1]] < safe_cloud_thresh]))
  bThreshHigh <- as.integer(mean(mean_blue_red))
  bThreshLow <- as.integer(bThreshHigh - 50)
  bThreshHigh <- ifelse(bThreshHigh < 30,60,bThreshHigh) # values should not be extremely low
  bThreshLow <- ifelse(bThreshLow < 40,40,bThreshLow)
  
  # set initial cloud probability threshold from mean of blue and red in clear-sky aresa
  cloudPrbThreshold <- mean(mean_blue_red) - 100
  
  # this step computes first safe clear-sky pixels
  bins_seq <- bThreshLow:bThreshHigh
  valDf <- data.frame(na.omit(values(prvStck)))
  
  binIndices <- lapply(bins_seq,function(x){which(valDf[[1]] == x)}) # these are the bins of interest for blue DNs
  bBins <- lapply(binIndices,function(x){data.frame(blue=valDf[x,1],red=valDf[x,2])}) # get the red and blue DNs where bins are valid
  rm(binIndices, valDf)
  bBins <- lapply(bBins,function(x) {
    nrow <- NROW(x)
    red_order <- order(x$red,decreasing=F)[1:2] # take the lowest 2 of red values
    df_subset <- x[red_order,]
  })
  
  # from the mean values of blue and red in clear-sky areas we calculate the mean ratio between
  # the bands. As we don't do the regression on the whole supposed clear-sky areas, we conserve
  # this information by applying it as coefficients to the samples fed into the regression. 
  # Furthermore, we double the coefficient of the band with higher mean and divide the other
  # coefficient by 2. This creates a more distinguished clear-sky line while the relationship
  # between the two bands is maintained. This at the end simplifies the delineation of cloudy pixels.
  coeffBlue <- (mean_blue_red[1] / mean_blue_red[2])
  coeffRed <- (mean_blue_red[2] / mean_blue_red[1])
  coeffBlue <- ifelse(coeffBlue > 1, coeffBlue * 2, coeffBlue / 2)
  coeffRed <- ifelse(coeffRed > 1, coeffRed * 2, coeffRed / 2)
  
  meanRed <- sapply(bBins,function(x){mean(x[["red"]])}) * coeffRed
  meanBlue <- sapply(bBins,function(x){x[["blue"]][1]}) * coeffBlue
  rm(bBins)
  
  # run least-alternate deviation regression
  lad <- tryCatch({
    L1pack::lad(meanBlue~meanRed,method="BR")
  },
  error=function(err) {
    return(err)
  }
  )
  rm(meanRed, meanBlue)
  if (inherits(lad,"simpleError")) {
    hotFailed <- TRUE # remember this and handle at the end
  } else {
    hotFailed <- FALSE
    regrVals <- c(lad$coefficients[1],lad$coefficients[2]) # intercept and slope
  }
  
  intercept <- as.numeric(regrVals[1])
  slope <- as.numeric(regrVals[2])
  
  # handle problem of slope values close to 1 (in fact, this problem should rarely occur)
  # if mean of blue in likely clear-sky areas is higher than mean of red: multiply slope by 2 else divide by 2
  if (slope < 1.5 && slope > 0.5) {
    slope <- ifelse(mean_blue_red[1] > mean_blue_red[2], slope*2, slope/2)
  }
  
  # get a sharp clear-sky-line where slope is close to 1
  # if mean of blue in likely clear-sky areas is higher red multiply slope by 2 else divide by 2
  # needed because slope values close to 1 lead to weak cloud seggregation
  thresh <- c(0.6,1.4)
  if (slope > thresh[1] && slope < thresh[2]) {
    slope <- ifelse(mean_blue_red[1] > mean_blue_red[2],slope*2,slope/2)
    in_t <- c(slope > thresh[1],slope < thresh[2])
    if(in_t[1] && in_t[2]) {
      # if slope is still close to 0
      slope <- ifelse(slope < 1,slope/2,slope*2)
    }
  }
  
  # calculate HOT cloud probablity layer
  try(nominator <- abs(slope * rBand - bBand + intercept))
  try(denominator <- sqrt(1 + (slope^2)))
  HOT <- try(nominator / denominator)
  if (inherits(HOT,error) || inherits(nominator,error) || inherits(denominator,error)) {
    hotFailed <- TRUE
  }
  HOT <- (HOT - minValue(HOT)) / (maxValue(HOT) - minValue(HOT)) * 100 # rescale to 0-100
  
  # calculate scene cc \% while deviation between HOT cc \% and provided cc \% larger maximum deviation from provider (positive or negative)
  numTry <- 1
  ccDeviationFromProvider <- 101
  while (isFALSE(hotFailed)
         && numTry <= maxTry 
         && abs(ccDeviationFromProvider) > maxDeviation) { # tolerance 2
    cMask <- try(HOT < cloudPrbThreshold) # threshold to seperate cloud pixels
    if (inherits(cMask,error)) hotFailed <- TRUE
    # calculate current cloud coverage
    cPercent <- .raster_percent(cMask,mode="custom",custom=c(0,1))
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
  scene_cPercent <- .raster_percent(cMask,mode="custom",custom=c(0,1))
  
  ## Calculate aoi cloud cover percentage
  if (hotFailed) {
    record <- .handle_cc_skip(record,dir_out=dir_out)
    out(hotFailWarning, type=2)
    return(NA)
  } else {
    record <- .record_cloudcov_finish(record,aoi,cMask,HOT,
                                      scene_cPercent,mask_path,cols,dir_given)
  }
  return(record)
  
}