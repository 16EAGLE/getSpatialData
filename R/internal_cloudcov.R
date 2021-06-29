# ---------------------------------------------------------------------
# name: internal_cloudcov
# description: This is the main backend of the cloudcov functionality.
# Frontend and processing (incl. reload actions) are located in calc_cloudcov.
# author: Henrik Fisser, 2019
# ---------------------------------------------------------------------

#' checks if a record is supported by calc_cloudcov() or not
#' @param record with one row
#' @return \code{is_supported} logical
#' @keywords internal
#' @noRd
.cloudcov_supported <- function(record) {
  MODIS <- name_product_group_modis()
  given_product <- record[[name_product()]]
  is_modis <- is.modis(record)
  is_sentinel3 <- is.sentinel3(record)
  given_product <- ifelse(is_modis, MODIS, given_product)
  cloudcov_products <- .cloudcov_products()
  if (is_modis) {
    is_supported <- .record_is_refl_modis(record)
  } else if (is_sentinel3) {
    is_supported <- .record_is_olci(record) # only OLCI
  } else { # Sentinel-2, Landsat
    is_supported <- given_product %in% cloudcov_products
  }
  return(is_supported)
}

#' Calculates the haze-optimal transformation cloud cover based on the red and blue band
#' 
#' \code{calc_hot_cloudcov} estimates the cloud cover of a satellite image raster using the haze-optimal transformation (HOT) within an aoi
#' The algorithm was implemented in this function for cloud cover estimation of DN values from preview images.
#' 
#' @inheritParams calc_cloudcov
#' @param record data.frame, single line representing one record from a records data.frame.
#' @param preview raster, subject of cloud cover calculation.
#' @param cols character vector of column names.
#' @param write_cmasks logical if cloud masks shall be written.        
#' @return A data.frame, one line as the input with one additional column holding the estimated cloud cover within the aoi.
#' 
#' @author Henrik Fisser
#' 
#' @importFrom raster nlayers stack values mask maxValue minValue raster
#' @importFrom stats na.omit
#' 
#' @keywords internal
#' @noRd

calc_hot_cloudcov <- function(record, preview, aoi = NULL, max_deviation = 2, 
                              cols = NULL, write_cmasks = TRUE, dir_out = NULL, verbose = TRUE) {

  max_try <- 30 # how often threshold adjustment should be repeated with adjusted threshold
  try_error <- TRY_ERROR()
  reload_msg = "Loading existing HOT aoi cloud mask"

  .check_verbose(verbose)

  identifier <- name_record_id()
  mask_path <- file.path(dir_out, paste0(record[[identifier]], "_cloud_mask.tif"))
  
  if (.check_file_exists(mask_path)) {
    cloud_mask <- raster(mask_path)
    out(reload_msg, msg=T)
    record <- .cloudcov_record_finalize(record, aoi, cloud_mask,
                                        mask_path = mask_path, cols = cols, 
                                        write_cmasks, reload = T)
    return(record)
  }
  
  preview <- .check_crs(preview)
  has_values <- .preview_has_valid_values(preview, aoi = aoi)
  if (isFALSE(has_values)) {
    record <- .cloudcov_handle_skip(record)
    return(record)
  }

  preview <- .ensure_minmax(preview)
  
  # calculation
  water_mask <- try(.safe_water(preview))
  hot_fail <- inherits(water_mask, try_error)
  
  if (!hot_fail) {
    hot <- try(getSpatialData:::.cloudcov_calc_hot(preview, water_mask))
    hot_fail <- inherits(hot, try_error)
  }
  
  if (!hot_fail) {
    mask_list <- getSpatialData:::.cloudcov_calc_cmask(record = record, 
                                      preview = preview, 
                                      hot = hot,
                                      water_mask = water_mask,
                                      max_deviation = max_deviation,
                                      max_try = max_try)
    cloud_mask <- mask_list[[1]]
    hot_fail <- mask_list[[2]]
    if (!hot_fail) {
      # calc scene cc percentage
      na_mask <- getSpatialData:::.create_preview_na_mask(preview, record)
      cloud_mask <- mask(cloud_mask, na_mask, maskvalue=0)
      if (is.landsat(record)) {
        cloud_mask <- .landsat_preview_mask_edges(cloud_mask)
      } else if (is.sentinel2(record)) {
        cloud_mask <- .sentinel2_preview_mask_edges(cloud_mask)
      }
    }
  }
  
  # calculate aoi cloud cover percentage
  if (hot_fail) {
    record <- .cloudcov_handle_skip(record)
    return(NA)
  } else {
    record <- .cloudcov_record_finalize(record, aoi, cloud_mask,
                                        mask_path, cols, write_cmasks)
  }
  
  return(record)
  
}

#' calculate haze optimal transformation (HOT)
#' @param preview RasterStack/Brick
#' @param water_mask RasterLayer
#' @return RasterLayer hot layer
#' @importFrom raster values 
#' @importFrom stats na.omit lm
#' @importFrom utils tail
#' @keywords internal
#' @noRd
.cloudcov_calc_hot <- function(preview, water_mask) {
  LOW_RED <- 10
  HIGH_RED <- 200
  red <- preview[[1]]
  blue <- preview[[3]]
  
  # apply water mask to preview before clear-sky value extraction
  preview_masked <- mask(preview, water_mask, maskvalue=1)
  
  # extract values from preview bands without water and likely clouds
  blue_vals <- as.integer(as.vector(na.omit(values(preview_masked[[3]]))))
  red_vals <- as.integer(as.vector(na.omit(values(preview_masked[[1]]))))
  
  # define the range of red values (DN 20 to 200) considered in binning
  include <- (red_vals > LOW_RED) & (red_vals < HIGH_RED)
  # subset blue and red values to the pixel values where red is in range
  blue_vals <- blue_vals[include]
  red_vals <- red_vals[include]
  
  # for the 80 lowest unique red DNs get the three highest blue DNs
  red_vals_ordered <- order(red_vals, decreasing = F)
  red_vals_subset <- unique(red_vals[red_vals_ordered])[1:80]
  blue_binned <- c()
  red_binned <- c()
  for (x in red_vals_subset) {
    blue_vals_x <- blue_vals[which(red_vals==x)]
    blue_vals_max <- blue_vals_x[order(blue_vals_x, decreasing = T)][1:3]
    blue_vals_max[which(is.na(blue_vals_max))] <- mean(blue_vals_max)
    fill <- ifelse(x == red_vals_subset[1], x, tail(blue_binned, 1))
    blue_vals_max[which(is.na(blue_vals_max))] <- fill
    red_binned <- append(red_binned, c(x, x, x))
    blue_binned <- append(blue_binned, blue_vals_max) # max blue DN at red DN x
  }
  
  # linear regression
  regr <- tryCatch({
    lm(blue~red, data = data.frame("blue" = blue_binned, "red" = red_binned))
  },
  error=function(err) {
    return(err)
  })
  slope <- regr$coefficients[2]
  intercept <- regr$coefficients[1]
  # calculate HOT layer
  hot <- (sin(slope) * blue - cos(slope) * red + intercept)
  hot <- hot / 255 * 100  # 0-100    #.rescale_raster(hot) # rescale to 0-100
  return(hot)
}

#' calculates binary cloud mask from hot layer by iteratively adjusting threshold
#' @param record data.frame one line
#' @param preview RasterStack
#' @param hot RasterLayer
#' @param water_mask RasterLayer (1=water)
#' @param max_deviation numeric
#' @param max_try integer
#' @param return list [[1]] RasterLayer cloud_mask [[2]] logical hot_fail
#' @keywords internal
#' @noRd
.cloudcov_calc_cmask <- function(record, preview, hot, water_mask,
                                 max_deviation, max_try) {
  
  try_error <- TRY_ERROR()
  provider_cloudcov <- record[[name_cloudcov()]][1]
  
  # S3 SLSTR needs different handling due to sea surface temperature measurement
  hot_threshold <- 50
  num_try <- 1
  deviation <- 101
  
  # create water and clear mask
  clear_mask <- try(.safe_clear(preview))
  hot_fail <- inherits(clear_mask, try_error)
  
  while (isFALSE(hot_fail) && num_try <= max_try && abs(deviation) > max_deviation) {
    cloud_mask <- try(hot < hot_threshold)
    cloud_mask[water_mask == 1] <- 1
    cloud_mask[clear_mask == 1] <- 1
    hot_fail <- inherits(cloud_mask, try_error)
    cPercent <- .raster_percent(cloud_mask, mode="custom", custom = c(0,1))
    # if provider cloudcov is NA get out at this point
    if (is.na(provider_cloudcov)) provider_cloudcov <- cPercent
    # difference between scene cloud cover from HOT and from data provider
    deviation <- try(provider_cloudcov - cPercent)
    hot_fail <- inherits(deviation, try_error) || is.na(deviation) || is.null(deviation)
    if (!hot_fail) {
      # if deviation is larger positive maxDeviation
      if (deviation >= max_deviation) { 
        # decrease threshold value because HOT cc \% is lower than provided cc \%
        hot_threshold <- hot_threshold - 1
        # if deviation is smaller negative maxDeviation
      } else if (deviation <= -max_deviation) { 
        # increase threshold value because HOT cc \% is higher than provided
        hot_threshold <- hot_threshold + 1 
      }
    }
    num_try <- num_try + 1
  }
  
  return(list(cloud_mask, hot_fail))
  
}

#' fills the record data.frame aoi cloud cover columns with NA cases if cc calculation failed or SAR is given
#' @param record data.frame with one row.
#' @return record data.frame with one row but added columns.
#' @keywords internal
#' @noRd
.cloudcov_handle_skip <- function(record) {
  is_SAR <- is.sentinel1(record)
  record[[name_cloud_mask_file()]] <- "NONE"
  record[[name_aoi_hot_cloudcov_percent()]] <- ifelse(is_SAR, NA, 9999)
  record[[name_scene_hot_cloudcov_percent()]] <- ifelse(is_SAR, NA, 9999)
  return(record)
}

#' creates new columns and fills a one line records data.frame with calc_hot_cloudcov results
#' finalizes the cloud mask and saves it
#' @param record data.frame.
#' @param aoi aoi.
#' @param cloud_mask raster cloud mask.
#' @param scene_cPercent numeric calculated HOT scene cloud cover.
#' @param maskFilename character file path where to save the cloud mask.
#' @param cols list of character column names.
#' @return record data.frame with additional columns.
#' @importFrom raster cellStats writeRaster mask
#' @keywords internal
#' @noRd
.cloudcov_record_finalize <- function(record, aoi, cloud_mask,
                                      mask_path, cols, write_cmasks, reload=F) {
  
  dir_out_exists <- file.exists(dirname(mask_path))
  aoi_cPercent <- getSpatialData:::.raster_percent(cloud_mask, mode = "aoi", aoi = aoi) # calculate the absolute HOT cloud cover in aoi
  scene_cPercent <- getSpatialData:::.raster_percent(cloud_mask, mode = "custom", custom = c(0, 1))
  cloud_mask[cloud_mask == 0] <- NA    # why NA? HF
  if (dir_out_exists && write_cmasks) {
    writeRaster(cloud_mask, mask_path, overwrite=T, datatype = INT2S())
  }
  record[[cols$cloud_mask_path]] <- ifelse(file.exists(mask_path), normalizePath(mask_path), "NONE")
  # add scene, aoi cloud cover percentage and mean aoi cloud cover probability to data.frame
  record[[cols$aoi_hot_cc_percent]] <- as.numeric(aoi_cPercent)
  record[[cols$scene_hot_cc_percent]] <- as.numeric(scene_cPercent)
  return(record)
  
}

#' simple blue-red-based water mask. Can also hit dark forests
#' @param preview RasterStack or RasterBrick
#' @return RasterLayer water mask (1=water)
#' @keywords internal
#' @noRd
.safe_water <- function(preview) {
  red <- preview[[1]]
  blue <- preview[[3]]
  norm_diff <- .normalized_difference(blue, red)
  wprob <- .rescale_raster(norm_diff)
  wprob[is.na(wprob)] <- 0
  wmask <- wprob > 65
  return(wmask)
}

#' mask of clear-sky pixels
#' @param preview RasterStack or RasterBrick
#' @return RasterLayer clear mask (1=clear)
#' @importFrom raster cellStats
#' @keywords internal
#' @noRd
.safe_clear <- function(preview) {
  MEDIUM_RGB <- 110
  DARK_THRESHOLD <- 50
  medium <- ((preview[[1]] < MEDIUM_RGB)
            + (preview[[2]] < MEDIUM_RGB) 
            + (preview[[3]] < MEDIUM_RGB)) == 3
  dark <- ((preview[[1]] < DARK_THRESHOLD)
           + (preview[[2]] < DARK_THRESHOLD)
           + (preview[[3]] < DARK_THRESHOLD)) >= 1
  # handling for bright clear-sky areas (e.g. deserts) -> red higher blue
  clear_prob <- .rescale_raster(.normalized_difference(preview[[3]], preview[[1]]))
  clear_prob[is.na(clear_prob)] <- 0
  clear <- ((clear_prob < 45) + medium + dark) >= 1
  return(clear)
}
