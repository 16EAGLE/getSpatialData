#' get column names needed for running calc_hot_cloudcov
#' @return character vector needed_cols
#' @keywords internal
#' @noRd
.cloudcov_get_needed_cols <- function() {
  return(c("product", "product_group", "record_id", "sensor", "cloudcov", "preview_url"))
}

#' checks if a record is supported by calc_cloudcov() or not
#' @param record with one row
#' @return \code{is_supported} logical
#' @keywords internal
#' @noRd
.cloudcov_supported <- function(record) {
  record_id <- tolower(record$record_id)
  product_id <- tolower(record$product)
  supported_modis <- tolower(c("MCD18A1.006", "MCD18A2.006", "MCD19A1.006", "MOD09A1.006", "MOD09CMG.006", 
                               "MOD09GA.006", "MOD09GQ.006", "MOD09Q1.006", "MODOCGA.006", "MYD09A1.006", 
                               "MYD09CMG.006", "MYD09GA.006", "MYD09GQ.006", "MYD09Q1.006", "MYDOCGA.006"))
  supported_modis <- tolower(paste0("MODIS_", supported_modis))
  if (startsWith(product_id, "modis")){
    return(any(startsWith(supported_modis, substr(product_id, 1, 13))))
  } else if (startsWith(product_id, "landsat") || product_id == "sentinel-2") {
    return(TRUE)
  } else if (product_id == "sentinel-3") {
    return(strsplit(record_id, "_")[[1]][2] == "ol")
  } else {
    return(FALSE)
  }
}

#' get column names added in calc_hot_cloudcov
#' @return list of character column names.
#' @keywords internal
#' @noRd
.cloudcov_colnames <- function() {
  
  cols <- list(cloud_mask_path="cloud_mask_file",
               aoi_hot_cc_percent="aoi_HOT_cloudcov_percent",
               scene_hot_cc_percent="scene_HOT_cloudcov_percent")
  
}


#' fills the record data.frame aoi cloud cover columns with NA cases if cc calculation failed or SAR is given
#' @param record data.frame with one row.
#' @param is_SAR logical if the record is a SAR acquisition. Default is FALSE.
#' @return record data.frame with one row but added columns.
#' @keywords internal
#' @noRd
.handle_cc_skip <- function(record, is_SAR = FALSE, dir_out = NULL) {
  
  if (!is.null(dir_out)) record["cloud_mask_file"] <- "NONE"
  record["aoi_HOT_cloudcov_percent"] <- ifelse(is_SAR,NA,100)
  record["scene_HOT_cloudcov_percent"] <- ifelse(is_SAR,NA,9999)
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