#' ---------------------------------------------------------------------
#' @name internal_names
#' @description These are internal functions that return names used in getSpatialData.
#' The purpose is generic naming and simple maintenance. A name can for example
#' be a column name of the records data.frame.
#' @keywords internal
#' @noRd
#' ---------------------------------------------------------------------

#' Column names as determined by getSpatialData
#' @keywords internal
#' @noRd
name_product <- function() {
  return("product")
}
#' @keywords internal
#' @noRd
name_product_group <- function() {
  return("product_group")
}
#' @keywords internal
#' @noRd
name_record_id <- function() {
  return("record_id")
}
#' @keywords internal
#' @noRd
name_entity_id <- function() {
  return("entity_id")
}
#' @keywords internal
#' @noRd
name_dataset_url <- function() {
  return("dataset_url")
}
#' @keywords internal
#' @noRd
name_md5_url <- function() {
  return("md5_url")
}
#' @keywords internal
#' @noRd
name_preview_url <- function() {
  return("preview_url")
}
#' @keywords internal
#' @noRd
name_meta_url <- function() {
  return("meta_url")
}
#' @keywords internal
#' @noRd
name_meta_url_fgdc <- function() {
  return("meta_url_fgdc")
}
#' @keywords internal
#' @noRd
name_summary <- function() {
  return("summary")
}
#' @keywords internal
#' @noRd
name_date_acquisition <- function() {
  return("date_acquisition")
}
#' @keywords internal
#' @noRd
name_start_time <- function() {
  return("start_time")
}
#' @keywords internal
#' @noRd
name_stop_time <- function() {
  return("stop_time")
}
#' @keywords internal
#' @noRd
name_date_ingestion <- function() {
  return("date_ingestion")
}
#' @keywords internal
#' @noRd
name_date_modified <- function() {
  return("date_modified")
}
#' @keywords internal
#' @noRd
name_tile_number_horizontal <- function() {
  return("tile_number_horizontal")
}
#' @keywords internal
#' @noRd
name_tile_number_vertical <- function() {
  return("tile_number_vertical")
}
#' @keywords internal
#' @noRd
name_tile_id <- function() {
  return("tile_id")
}
#' @keywords internal
#' @noRd
name_cloudcov <- function() {
  return("cloudcov")
}
#' @keywords internal
#' @noRd
name_sensor_id <- function() {
  return("sensor_id")
}
#' @keywords internal
#' @noRd
name_sensor <- function() {
  return("sensor")
}
#' @keywords internal
#' @noRd
name_platform <- function() {
  return("platform")
}
#' @keywords internal
#' @noRd
name_platform_serial <- function() {
  return("platform_serial")
}
#' @keywords internal
#' @noRd
name_platform_id <- function() {
  return("platform_id")  
}
#' @keywords internal
#' @noRd
name_level <- function() {
  return("level")  
}
#' @keywords internal
#' @noRd
name_levels_available <- function() {
  return("levels_available")  
}
#' @keywords internal
#' @noRd
name_footprint <- function() {
  return("footprint")  
}
#' @keywords internal
#' @noRd
name_cloud_mask_file <- function() {
  return("cloud_mask_file")
}
#' @keywords internal
#' @noRd
name_aoi_hot_cloudcov_percent <- function() {
  return("aoi_HOT_cloudcov_percent")
}
#' @keywords internal
#' @noRd
name_scene_hot_cloudcov_percent <- function() {
  return("scene_HOT_cloudcov_percent")
}
#' @keywords internal
#' @noRd
name_preview_file <- function() {
  return("preview_file")
}
#' @keywords internal
#' @noRd
name_preview_file_jpg <- function() {
  return("preview_file_jpg")
}
#' @keywords internal
#' @noRd
name_rgb_mosaic_file <- function() {
  return("rgb_mosaic_file")
}
#' @keywords internal
#' @noRd
name_cmask_mosaic_file <- function() {
  return("cmask_mosaic_file")
}
#' @keywords internal
#' @noRd
name_selected_for_timestamp <- function() {
  return("selected_for_timestamp")
}
#' @keywords internal
#' @noRd
name_sub_period <- function() {
  return("sub_period")
}

# Names as they occur in the records 'product' column
#' @keywords internal
#' @noRd
name_product_sentinel1 <- function() {
  return("Sentinel-1")
}
#' @keywords internal
#' @noRd
name_product_sentinel2 <- function() {
  return("Sentinel-2")
}
#' @keywords internal
#' @noRd
name_product_sentinel3 <- function() {
  return("Sentinel-3")
}
#' @keywords internal
#' @noRd
name_product_landsatmss <- function() {
  return("LANDSAT_MSS_C1")
}
#' @keywords internal
#' @noRd
name_product_landsat5 <- function() {
  return("LANDSAT_TM_C1")
}
#' @keywords internal
#' @noRd
name_product_landsat7 <- function() {
  return("LANDSAT_ETM_C1")
}
#' @keywords internal
#' @noRd
name_product_landsat8 <- function() {
  return("LANDSAT_8_C1")
}
name_product_group_modis <- function() {
  return("MODIS")
}
name_product_group_landsat <- function() {
  return("Landsat")
}
name_product_group_sentinel <- function() {
  return("Sentinel")
}

# cloudcov column names wrappers
#' get column names added in calc_hot_cloudcov
#' @return list of character column names.
#' @keywords internal
#' @noRd
.cloudcov_colnames <- function() {
  
  cols <- list(cloud_mask_path = name_cloud_mask_file(),
               aoi_hot_cc_percent = name_aoi_hot_cloudcov_percent(),
               scene_hot_cc_percent = name_scene_hot_cloudcov_percent())
  return(cols)
  
}

#' get column names needed for running calc_hot_cloudcov
#' @return character vector needed_cols
#' @keywords internal
#' @noRd
.cloudcov_get_needed_cols <- function() {
  return(c(name_product(), name_product_group(), name_record_id(), name_sensor(), 
           name_cloudcov(), name_preview_url()))
}

#' get the cloudcov-supported MODIS names
#' @return character vector of MODIS names
#' @keywords internal
#' @noRd
.cloudcov_get_supported_modis <- function() {
  return(tolower(c("MCD18A1.006", "MCD18A2.006", "MCD19A1.006", "MOD09A1.006", "MOD09CMG.006", 
                   "MOD09GA.006", "MOD09GQ.006", "MOD09Q1.006", "MODOCGA.006", "MYD09A1.006", 
                   "MYD09CMG.006", "MYD09GA.006", "MYD09GQ.006", "MYD09Q1.006", "MYDOCGA.006")))
}
