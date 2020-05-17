#' ---------------------------------------------------------------------
#' name: internal_names
#' description: These are internal functions that return names used in getSpatialData.
#' The purpose is generic naming and simple maintenance. A name can for example
#' be a column name of the records data.frame.
#' ---------------------------------------------------------------------

#### column names as determined by getSpatialData
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

#### names related to products
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
name_sentinel2_L2A <- function() {
  return("MSIL12A")
}
#' @keywords internal
#' @noRd
name_sentinel2_L1C <- function() {
  return("MSIL1C")
}

#' @keywords internal
#' @noRd
name_product_sentinel3 <- function() {
  return("Sentinel-3")
}
#' @keywords internal
#' @noRd
name_product_sentinel5 <- function() {
  return("Sentinel-5")
}
name_product_sentinel5p <- function() {
  return("Sentinel-5P")
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
#' @keywords internal
#' @noRd
name_product_group_modis <- function() {
  return("MODIS")
}
#' @keywords internal
#' @noRd
name_product_group_landsat <- function() {
  return("Landsat")
}
#' @keywords internal
#' @noRd
name_product_group_sentinel <- function() {
  return("Sentinel")
}
#' @keywords internal
#' @noRd
# names of continental / global Sentinel-3 tiles
names_continental_s3 <- function() {
  return(c("global", "africa", "europe", "australia", "asia", "south_america", "north_america"))
}

#' returns optical product names supported by calc_cloudcov
#' @return character vector of product names
#' @keywords internal
#' @noRd
.cloudcov_products <- function() {
  optical_sensors <- c(name_product_landsat8(), 
                       name_product_landsat7(), 
                       name_product_landsat5(), 
                       name_product_landsatmss(),
                       name_product_sentinel2(), 
                       name_product_sentinel3())
  optical_sensors <- append(optical_sensors, .get_cloudcov_supported_modis())
  return(optical_sensors)
}

#' get the cloudcov-supported MODIS names
#' @return character vector of MODIS names
#' @keywords internal
#' @noRd
.get_cloudcov_supported_modis <- function() {
  return(c("MODIS_MCD19A1", "MODIS_MOD09A1", "MODIS_MOD09GA", 
           "MODIS_MOD09GQ", "MODIS_MOD09Q1", "MODIS_MODOCGA", 
           "MODIS_MYD09A1", "MODIS_MYD09GA", 
           "MODIS_MYD09GQ", "MODIS_MYD09Q1", "MODIS_MYDOCGA", 
           "MODIS_MOD09CMG", "MODIS_MYD09CMG"))
}

#' get the select-supported MODIS names. CMG not supported due to different gridding
#' @return character vector of MODIS names
#' @keywords internal
#' @noRd
.get_select_supported_modis <- function() {
  supported <- .get_cloudcov_supported_modis()
  supported[which(supported %in% c("MODIS_MYD09CMG", "MODIS_MOD09CMG"))] <- NA
  return(.gsd_compact(supported))
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
  return(c(name_product(), name_product_group(), name_record_id(), name_product(), 
           name_cloudcov(), name_preview_url()))
}

#' IO drivers for st_write()
#' @return named list of character drivers whose support is ensured and their extensions
#' @keywords internal
#' @noRd
.get_records_drivers <- function() {
  drivers <- list("GeoJSON" = ".geojson", 
                  "GPKG" = ".gpkg",
                  "netCDF" = ".nc")
  return(drivers)
}

#### classes
#' numeric
#' @keywords internal
#' @noRd
NUMERIC <- function() {return(class(numeric()))}
#' integer
#' @keywords internal
#' @noRd
INTEGER <- function() {return(class(integer()))}
#' character
#' @keywords internal
#' @noRd
CHARACTER <- function() {return(class(character()))}
#' logical
#' @keywords internal
#' @noRd
LOGICAL <- function() {return(class(logical()))}
#' List
#' @keywords internal
#' @noRd
LIST <- function() {return(class(list()))}
#' data.frame
#' @keywords internal
#' @noRd
DATAFRAME <- function() {return(class(data.frame()))}
#' RasterLayer
#' @keywords internal
#' @noRd
RASTER_LAYER <- function() {return("RasterLayer")}
#' RasterStack
#' @keywords internal
#' @noRd
RASTER_STACK <- function() {return("RasterStack")}
#' RasterBrick
#' @keywords internal
#' @noRd
RASTER_BRICK <- function()  {return("RasterBrick")}
#' sf
#' @keywords internal
#' @noRd
SF <- function()  {return("sf")}
#' sfc
#' @keywords internal
#' @noRd
SFC <- function()  {return("sfc")}
#' Spatial
#' @keywords internal
#' @noRd
SPATIAL <- function() {return("Spatial")}
#' SpatialPolygons
#' @keywords internal
#' @noRd
SPATIAL_POLYGONS <- function() {return("SpatialPolygons")}
#' SpatialPolygonsDataFrame
#' @keywords internal
#' @noRd
SPATIAL_POLYGONS_DF <- function() {return("SpatialPolygonsDataFrame")}
#' sp
#' @keywords internal
#' @noRd
SP <- function()  {return("sp")}

#### data types

#' INT2S
#' @keywords internal
#' @noRd
INT2S <- function() {return("INT2S")}


#### ERRORS
#' try-error
#' @keywords internal
#' @noRd
TRY_ERROR <- function() {"try-error"}
#' error
#' @keywords internal
#' @noRd
ERROR <- function() {return("error")}
