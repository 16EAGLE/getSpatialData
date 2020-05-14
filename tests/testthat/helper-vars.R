# TEST DIRECTORIES
# -----------------
tt <- list()
tt$home <- getwd() # testthat directory

tt$home <- file.path(tt$home, "tests", "testthat")

tt$tmp <- file.path(tt$home, "tmp") # tmp dir that can be created for tests (and deleted!)
tt$resources$home <- file.path(tt$home, "resources")
tt$resources$records <- file.path(tt$resources$home, "records")
tt$resources$previews <- file.path(tt$resources$home, "previews")
tt$resources$cmasks <- file.path(tt$resources$home, "cloud_masks")
tt$resources$aoi <- file.path(tt$resources$home, "aoi_test")
dir_error <- "Cannot run tests because directory not found: "
if (!dir.exists(tt$home)) stop(paste0(dir_error, tt$home))
if (!dir.exists(tt$resources$home)) stop(paste0(dir_error, tt$resources))
for (dir in tt$resources) if (!dir.exists(dir)) stop(paste0(dir_error, dir))

# TEST PARAMETERS
# -----------------
# classes
DATAFRAME <- "data.frame"
SF <- "sf"
NUMERIC <- "numeric"
INTEGER <- "integer"
CHARACTER <- "character"
LOGICAL <- "logical"

# sensor names
SENTINEL1 <- "Sentinel-1"
SENTINEL2 <- "Sentinel-2"
SENTINEL3 <- "Sentinel-3"
LANDSAT <- "Landsat"
MODIS <- "MODIS"
MIXED <- "mixed"

# records data.frame column names
COLS <- list()
COLS$preview_jpg <- "preview_file_jpg"
COLS$preview_tif <- "preview_file"
COLS$HOT_scene <- "aoi_HOT_cloudcov_percent"
COLS$HOT_aoi <- "scene_HOT_cloudcov_percent"
COLS$cmask_tif <- "cloud_mask_file"
COLS$pmos_col <- "rgb_mosaic_file"
COLS$cmos_col <- "cmask_mosaic_file"
COLS$timestamp_col <- "selected_for_timestamp"

# for file naming
PREFIX <- list()
PREFIX$records <- "records"
PREFIX$previews <- "records_previews"
PREFIX$cmasks <- "records_cmasks"
construct_filepath <- function(dir, sensor, prefix) {
  return(file.path(dir, paste(prefix, paste0(gsub("-", "", sensor), ".gpkg"), sep="_")))
}

# HELPERS
# -----------------
# for initializing and finishing tmp dir
initialize_dir <- function(dir) {
  dir.create(dir)
}
finish_dir <- function(dir) {
  if (dir.exists(dir)) unlink(dir, TRUE)
}

set_null_cloudcov_cols <- function(records) {
  names <- names(records)
  if (COLS$HOT_scene %in% names) records[[COLS$HOT_scene]] <- NULL
  if (COLS$HOT_aoi %in% names) records[[COLS$HOT_aoi]] <- NULL
  if (COLS$cmask_tif %in% names) records[[COLS$cmask_tif]] <- NULL
  return(records)
}

set_null_preview_cols <- function(records) {
  names <- names(records)
  if (COLS$preview_jpg %in% names) records[[COLS$preview_jpg]] <- NULL
  if (COLS$preview_tif %in% names) records[[COLS$preview_tif]] <- NULL
  return(records)
}

set_null_select_cols <- function(records) {
  names <- names(records)
  if (COLS$pmos_col %in% names) records[[COLS$pmos_col]] <- NULL
  if (COLS$cmos_col %in% names) records[[COLS$cmos_col]] <- NULL
  if (COLS$timestamp_col %in% names) records[[COLS$timestamp_col]] <- NULL
  if (COLS$sub_period_col %in% names) records[[COLS$sub_period_col]] <- NULL
  return(records)
}

# for reading a raster expecting NO error
test_raster_read <- function(file) {
  expect_error(expect_error(raster(file)))
  return(raster(file)) # double expect_error() == expect NO error
}

# for reading a raster stack expecting NO error
test_stack_read <- function(file) {
  expect_error(expect_error(stack(file)))
  return(stack(file))
}

# for testing errors
# generic type error from .check_type()
type_error_msg <- function(input, arg_name, type) {
  return(paste0("Argument '", arg_name, "' must be of type '", type, "' but is '", class(input),"'"))
}
# dir_out does not exist error from .check_dir_out()
dir_out_error_msg <- function(dir_out) {
  DIR_OUT_NOT_EXIST <- "Directory 'dir_out' does not exist: "
  return(paste0(DIR_OUT_NOT_EXIST, dir_out))
}
column_error_msg <- function(column) {
  return(paste0("A column of 'records' named '", column, "' is required for this action, but is missing."))
}
AOI_TYPE_ERROR <- "Argument 'aoi' needs to be a 'SpatialPolygons' or 'sfc_POLYGON' or 'matrix' object."
AOI_UNDEFINED_ERROR <- "Argument 'aoi' is undefined and no session AOI could be obtained. Define aoi or use set_aoi() to define a session AOI."
RECORDS_TYPE_ERROR <- "Argument 'records' must be of class 'data.frame' or 'sf' 'data.frame'."

# TEST VARIABLES
# -----------------
aoi_test <- getSpatialData:::.read_polygons(file.path(tt$resources$aoi, "aoi_test.gpkg"))


############################################################################
# old
data("aoi_data")

test.cred <- list(dhus.user = Sys.getenv("gSD_user"),
                  dhus.pass = Sys.getenv("gSD_pass"),
                  s5p.user = "s5pguest",
                  s5p.pass = "s5pguest",
                  gnss.user = "gnssguest",
                  gnss.pass = "gnssguest",
                  ee.user = Sys.getenv("gSD_user"),
                  ee.pass = Sys.getenv("gSD_pass"),
                  espa.user = Sys.getenv("gSD_user"),
                  espa.pass = Sys.getenv("gSD_pass"))

test.run <- list(authentify = if(test.cred$dhus.user == "") FALSE else TRUE, 
                 downloads = if(Sys.getenv("gSD_downtests") == "yes") TRUE else FALSE)

#if(vars.auth$dhus.user != "") runAuthTests <- TRUE else runAuthTests <- FALSE
#if(Sys.getenv("gSD_downtests") == "yes") runDownTests <- TRUE else runDownTests <- FALSE

vars.global <- list(dir.arc = tempdir(),
                    aoi = aoi_test,
                    time_range = c("2019-03-01", "2019-03-30"))

vars.sentinel <- data.frame(platforms = c("Sentinel-1", "Sentinel-2", "Sentinel-3", "Sentinel-5P"),
                            expect.prev = c(T, T, T, F), stringsAsFactors = F,
                            user = c(test.cred$dhus.user, test.cred$dhus.user, test.cred$dhus.user, test.cred$s5p.user),
                            pass = c(test.cred$dhus.pass, test.cred$dhus.pass, test.cred$dhus.pass, test.cred$s5p.pass))
if(isFALSE(test.run$authentify)) vars.sentinel <- vars.sentinel[-c(1:2),]





