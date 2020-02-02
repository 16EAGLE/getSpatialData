# TEST DIRECTORIES
# -----------------
tt <- list()
tt$home <- getwd()
tt$tmp <- file.path(tt$home, "tmp") # tmp dir that can be created for tests (and deleted!)
tt$resources$home <- file.path(tt$home, "resources")
tt$resources$records <- file.path(tt$resources$home, "records")
tt$resources$previews <- file.path(tt$resources$home, "previews")
tt$resources$cmasks <- file.path(tt$resources$home, "cloud_masks")
tt$resources$aoi <- file.path(tt$resources$home, "tunisia_aoi")
dir_error <- "Cannot run tests because directory not found: "
if (!dir.exists(tt$home)) stop(paste0(dir_error, tt$home))
if (!dir.exists(tt$resources$home)) stop(paste0(dir_error, tt$resources))
for (dir in tt$resources) if (!dir.exists(dir)) stop(paste0(dir_error, dir))

# helpers for initializing and finishing tmp dir
initialize_dir <- function(dir) {
  if (dir.exists(dir)) unlink(dir, TRUE)
  dir.create(dir)
}
finish_dir <- function(dir) {
  if (dir.exists(dir)) unlink(dir, TRUE)
}

# TEST PARAMETERS
# -----------------
# classes
DATAFRAME <- "data.frame"
NUMERIC <- "numeric"
INTEGER <- "integer"
CHARACTER <- "character"

# sensor names
SENTINEL2 <- "Sentinel-2"
SENTINEL3 <- "Sentinel-3"
LANDSAT <- "Landsat"
MODIS <- "MODIS"
MIXED <- "mixed"

# for file naming
SUFFIX <- list()
SUFFIX$records <- "records"
SUFFIX$previews <- "records_previews"
SUFFIX$cmasks <- "records_cmasks"
construct_filepath <- function(sensor, suffix) {
  return(paste(sensor, paste0(suffix, ".csv"), sep="_"))
}

# records data.frame column names
COLS <- list()
COLS$preview_jpg <- "preview_file_jpg"
COLS$preview_tif <- "preview_file"
COLS$HOT_scene <- "aoi_HOT_cloudcov_percent"
COLS$HOT_aoi <- "scene_HOT_cloudcov_percent"
COLS$cmask_tif <- "cloud_mask_file"

# TEST VARIABLES
# -----------------
aoi_tunisia <- .read_shp(file.path(tt$resources$aoi, "tunisia_aoi.shp"))
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
                    aoi = aoi_data[[1]],
                    time_range = c("2019-03-01", "2019-03-30")) #c("2017-08-01", "2017-08-30"))

vars.sentinel <- data.frame(platforms = c("Sentinel-1", "Sentinel-2", "Sentinel-3", "Sentinel-5P"),
                            expect.prev = c(T, T, T, F), stringsAsFactors = F,
                            user = c(test.cred$dhus.user, test.cred$dhus.user, test.cred$dhus.user, test.cred$s5p.user),
                            pass = c(test.cred$dhus.pass, test.cred$dhus.pass, test.cred$dhus.pass, test.cred$s5p.pass))
if(isFALSE(test.run$authentify)) vars.sentinel <- vars.sentinel[-c(1:2),]





