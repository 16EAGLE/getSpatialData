# DIRECTORIES
# -----------------------------------------------------------------------------------------
dir_records <- tt$resources$records
dir_previews <- tt$resources$previews
dir_cmasks <- tt$resources$cmasks

# TEST FUNCTIONS
# -----------------------------------------------------------------------------------------

# tests on the output with clean input
clean_test_calc_cloudcov <- function(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER) {
  
  expect_is(records_cc, DATAFRAME)
  # check column number
  expect_length(records_cc, length(records) + 5)
  cols_given <- names(records_cc)
  # check if column exists
  expect_true(COLS$HOT_aoi %in% cols_given)
  expect_true(COLS$HOT_scene %in% cols_given)
  expect_true(COLS$cmask_tif %in% cols_given)
  expect_true(COLS$preview_jpg %in% cols_given)
  expect_true(COLS$preview_tif %in% cols_given)
  # get column vector
  aoi_cc <- records_cc[[COLS$HOT_aoi]]
  scene_cc <- records_cc[[COLS$HOT_scene]]
  cmask_tifs <- records_cc[[COLS$cmask_tif]]
  preview_jpgs <- records_cc[[COLS$preview_jpg]]
  preview_tifs <- records_cc[[COLS$preview_tif]]
  # check column data type
  expect_is(aoi_cc, NUMERIC)
  expect_is(scene_cc, NUMERIC)
  expect_is(cmask_tifs, CHARACTER)
  expect_is(preview_jpgs, CHARACTER)
  expect_is(preview_tifs, CHARACTER)
  expect_true(all(aoi_cc) > 0)
  expect_true(all(scene_cc) > 0)
  # check if all files exist
  for (files in list(cmask_tifs, preview_jpgs, preview_tifs)) {
    for (file in files) {
      expect_true(file.exists(file))
    }
  }

}

# tests errors
error_test_calc_cloudcov <- function(records, aoi, tt) {
  
  expect_error(calc_cloudcov(records, aoi = "", dir_out = tt$tmp))
  expect_error(calc_cloudcov(records, aoi, dir_out = "peace & love for the world"))
  expect_error(calc_cloudcov(records, aoi, username = "Lord Voldemort"))
  expect_error(calc_cloudcov(records, aoi, password = "Nagini"))
  expect_error(calc_cloudcov(records, aoi, verbose = "Avada Kedavra"))
  
  
}

# RUN TESTS
# -----------------------
# -----------------------
## WITHOUT RELOAD
# Input: a records data.frame as received from get_records()
# -----------------------

# -----------------------------------------------------------------------------------------
# test with Sentinel-2
# provide a records csv WITHOUT previous get_previews() call
records <- read.csv(construct_filepath(SENTINEL2, SUFFIX$records))
records_cc1 <- calc_cloudcov(records, aoi_tunisia, dir_out = tt$tmp)
# provide a records csv WITH previous get_previews() call
records_previews <- read.csv(construct_filepath(SENTINEL2, SUFFIX$previews))
records_cc2 <- calc_cloudcov(records_previews, aoi_tunisia, dir_out = tt$tmp)

for (records_cc in list(records_cc1, records_cc2)) {
  initialize_dir(tt$tmp)
  clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
  finish_dir(tt$tmp)
}
# -----------------------------------------------------------------------------------------
# test with Landsat
records <- read.csv(construct_filepath(LANDSAT, SUFFIX$records))



# -----------------------------------------------------------------------------------------
# test with MODIS
records <- read.csv(construct_filepath(MODIS, SUFFIX$records))

# -----------------------------------------------------------------------------------------
# test with Sentinel-3
records <- read.csv(construct_filepath(SENTINEL3, SUFFIX$records))


# -----------------------------------------------------------------------------------------
# test all sensors mixed
records_file <- file.path(dir_records, "mixed_records.csv")


# -----------------------
## WITH RELOAD OF PREVIEW
# -----------------------
# test with Sentinel-2
# -----------------------

# -----------------------
# test with Landsat
# -----------------------

# -----------------------
# test with MODIS
# -----------------------

# -----------------------
# test with Sentinel-3
# -----------------------

# -----------------------
# test all sensors mixed

# -----------------------
## WITH RELOAD OF CSV
# -----------------------


