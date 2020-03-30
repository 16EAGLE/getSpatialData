# -----------------------------------------------------------------------------------------
# DIRECTORIES
# -----------------------------------------------------------------------------------------

dir_records <- tt$resources$records
ADDED_COLS_PREVIEWS <- 5
ADDED_COLS_WITHOUT_PREVIEWS <- 3

# -----------------------------------------------------------------------------------------
# DEFINE TEST FUNCTIONS
# -----------------------------------------------------------------------------------------

# tests on the output with clean input
# ------------------------------------
clean_test_calc_cloudcov <- function(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER) {
  
  cols_given <- names(records_cc)
  # check if column exists
  expect_true(COLS$HOT_aoi %in% cols_given)
  expect_true(COLS$HOT_scene %in% cols_given)
  expect_true(COLS$cmask_tif %in% cols_given)
  expect_true(COLS$preview_jpg %in% cols_given)
  expect_true(COLS$preview_tif %in% cols_given)
  # get column vectors
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
# ------------------------------------
error_test_calc_cloudcov <- function(records, aoi, tt) {
  
  # records type
  expect_error(calc_cloudcov(records = "Dumbledore", aoi = aoi, dir_out = tt$tmp), RECORDS_TYPE_ERROR)
  # records column missing
  needed_cols <- getSpatialData:::.get_needed_cols_calc_cloudcov()
  for (col_remove in needed_cols) {
    input1_records <- records
    input1_records[[col_remove]] <- NULL
    expect_error(calc_cloudcov(input1_records, aoi = aoi, dir_out = tt$tmp), column_error_msg(col_remove))
  }
  # aoi
  expect_error(calc_cloudcov(records, aoi = "Forbidden Forest", dir_out = tt$tmp), AOI_TYPE_ERROR)
  expect_error(calc_cloudcov(records,aoi = NULL, dir_out = tt$tmp), AOI_UNDEFINED_ERROR)
  # dir_out
  input1_dir_out <- 10
  error1_dir_out <- type_error_msg(input1_dir_out, "dir_out", CHARACTER)
  input2_dir_out <- "Lord Voldemort"
  error2_dir_out <- dir_out_error_msg(input2_dir_out) # dir does not exist message
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = input1_dir_out), error1_dir_out)
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = input2_dir_out), error2_dir_out)
  # username
  input_username <- 11
  error_username <- type_error_msg(input_username, "password", CHARACTER)
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = tt$tmp, username = input_username), error_username)
  # password
  input_password <- 12
  error_password <- type_error_msg(input_password, "username", CHARACTER)
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = tt$tmp, password = input_password), error_password)
  # verbose
  input_verbose = "Avada Kedavra"
  error_verbose <- type_error_msg(input_verbose, "verbose", LOGICAL)
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = tt$tmp, verbose = input_verbose), error_verbose)
  
}

# -----------------------------------------------------------------------------------------
# RUN TESTS
# -----------------------------------------------------------------------------------------

# ------------------------------------
# WITHOUT RELOAD
# Input: a records data.frame as received from get_records() 
# Specification: NO reload of previously processed records
# ------------------------------------

# TEST 1
# -------
# test errors
# Target: Test errors (sensor does not matter)
records <- read.csv(construct_filepath(dir_records, SENTINEL2, SUFFIX$records))
# with false input
error_test_calc_cloudcov(records, aoi = aoi_tunisia, tt)
# calc_hot_cloudcov() with modified preview
initialize_dir(tt$tmp)
record_preview <- read.csv(construct_filepath(dir_records, SENTINEL2, SUFFIX$previews))[1,] # one line
preview <- getSpatialData:::.read_brick(record_preview$preview_file)
input_preview1 <- getSpatialData:::.subset_brick(preview) # blue red band preview (enough, should work)
input_preview2 <- preview[[1]] # single band preview (should not work)
expect_is(getSpatialData:::calc_hot_cloudcov(record_preview, input_preview1, 
                                                aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
error_preview2 <- paste0("RGB (3 layers) or RB (2 layers) image stack has to be provided as 'preview'. 
               Number of layers of the given stack is: 1.\nHOT could not be calculated for
               record: ", record_preview$record_id)
expect_error(getSpatialData:::calc_hot_cloudcov(record_preview, input_preview2, aoi = aoi_tunisia, 
                                                dir_out = tt$tmp), error_preview2)
finish_dir(tt$tmp)

# TEST 2
# -------
# Target: Test with Sentinel-2
# provide a records csv WITHOUT previous get_previews() call
records <- read.csv(construct_filepath(dir_records, SENTINEL2, SUFFIX$records))
records_cc1 <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
# provide a records csv WITH previous get_previews() call
records_previews <- read.csv(construct_filepath(dir_records, SENTINEL2, SUFFIX$previews))
records_cc2 <- expect_is(calc_cloudcov(records_previews, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
expect_length(names(records_cc1), length(records) + ADDED_COLS_PREVIEWS)
expect_length(names(records_cc2), length(records) + ADDED_COLS_WITHOUT_PREVIEWS)
for (records_cc in list(records_cc1, records_cc2)) {
  initialize_dir(tt$tmp)
  clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
  finish_dir(tt$tmp)
}

# TEST 3
# -------
# Target: Test with Landsat
records <- read.csv(construct_filepath(dir_records, LANDSAT, SUFFIX$records))
records_previews <- read.csv(construct_filepath(dir_records, LANDSAT, SUFFIX$previews))
records_cc1 <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
records_cc2 <- expect_is(calc_cloudcov(records_previews, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
expect_length(names(records_cc1), length(records) + ADDED_COLS_PREVIEWS)
expect_length(names(records_cc2), length(records) + ADDED_COLS_WITHOUT_PREVIEWS)

for (records_cc in list(records_cc1, records_cc2)) {
  initialize_dir(tt$tmp)
  clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
  finish_dir(tt$tmp)
}

# TEST 4
# -------
# Target: Test with MODIS
records <- read.csv(construct_filepath(dir_records, MODIS, SUFFIX$records))
records_previews <- read.csv(construct_filepath(dir_records, MODIS, SUFFIX$previews))
records_cc1 <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
records_cc2 <- expect_is(calc_cloudcov(records_previews, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
expect_length(names(records_cc1), length(records) + ADDED_COLS_PREVIEWS)
expect_length(names(records_cc2), length(records) + ADDED_COLS_WITHOUT_PREVIEWS)
for (records_cc in list(records_cc1, records_cc2)) {
  initialize_dir(tt$tmp)
  clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
  finish_dir(tt$tmp)
}

# TEST 5
# -------
# Target: Test with Sentinel-3
records <- read.csv(construct_filepath(dir_records, SENTINEL3, SUFFIX$records))
records_previews <- read.csv(construct_filepath(dir_records, SENTINEL3, SUFFIX$previews))
records_cc1 <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
records_cc2 <- expect_is(calc_cloudcov(records_previews, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
expect_length(names(records_cc1), length(records) + ADDED_COLS_PREVIEWS)
expect_length(names(records_cc2), length(records) + ADDED_COLS_WITHOUT_PREVIEWS)
for (records_cc in list(records_cc1, records_cc2)) {
  initialize_dir(tt$tmp)
  clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
  finish_dir(tt$tmp)
}

# TEST 6
# -------
# Target: Test sensors mixed sensors including SAR
records <- read.csv(construct_filepath(dir_records, MIXED, SUFFIX$records))
records_previews <- read.csv(construct_filepath(dir_records, MIXED, SUFFIX$previews))
records_cc1 <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
records_cc2 <- expect_is(calc_cloudcov(records_previews, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
expect_length(names(records_cc1), length(records) + ADDED_COLS_PREVIEWS)
expect_length(names(records_cc2), length(records) + ADDED_COLS_WITHOUT_PREVIEWS)
for (records_cc in list(records_cc1, records_cc2)) {
  initialize_dir(tt$tmp)
  clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
  finish_dir(tt$tmp)
}

# ------------------------------------
# WITH RELOAD
# Specification: Reload of previously processed records 
#                 1. reload of single line record from previously processed csv
#                 2. reload of previously processed cloud mask
# Explanation: calc_cloudcov writes each processed csv row (with added columns) to a single csv
# in dir_out in order to facilitate reload in calc_cloud. The cloud masks are written to dir_out as well
# and if existing reloaded in calc_hot_cloudcov. In calc_cloudcov it is also checked if the cloud mask
# already exists in case the csv row exists. If the cloud mask is not in dir_out calc_hot_cloudcov is called.
# Otherwise, processing is finished for the given record (row).
# ------------------------------------

# TEST 7
# -------
# Target: Test with Sentinel-2 and reload
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, SENTINEL2, SUFFIX$records))
# process single row
record_row <- expect_is(calc_cloudcov(records[1,], aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
# process all rows, now including reload of one record (row csv and cloud mask)
records_cc <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
# delete cloud mask and re-run with existing csv row but non-existing cloud mask
unlink(record_row[[COLS$cmask_tif]])
records_cc <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
finish_dir(tt$tmp)

# TEST 8
# -------
# Target: Test with Landsat and reload
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, LANDSAT, SUFFIX$records))
# process single row
record_row <- expect_is(calc_cloudcov(records[1,], aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
# process all rows, now including reload of one record (row csv and cloud mask)
records_cc <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
# delete cloud mask and re-run with existing csv row but non-existing cloud mask
unlink(record_row[[COLS$cmask_tif]])
records_cc <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
finish_dir(tt$tmp)

# TEST 9
# -------
# Target: Test with MODIS and reload
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, MODIS, SUFFIX$records))
# process single row
record_row <- expect_is(calc_cloudcov(records[1,], aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
# process all rows, now including reload of one record (row csv and cloud mask)
records_cc <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
# delete cloud mask and re-run with existing csv row but non-existing cloud mask
unlink(record_row[[COLS$cmask_tif]])
records_cc <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
finish_dir(tt$tmp)

# TEST 10
# -------
# Target: Test with Sentinel-3 and reload
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, SENTINEL3, SUFFIX$records))
# process single row
record_row <- expect_is(calc_cloudcov(records[1,], aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
# process all rows, now including reload of one record (row csv and cloud mask)
records_cc <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
# delete cloud mask and re-run with existing csv row but non-existing cloud mask
unlink(record_row[[COLS$cmask_tif]])
records_cc <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
finish_dir(tt$tmp)

# TEST 11
# -------
# Target: Test with mixed sensors and reload
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, MIXED, SUFFIX$records))
# process single row
expect_is(calc_cloudcov(records[1,], aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME)
# process all rows, now including reload of one record (row csv and cloud mask)
records_cc <- expect_is(calc_cloudcov(records, aoi = aoi_tunisia, dir_out = tt$tmp), DATAFRAME) 
clean_test_calc_cloudcov(records_cc, COLS, DATAFRAME, NUMERIC, CHARACTER)
finish_dir(tt$tmp)
