# -----------------------------------------------------------------------------------------
# DIRECTORIES
# -----------------------------------------------------------------------------------------

dir_records <- tt$resources$records

# -----------------------------------------------------------------------------------------
# DEFINE TEST FUNCTIONS
# -----------------------------------------------------------------------------------------

# tests on the output with clean input
# ------------------------------------
clean_test_calc_cloudcov <- function(dir_records, aoi_test, sensor, tt, PREFIX, COLS, NUMERIC, CHARACTER) {
  initialize_dir(tt$tmp)
  ADDED_COLS_CLOUDCOV <- 3
  ADDED_COLS_PREVIEWS_CLOUDCOV <- 5
  records <- read_records(construct_filepath(dir_records, sensor, PREFIX$cmasks))
  is_mixed <- sensor == "mixed"
  if (NROW(records) > 1) {
    if (is_mixed) {
      nrows <- 1:NROW(records)
    } else {
      # vary the number of records to process with from time to time, all have to work
      nrows <- 1:sample(c(1:NROW(records)), 1)
    }
  } else {
    nrows <- 1
  }
  records_previews <- set_null_cloudcov_cols(records)
  records <- set_null_preview_cols(records_previews)
  length_base_records <- length(records)
  # -------
  # no reload, return as data.frame
  records_no_reload_df <- expect_is(calc_cloudcov(records[nrows,], 
                                                  aoi = aoi_test, dir_out = tt$tmp, as_sf = FALSE), DATAFRAME)
  expect_length(names(records_no_reload_df), length_base_records + ADDED_COLS_PREVIEWS_CLOUDCOV)
  result_valid_test_calc_cloudcov(records_no_reload_df)
  # cleanup
  if (!is_mixed) {
    finish_dir(tt$tmp)
    initialize_dir(tt$tmp)
    # no reload, return as sf
    records_no_reload_sf <- expect_is(calc_cloudcov(records[nrows,],
                                                    aoi = aoi_test, dir_out = tt$tmp, as_sf = TRUE), SF)
    expect_length(names(records_no_reload_sf), length_base_records + ADDED_COLS_PREVIEWS_CLOUDCOV)
    result_valid_test_calc_cloudcov(records_no_reload_sf)
  }
  
  # -------
  # delete processed cloud masks and process with preview reload
  delete_files(records_no_reload_df[[COLS$cmask_tif]])
  # preview reload, return as sf
  records_previews <- set_null_cloudcov_cols(records_no_reload_df)
  records_preview_reload <- expect_is(calc_cloudcov(records_previews[nrows,], 
                                                          aoi = aoi_test, dir_out = tt$tmp, as_sf = TRUE), SF)
  expect_length(names(records_preview_reload), NCOL(records_previews) + ADDED_COLS_CLOUDCOV)
  result_valid_test_calc_cloudcov(records_preview_reload)
  if (!is_mixed) {
    # cleanup
    delete_files(records_no_reload_df[[COLS$cmask_tif]])
    # preview reload, return as data.frame
    records_preview_reload <- expect_is(calc_cloudcov(records_no_reload_df[nrows,], 
                                                         aoi = aoi_test, dir_out = tt$tmp, as_sf = FALSE), DATAFRAME)
    expect_length(names(records_preview_reload), NCOL(records_previews) + ADDED_COLS_CLOUDCOV)
    result_valid_test_calc_cloudcov(records_preview_reload)
  }
  
  # -------
  # delete processed record files and process with cloud mask (and preview) reload
  del <- sapply(records_no_reload_df[[getSpatialData:::name_record_id()]], function(record_id) {
    files <- list.files(tt$tmp, pattern=record_id)
    unlink(file.path(tt$tmp, files[!sapply(files, endsWith, ".tif")]))
  })
  rm(del)
  records_cloud_mask_reload <- expect_is(calc_cloudcov(records[nrows,], aoi = aoi_test, dir_out = tt$tmp, as_sf = TRUE), SF)
  expect_length(names(records_cloud_mask_reload), NCOL(records) + ADDED_COLS_PREVIEWS_CLOUDCOV)
  result_valid_test_calc_cloudcov(records_cloud_mask_reload)
  finish_dir(tt$tmp)
}

# tests errors
# ------------------------------------
error_test_calc_cloudcov <- function(records, aoi, tt) {
  # records type
  expect_error(calc_cloudcov(records = "Dumbledore", aoi = aoi, dir_out = tt$home), RECORDS_TYPE_ERROR)
  # records column missing
  needed_cols <- getSpatialData:::.cloudcov_get_needed_cols()
  for (col_remove in needed_cols) {
    input1_records <- records
    input1_records[[col_remove]] <- NULL
    expect_error(calc_cloudcov(input1_records, aoi = aoi, dir_out = tt$home), column_error_msg(col_remove))
  }
  # aoi
  expect_error(calc_cloudcov(records, aoi = "Forbidden Forest", dir_out = tt$home), AOI_TYPE_ERROR)
  expect_error(calc_cloudcov(records,aoi = NULL, dir_out = tt$home))
  # dir_out
  input1_dir_out <- 10
  error1_dir_out <- type_error_msg(input1_dir_out, "dir_out", CHARACTER)
  input2_dir_out <- "Lord Voldemort"
  error2_dir_out <- dir_out_error_msg(input2_dir_out) # dir does not exist message
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = input1_dir_out), error1_dir_out)
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = input2_dir_out), error2_dir_out)
  # username
  input_username <- 11
  error_username <- type_error_msg(input_username, "username", CHARACTER)
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = tt$home, username = input_username), error_username)
  # password
  input_password <- 12
  error_password <- type_error_msg(input_password, "password", CHARACTER)
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = tt$home, password = input_password), error_password)
  # verbose
  input_verbose = "Avada Kedavra"
  error_verbose <- type_error_msg(input_verbose, "verbose", LOGICAL)
  expect_error(calc_cloudcov(records, aoi = aoi, dir_out = tt$home, verbose = input_verbose), error_verbose)
}

# tests if cloud masks have right format and values make sense
# ------------------------------------
cloud_masks_test_calc_cloudcov <- function(records_cc) {
  row <- sample(1:NROW(records_cc), 1) # take one row but not always the same
  record <- records_cc[row,]
  cloud_mask_file <- expect_type(record[[COLS$cmask_tif]], CHARACTER)
  is_tif <- endsWith(cloud_mask_file, ".tif")
  is_none <- endsWith(cloud_mask_file, "NONE")
  cloudcov <- record[[COLS$HOT_scene]]
  aoi_cloudcov <- record[[COLS$HOT_aoi]]
  if (getSpatialData::is.sentinel1(record)) {
    expect_false(is_tif)
    expect_true(is_none)
    expect_true(is.na(cloudcov))
    expect_true(is.na(aoi_cloudcov))
  } else {
    expect_true(is_tif || is_none) # can be NONE
    cloud_mask <- getSpatialData:::.read_brick(cloud_mask_file)
    values <- values(cloud_mask)
    values <- values[!is.na(values)]
    expect_true(!is.null(values))
    expect_true(all(values <= 1))
    expect_true(all(values >= 0))
    expect_is(cloudcov, NUMERIC)
    expect_true(cloudcov >= 0)
    expect_true(cloudcov <= 100 || cloudcov == 9999)
    expect_is(aoi_cloudcov, NUMERIC)
    expect_true(aoi_cloudcov >= 0)
    expect_true(aoi_cloudcov <= 100 || (aoi_cloudcov == 9999 && is_none))
  }
}

#' tests if result of calc_cloudcov is valid
# ------------------------------------
result_valid_test_calc_cloudcov <- function(records_cc) {
  cols_given <- names(records_cc)
  # check if column exists
  expect_true(COLS$HOT_aoi %in% cols_given)
  expect_true(COLS$HOT_scene %in% cols_given)
  expect_true(COLS$cmask_tif %in% cols_given)
  expect_true(COLS$preview_jpg %in% cols_given)
  expect_true(COLS$preview_tif %in% cols_given)
  cmask_tifs <- records_cc[[COLS$cmask_tif]]
  preview_jpgs <- records_cc[[COLS$preview_jpg]]
  preview_tifs <- records_cc[[COLS$preview_tif]]
  # check column data type
  expect_is(cmask_tifs, CHARACTER)
  expect_is(preview_jpgs, CHARACTER)
  expect_is(preview_tifs, CHARACTER)
  # check cloud mask rasters
  cloud_masks_test_calc_cloudcov(records_cc)
}

#' helper for deleting vector of files
delete_files <- function(files) {
  del <- sapply(files, function(cloud_mask_file) {
    unlink(cloud_mask_file)
  })
  rm(del)
}

# -----------------------------------------------------------------------------------------
# RUN TESTS
# -----------------------------------------------------------------------------------------

# TEST 1
# -------
# Target: Test errors (sensor does not matter)
initialize_dir(tt$tmp)
records <- read_records(construct_filepath(dir_records, SENTINEL2, PREFIX$cmasks))
# with false input
error_test_calc_cloudcov(records, aoi = aoi_test, tt)
# calc_hot_cloudcov() with modified preview
record_cloud_mask <- read_records(construct_filepath(dir_records, SENTINEL2, PREFIX$cmasks), as_sf = FALSE)[1,] # one line
record_preview <- set_null_cloudcov_cols(record_cloud_mask)
record <- set_null_preview_cols(record_cloud_mask)
record_cloud_mask <- getSpatialData:::calc_cloudcov(record, aoi = aoi_test, dir_out = tt$tmp)
unlink(record_cloud_mask$cloud_mask_file)
record_preview <- set_null_cloudcov_cols(record_cloud_mask)
preview <- getSpatialData:::.read_brick(record_preview$preview_file)
input_preview1 <- getSpatialData:::.subset_brick(preview) # blue red band preview (should not work)
expect_error(getSpatialData:::calc_hot_cloudcov(record_preview, input_preview1, 
                                                aoi = aoi_test, dir_out = tt$tmp))
finish_dir(tt$tmp)

# TEST 2
# -------
# Target: Test with Sentinel-1
clean_test_calc_cloudcov(dir_records, aoi_test, SENTINEL1, tt, PREFIX, COLS, NUMERIC, CHARACTER)

# TEST 3
# -------
# Target: Test with Sentinel-2
clean_test_calc_cloudcov(dir_records, aoi_test, SENTINEL2, tt, PREFIX, COLS, NUMERIC, CHARACTER)

# TEST 4
# -------
# Target: Test with Sentinel-3
clean_test_calc_cloudcov(dir_records, aoi_test, SENTINEL3, tt, PREFIX, COLS, NUMERIC, CHARACTER)

# TEST 5
# -------
# Target: Test with Landsat
clean_test_calc_cloudcov(dir_records, aoi_test, LANDSAT, tt, PREFIX, COLS, NUMERIC, CHARACTER)

# TEST 6
# -------
# Target: Test with MODIS
clean_test_calc_cloudcov(dir_records, aoi_test, MODIS, tt, PREFIX, COLS, NUMERIC, CHARACTER)

# TEST 7
# -------
# Target: Test mixed sensors including SAR
clean_test_calc_cloudcov(dir_records, aoi_test, MIXED, tt, PREFIX, COLS, NUMERIC, CHARACTER)

# -----------------------------------------------------------------------------------------
