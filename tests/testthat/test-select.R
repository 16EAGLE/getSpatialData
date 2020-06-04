# -----------------------------------------------------------------------------------------
# DIRECTORIES
# -----------------------------------------------------------------------------------------

dir_records <- tt$resources$records
prio_products <- c(getSpatialData:::name_product_sentinel2(), getSpatialData:::name_product_landsat8(), 
                   getSpatialData:::name_product_landsat7(), getSpatialData:::name_product_landsat5(),
                   getSpatialData:::name_product_landsatmss(), getSpatialData:::name_product_sentinel3(),
                   getSpatialData:::name_product_group_modis())
max_sub_period <- 10
min_distance <- 5
n_timestamps <- 3

# -----------------------------------------------------------------------------------------
# DEFINE TEST FUNCTIONS
# -----------------------------------------------------------------------------------------

# main test function
# ------------------------------------
clean_test_select <- function(dir_records, aoi_test, sensor, prio_products, tt, PREFIX, COLS, DATAFRAME, NUMERIC, CHARACTER) {
  initialize_dir(tt$tmp)
  modes <- c("select_unitemporal", "select_bitemporal", "select_timeseries")
  max_sub_period <- 10
  min_distance <- 5
  n_timestamps <- 3
  max_cloudcov_tile <- 80
  records_read <- read_records(construct_filepath(dir_records, sensor, PREFIX$cmasks))

  # calculate cloud masks
  records <- calc_cloudcov(records_read, aoi = aoi_test, dir_out = tt$tmp)
  # delete a cloud mask and test error
  unlink(records$cloud_mask_file[1])
  expect_error(select_unitemporal(records,
                                  max_cloudcov_tile = max_cloudcov_tile,
                                  max_sub_period = max_sub_period,
                                  aoi = aoi_test, dir_out = tt$tmp))
  unlink(records$preview_file[1])
  expect_error(select_unitemporal(records,
                                  max_cloudcov_tile = max_cloudcov_tile,
                                  max_sub_period = max_sub_period,
                                  aoi = aoi_test, dir_out = tt$tmp))
  records <- calc_cloudcov(records_read, aoi = aoi_test, dir_out = tt$tmp)
  
  for (mode in modes) {
    is_unitemporal <- grepl("unitemporal", mode)
    is_bitemporal <- grepl("bitemporal", mode)
    is_timeseries <- grepl("timeseries", mode)
    as_sf <- sample(c(TRUE, FALSE), 1)
    expected_class <- ifelse(as_sf, SF, DATAFRAME)
    if (is_unitemporal) {
      test_unitemporal_without_writing(records, expected_class, max_cloudcov_tile, as_sf, tt, COLS, aoi_test)
      records_unitemporal <- expect_is(select_unitemporal(records,
                                                          max_cloudcov_tile = max_cloudcov_tile,
                                                          max_sub_period = max_sub_period,
                                                          aoi = aoi_test, dir_out = tt$tmp, as_sf = as_sf), expected_class)
      records_select <- records_unitemporal
    } else if (is_bitemporal) {
      test_bitemporal_without_writing(records, expected_class, max_cloudcov_tile, as_sf, tt, COLS, aoi_test)
      records_bitemporal <- expect_is(select_bitemporal(records, 
                                                        max_cloudcov_tile = max_cloudcov_tile,
                                                        max_sub_period = max_sub_period,
                                                        min_distance = min_distance,
                                                        aoi = aoi_test, dir_out = tt$tmp, as_sf = as_sf), expected_class)
      records_select <- records_bitemporal
    } else if (is_timeseries) {
      test_timeseries_without_writing(records, expected_class, max_cloudcov_tile, as_sf, tt, COLS, aoi_test)
      records_timeseries <- expect_is(select_timeseries(records,
                                                        n_timestamps = n_timestamps,
                                                        max_cloudcov_tile = max_cloudcov_tile,
                                                        max_sub_period = max_sub_period,
                                                        min_distance = min_distance,
                                                        aoi = aoi_test, dir_out = tt$tmp, as_sf = as_sf), expected_class)
      records_select <- records_timeseries
    }
  
    cols_given <- names(records_select)
    # check if column exists
    expect_true(COLS$pmos_col %in% cols_given)
    expect_true(COLS$cmos_col %in% cols_given)
    expect_true(COLS$timestamp_col %in% cols_given)
    # get column vectors
    cmos_col <- records_select[[COLS$cmos_col]]
    pmos_col <- records_select[[COLS$pmos_col]]
    timestamp_col <- records_select[[COLS$timestamp_col]]
    # check column data type
    expect_is(pmos_col, CHARACTER)
    expect_is(cmos_col, CHARACTER)
    expect_true(any(inherits(timestamp_col, NUMERIC), inherits(timestamp_col, INTEGER)))
    # check rasters
    for (file in cmos_col) {
      if (!is.na(file)) { #  can be
        expect_true(file.exists(file))
        loaded_cmos <- test_raster_read(file) 
        expect_false(is.na(crs(loaded_cmos)) && is.na(st_crs(loaded_cmos)))
        # values
        expect_equal(minValue(loaded_cmos), 1) # not 0!
        expect_equal(maxValue(loaded_cmos), 1)
      }
    }
    for (file in pmos_col) {
      if (!is.na(file)) { # can be
        expect_true(file.exists(file))
        loaded_pmos <- test_stack_read(file)
        expect_false(is.na(crs(loaded_pmos)) && is.na(st_crs(loaded_pmos)))
      }
    }
    is_selected <- !is.na(timestamp_col)
    # check if max_cloudcov_tile is fulfilled
    cloudcov <- records_select[is_selected,][[getSpatialData:::name_cloudcov()]]
    cloudcov_filtered <- as.numeric(cloudcov[!is.na(cloudcov)])
    if (length(cloudcov_filtered) > 0) {
      expect_true(all(cloudcov_filtered < max_cloudcov_tile))
    }
    # check if temporal args are fulfilled
    timestamp_col_filtered <- timestamp_col[is_selected]
    dates <- sapply(records_select[is_selected,][[getSpatialData:::name_date_acquisition()]], as.Date)
    if (is_unitemporal) {
      expect_true(unique(timestamp_col_filtered) == 1)
      if (length(dates) > 1) {
        start_date <- min(dates)
        end_date <- max(dates)
        sub_period_length <- end_date - start_date + 1
        expect_true(sub_period_length <= max_sub_period)
      }
    } else if (is_bitemporal) {
      expect_true(all(timestamp_col_filtered < 3))
    }
    
    if (is_bitemporal || is_timeseries) {
      previous_last_date <- 0
      for (ts in sort(unique(timestamp_col_filtered))) {
        match <- which(records_select[[COLS$timestamp_col]] == ts)
        dates_matched <- as.Date(records_select[match,][[getSpatialData:::name_date_acquisition()]])
        start_date <- min(dates_matched)
        end_date <- max(dates_matched)
        sub_period_length <- as.numeric(as.Date(end_date) - as.Date(start_date)) + 1
        expect_true(sub_period_length <= max_sub_period)
        if (ts > 1) {
          expect_true((start_date - previous_last_date) > min_distance)
        }
        previous_last_date <- end_date
      }
    }

  }
  finish_dir(tt$tmp)
}

# test without writing preview mosaic respectively cloud mask mosaic
validate_output <- function(records, column_name_given, column_name_not_given) {
  names <- names(records)
  expect_false(column_name_not_given %in% names)
  expect_true(column_name_given %in% names)
  not_na <- !is.na(records[[column_name_given]])
  if (any(not_na)) {
    expect_true(all(file.exists(records[[column_name_given]][which(not_na)])))
  }
}
# unitemporal
test_unitemporal_without_writing <- function(records, expected_class, max_cloudcov_tile, as_sf, tt, COLS, aoi_test) {
  records_without_cmos <- expect_is(select_unitemporal(records,
                                                       max_cloudcov_tile = max_cloudcov_tile,
                                                       max_sub_period = 25,
                                                       aoi = aoi_test, 
                                                       write_cmask_mosaic = FALSE,
                                                       dir_out = tt$tmp, as_sf = as_sf), expected_class)
  validate_output(records_without_cmos, COLS$pmos_col, COLS$cmos_col)
  records_without_pmos <- expect_is(select_unitemporal(records,
                                                       max_cloudcov_tile = max_cloudcov_tile,
                                                       max_sub_period = 25,
                                                       aoi = aoi_test, 
                                                       write_preview_mosaic = FALSE,
                                                       dir_out = tt$tmp, as_sf = as_sf), expected_class)
  validate_output(records_without_pmos, COLS$cmos_col, COLS$pmos_col)
} 
# bitemporal
test_bitemporal_without_writing <- function(records, expected_class, max_cloudcov_tile, as_sf, tt, COLS, aoi_test) {
  records_without_cmos <- expect_is(select_bitemporal(records,
                                                      min_distance = 5,
                                                      max_cloudcov_tile = max_cloudcov_tile,
                                                      max_sub_period = 25,
                                                      aoi = aoi_test, 
                                                      write_cmask_mosaic = FALSE,
                                                      dir_out = tt$tmp, as_sf = as_sf), expected_class)
  validate_output(records_without_cmos, COLS$pmos_col, COLS$cmos_col)
  records_without_pmos <- expect_is(select_bitemporal(records,
                                                      min_distance = 5,
                                                      max_cloudcov_tile = max_cloudcov_tile,
                                                      max_sub_period = 25,
                                                      aoi = aoi_test, 
                                                      write_preview_mosaic = FALSE,
                                                      dir_out = tt$tmp, as_sf = as_sf), expected_class)
  validate_output(records_without_pmos, COLS$cmos_col, COLS$pmos_col)
} 
# timeseries
test_timeseries_without_writing <- function(records, expected_class, max_cloudcov_tile, as_sf, tt, COLS, aoi_test) {
  records_without_cmos <- expect_is(select_timeseries(records,
                                                      n_timestamps = 3,
                                                      min_distance = 5,
                                                      max_cloudcov_tile = max_cloudcov_tile,
                                                      max_sub_period = 25,
                                                      aoi = aoi_test, 
                                                      write_cmask_mosaic = FALSE,
                                                      dir_out = tt$tmp, as_sf = as_sf), expected_class)
  validate_output(records_without_cmos, COLS$pmos_col, COLS$cmos_col)
  records_without_pmos <- expect_is(select_timeseries(records,
                                                      n_timestamps = 3,
                                                      min_distance = 5,
                                                      max_cloudcov_tile = max_cloudcov_tile,
                                                      max_sub_period = 25,
                                                      aoi = aoi_test, 
                                                      write_preview_mosaic = FALSE,
                                                      dir_out = tt$tmp, as_sf = as_sf), expected_class)
  validate_output(records_without_pmos, COLS$cmos_col, COLS$pmos_col)
} 

# tests errors
# ------------------------------------
error_test_select <- function(records, aoi, tt) {
  
  # records type
  flawed_records <- "Dumbledore"
  max_sub <- 10
  n_timestamps = 3
  min_dist = 6
  expect_error(select_unitemporal(records = flawed_records, max_sub_period = max_sub, 
                                  aoi = aoi, dir_out = tt$tmp), RECORDS_TYPE_ERROR)
  expect_error(select_bitemporal(records = flawed_records, max_sub_period = max_sub, min_distance = min_dist,
                                 aoi = aoi, dir_out = tt$tmp), RECORDS_TYPE_ERROR)
  expect_error(select_timeseries(records = flawed_records, n_timestamps = n_timestamps, max_sub_period = max_sub, 
                                 min_distance = min_dist, aoi = aoi, dir_out = tt$tmp), RECORDS_TYPE_ERROR)
  # records column missing
  needed_cols <- getSpatialData:::.get_needed_cols_select()
  for (col_remove in needed_cols) {
    input1_records <- records
    input1_records[[col_remove]] <- NULL
    expect_error(select_unitemporal(input1_records, 
                                    max_sub_period = max_sub, 
                                    aoi = aoi, dir_out = tt$tmp), column_error_msg(col_remove))
    expect_error(select_bitemporal(input1_records, 
                                   max_sub_period = max_sub, 
                                   min_distance = min_dist, 
                                   aoi = aoi, dir_out = tt$tmp), column_error_msg(col_remove))
    expect_error(select_timeseries(input1_records, 
                                   n_timestamps = n_timestamps, 
                                   max_sub_period = max_sub, 
                                   min_distance = min_dist, 
                                   aoi = aoi, dir_out = tt$tmp), column_error_msg(col_remove))
  }
  # max_sub_period
  max_sub_period_name <- "max_sub_period"
  input1_max_sub_period <- "Nagini"
  error1_max_sub_period <- type_error_msg(input1_max_sub_period, max_sub_period_name, NUMERIC)
  expect_error(select_unitemporal(records, max_sub_period = input1_max_sub_period, 
                                  aoi = aoi, dir_out = tt$tmp), error1_max_sub_period) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = input1_max_sub_period,
                                 aoi = aoi, dir_out = tt$tmp), error1_max_sub_period) # BT
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = input1_max_sub_period,
                                 aoi = aoi, dir_out = tt$tmp), error1_max_sub_period) # TS
  
  # min_distance
  min_distance_name <- "min_distance"
  input1_min_distance <- "Malfoy"
  error1_min_distance <- type_error_msg(input1_min_distance, min_distance_name, NUMERIC)
  expect_error(select_bitemporal(records, min_distance = input1_min_distance, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = tt$tmp), error1_min_distance) # BT
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = input1_min_distance, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = tt$tmp), error1_min_distance) # TS
  
  # n_timestamps
  num_timestamps_name <- "n_timestamps"
  input1_num_timestamps <- "Lucius"
  error1_num_timestamps <- type_error_msg(input1_num_timestamps, num_timestamps_name, NUMERIC)
  expect_error(select_timeseries(records, n_timestamps = input1_num_timestamps, 
                                 min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = tt$tmp), error1_num_timestamps) # TS
  
  # min_improvement
  min_improvement_name <- "min_improvement"
  input1_min_improvement <- "Dolores"
  error1_min_improvment <- type_error_msg(input1_min_improvement, min_improvement_name, NUMERIC)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                 min_improvement = input1_min_improvement,
                                 aoi = aoi, dir_out = tt$tmp), error1_min_improvment) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 min_improvement = input1_min_improvement,
                                 aoi = aoi, dir_out = tt$tmp), error1_min_improvment) # BT
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 min_improvement = input1_min_improvement,
                                 aoi = aoi, dir_out = tt$tmp), error1_min_improvment) # TS
  
  # max_cloudcov_tile
  max_cloudcov_tile_name <- "max_cloudcov_tile"
  input1_max_cloudcov_tile <- "Umbridge"
  error1_max_cloudcov_tile <- type_error_msg(input1_max_cloudcov_tile, max_cloudcov_tile_name, NUMERIC)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  max_cloudcov_tile = input1_max_cloudcov_tile,
                                  aoi = aoi, dir_out = tt$tmp), error1_max_cloudcov_tile) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 max_cloudcov_tile = input1_max_cloudcov_tile,
                                 aoi = aoi, dir_out = tt$tmp), error1_max_cloudcov_tile) # BT
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 max_cloudcov_tile = input1_max_cloudcov_tile,
                                 aoi = aoi, dir_out = tt$tmp), error1_max_cloudcov_tile) # TS
  
  # satisfaction_value
  satisfaction_value_name <- "satisfaction_value"
  input1_satisfaction_value <- "Wormtail"
  error1_satisfaction_value <- type_error_msg(input1_satisfaction_value, satisfaction_value_name, NUMERIC)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  satisfaction_value = input1_satisfaction_value,
                                  aoi = aoi, dir_out = tt$tmp), error1_satisfaction_value) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 satisfaction_value = input1_satisfaction_value,
                                 aoi = aoi, dir_out = tt$tmp), error1_satisfaction_value) # BT
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 satisfaction_value = input1_satisfaction_value,
                                 aoi = aoi, dir_out = tt$tmp), error1_satisfaction_value) # TS
  
  # prio_products
  prio_products_name <- "prio_products"
  input1_prio_products <- c(100, 200)
  error1_prio_products <- type_error_msg(input1_prio_products, prio_products_name, CHARACTER)
  input2_prio_products <- c("Sentinel-2", "Kedavra")
  error2_prio_products <- "Argument 'prio_products' has to be provided with sensor names in the same format as returned by get_select_supported()"
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  prio_products = input1_prio_products,
                                  aoi = aoi, dir_out = tt$tmp), error1_prio_products) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 prio_products = input1_prio_products,
                                 aoi = aoi, dir_out = tt$tmp), error1_prio_products)
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 prio_products = input1_prio_products,
                                 aoi = aoi, dir_out = tt$tmp), error1_prio_products)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  prio_products = input2_prio_products,
                                  aoi = aoi, dir_out = tt$tmp), error2_prio_products) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 prio_products = input2_prio_products,
                                 aoi = aoi, dir_out = tt$tmp), error2_prio_products)
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 prio_products = input2_prio_products,
                                 aoi = aoi, dir_out = tt$tmp), error2_prio_products)
  
  # aoi
  input1_aoi <- NULL
  input2_aoi <- "Crucio"
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = input1_aoi, dir_out = tt$tmp)) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = input1_aoi, dir_out = tt$tmp))
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = input1_aoi, dir_out = tt$tmp))
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = input2_aoi, dir_out = tt$tmp)) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = input2_aoi, dir_out = tt$tmp))
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = input2_aoi, dir_out = tt$tmp))
  
  # dir_out
  input1_dir_out <- 10
  error1_dir_out <- type_error_msg(input1_dir_out, "dir_out", CHARACTER)
  input2_dir_out <- "Quirrell"
  error2_dir_out <- dir_out_error_msg(input2_dir_out)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = aoi, dir_out = input1_dir_out), error1_dir_out) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = input1_dir_out), error1_dir_out)
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = input1_dir_out), error1_dir_out)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = aoi, dir_out = input2_dir_out), error2_dir_out) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = input2_dir_out), error2_dir_out)
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = input2_dir_out), error2_dir_out)
  
  # verbose
  input1_verbose <- "of course"
  error1_verbose <- type_error_msg(input1_verbose, "verbose", LOGICAL)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = aoi, dir_out = tt$tmp, verbose = input1_verbose), error1_verbose) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = tt$tmp, verbose = input1_verbose), error1_verbose)
  expect_error(select_timeseries(records, n_timestamps = n_timestamps, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = tt$tmp, verbose = input1_verbose), error1_verbose)
}

# -----------------------------------------------------------------------------------------
# RUN TESTS
# -----------------------------------------------------------------------------------------

# errors
# ------------------------------------
# TEST 1
# -------
# Target: Test errors
records <- read_records(construct_filepath(dir_records, SENTINEL2, PREFIX$cmasks))
# with false input
error_test_select(records, aoi = aoi_test, tt)

# clean input
# ------------------------------------
# TEST 2
# -------
# Target: test with Sentinel-1
initialize_dir(tt$tmp)
records <- read_records(construct_filepath(dir_records, SENTINEL1, PREFIX$cmasks))
# Target: test unitemporal with Sentinel-1
records_select <- select_unitemporal(records, max_sub_period = max_sub_period, aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test bitemporal with Sentinel-1
records_select <- select_bitemporal(records, min_distance = min_distance, max_sub_period = max_sub_period, aoi = aoi_test,, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test timeseries with Sentinel-1
records_select <- select_timeseries(records,
                                    n_timestamps = n_timestamps, 
                                    min_distance = min_distance, 
                                    max_sub_period = max_sub_period, 
                                    aoi = aoi_test,
                                    dir_out = tt$tmp)
finish_dir(tt$tmp)

# TEST 3
# -------
# Target: test with Sentinel-2
clean_test_select(dir_records, aoi_test = aoi_test, sensor = SENTINEL2, 
                  prio_products = NULL, tt = tt, PREFIX = PREFIX, COLS = COLS,
                  DATAFRAME = DATAFRAME, NUMERIC = NUMERIC, CHARACTER = CHARACTER)

# TEST 4
# -------
# Target: test with Sentinel-3
clean_test_select(dir_records, aoi_test = aoi_test, sensor = SENTINEL3, 
                  prio_products = NULL, tt = tt, PREFIX = PREFIX, COLS = COLS,
                  DATAFRAME = DATAFRAME, NUMERIC = NUMERIC, CHARACTER = CHARACTER)

# TEST 5
# -------
# Target: test with Landsat
clean_test_select(dir_records, aoi_test = aoi_test, sensor = LANDSAT, 
                  prio_products = NULL, tt = tt, PREFIX = PREFIX, COLS = COLS,
                  DATAFRAME = DATAFRAME, NUMERIC = NUMERIC, CHARACTER = CHARACTER)

# TEST 6
# -------
# Target: test with MODIS
clean_test_select(dir_records, aoi_test = aoi_test, sensor = MODIS, 
                  prio_products = NULL, tt = tt, PREFIX = PREFIX, COLS = COLS,
                  DATAFRAME = DATAFRAME, NUMERIC = NUMERIC, CHARACTER = CHARACTER)

# TEST 7
# -------
# Target: test with mixed sensors without prio_products
clean_test_select(dir_records, aoi_test = aoi_test, sensor = MIXED, 
                  prio_products = NULL, tt = tt, PREFIX = PREFIX, COLS = COLS,
                  DATAFRAME = DATAFRAME, NUMERIC = NUMERIC, CHARACTER = CHARACTER)

# TEST 8
# -------
# Target: test with mixed sensors with prio_products
prio_products <- c(getSpatialData:::name_product_sentinel2(), getSpatialData:::name_product_landsat())
clean_test_select(dir_records, aoi_test = aoi_test, sensor = MIXED, 
                  prio_products = prio_products, tt = tt, PREFIX = PREFIX, COLS = COLS,
                  DATAFRAME = DATAFRAME, NUMERIC = NUMERIC, CHARACTER = CHARACTER)

# TEST 9
# -------
# Target: test with mixed sensors with prio_products and Sentinel-1
prio_products <- c(getSpatialData:::name_product_sentinel2(), getSpatialData:::name_product_landsat())
clean_test_select(dir_records, aoi_test = aoi_test, sensor = MIXED, 
                  prio_products = prio_products, tt = tt, PREFIX = PREFIX, COLS = COLS,
                  DATAFRAME = DATAFRAME, NUMERIC = NUMERIC, CHARACTER = CHARACTER)