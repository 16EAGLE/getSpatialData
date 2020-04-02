# -----------------------------------------------------------------------------------------
# DIRECTORIES
# -----------------------------------------------------------------------------------------

dir_records <- tt$resources$records
prio_sensors <- c("Sentinel-2", "LANDSAT_8_C1", "LANDSAT_ETM_C1", "LANDSAT_TM_C1", "LANDSAT_MSS_C1",
                  "Sentinel-3", "MODIS")
max_sub_period <- 10
min_distance <- 5
num_timestamps <- 3

# -----------------------------------------------------------------------------------------
# DEFINE TEST FUNCTIONS
# -----------------------------------------------------------------------------------------

# this is the main test function.
# records_select is the output of a select function with clean input
# ------------------------------------
clean_test_select <- function(records_select, COLS) {
  
  DATAFRAME = "data.frame"
  CHARACTER = "character"
  NUMERIC = "numeric"
  
  expect_is(records_select, DATAFRAME)
  cols_given <- names(records_select)
  # check if column exists
  expect_true(COLS$pmos_col %in% cols_given)
  expect_true(COLS$cmos_col %in% cols_given)
  expect_true(COLS$timestamp_col %in% cols_given)
  expect_true(COLS$sub_period_col %in% cols_given)
  # get column vectors
  pmos_col <- records_cc[[COLS$pmos_col]]
  cmos_col <- records_cc[[COLS$cmos_col]]
  timestamp_col <- records_cc[[COLS$timestamp_col]]
  sub_period_col <- records_cc[[COLS$sub_period_col]]
  # check column data type
  expect_is(pmos_col, CHARACTER)
  expect_is(cmos_col, CHARACTER)
  expect_is(timestamp_col, NUMERIC)
  expect_is(sub_period_col, NUMERIC)
  
  # check rasters
  for (file in cmos_col) {
    expect_true(file.exists(file))
    loaded_cmos <- test_raster_read(file) 
    expect_false(is.na(crs(loaded_cmos)) && is.na(st_crs(loaded_cmos)))
    # values must be binary
    expect_equal(minValue(loaded_cmos), 0)
    expect_equal(maxValue(loaded_cmos), 1)
  }
  
  for (file in pmos_col) {
    expect_true(file.exists(file))
    loaded_pmos <- test_stack_read(file)
    expect_false(is.na(crs(loaded_pmos)) && is.na(st_crs(loaded_pmos)))
  }
  
}

clean_unitemporal_wrapper <- function(COLS, records, max_sub_period, aoi, dir_out, prio_sensors = c()) {
  records_select <- select_unitemporal(records, max_sub_period = max_sub_period, aoi = aoi, dir_out = dir_out)
  clean_test_select(records_select, COLS)
}

clean_bitemporal_wrapper <- function(COLS, records, min_distance, max_sub_period, aoi, dir_out, prio_sensors = c()) {
  records_select <- select_bitemporal(records, min_distance = min_distance, 
                                      max_sub_period = max_sub_period, aoi = aoi, dir_out = dir_out)
  clean_test_select(records_select, COLS)
}

clean_timeseries_wrapper <- function(COLS, records, num_timestamps, min_distance, max_sub_period, aoi, dir_out, prio_sensors = c()) {
  records_select <- select_timeseries(records, num_timestamps = num_timestamps, min_distance = min_distance,
                                      max_sub_period = max_sub_period,
                                      aoi = aoi, dir_out = dir_out)
  clean_test_select(records_select, COLS)
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
  expect_error(select_timeseries(records = flawed_records, num_timestamps = n_timestamps, max_sub_period = max_sub, 
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
                                   num_timestamps = n_timestamps, 
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
                                 aoi = aoi, dir_out = tt$tmp), error1_max_sub_period)
  expect_error(select_timeseries(records, num_timestamps = n_timestamps, max_sub_period = input1_max_sub_period,
                                 aoi = aoi, dir_out = tt$tmp), error1_max_sub_period)
  
  # min_distance
  min_distance_name <- "min_distance"
  input1_min_distance <- "Malfoy"
  error1_min_distance <- type_error_msg(input1_min_distance, min_distance_name, NUMERIC)
  expect_error(select_bitemporal(records, min_distance = input1_min_distance, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = tt$tmp), error1_min_distance) # BT
  expect_error(select_timeseries(records, min_distance = input1_min_distance, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = tt$tmp), error1_min_distance) # TS
  
  # num_timestamps
  num_timestamps_name <- "num_timestamps"
  input1_num_timestamps <- "Lucius"
  error1_num_timestamps <- type_error_msg(input1_num_timestamps, num_timestamps_name, NUMERIC)
  expect_error(select_timeseries(records, num_timestamps = input1_num_timestamps, 
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
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
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
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
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
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
                                 satisfaction_value = input1_satisfaction_value,
                                 aoi = aoi, dir_out = tt$tmp), error1_satisfaction_value) # TS
  
  # prio_sensors
  prio_sensors_name <- "prio_sensors"
  input1_prio_sensors <- c(100, 200)
  error1_prio_sensors <- type_error_msg(input1_prio_sensors, prio_sensors_name, CHARACTER)
  input2_prio_sensors <- c("Sentinel-2", "Kedavra")
  error2_prio_sensors <- "Argument 'prio_sensors' has to be provided with sensor names in the same format as returned by get_names() except MODIS products ('MODIS')"
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  prio_sensors = input1_prio_sensors,
                                  aoi = aoi, dir_out = tt$tmp), error1_prio_sensors) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 prio_sensors = input1_prio_sensors,
                                 aoi = aoi, dir_out = tt$tmp), error1_prio_sensors)
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
                                 prio_sensors = input1_prio_sensors,
                                 aoi = aoi, dir_out = tt$tmp), error1_prio_sensors)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  prio_sensors = input2_prio_sensors,
                                  aoi = aoi, dir_out = tt$tmp), error2_prio_sensors) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 prio_sensors = input2_prio_sensors,
                                 aoi = aoi, dir_out = tt$tmp), error2_prio_sensors)
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
                                 prio_sensors = input2_prio_sensors,
                                 aoi = aoi, dir_out = tt$tmp), error2_prio_sensors)
  
  # aoi
  input1_aoi <- NULL
  input2_aoi <- "Crucio"
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = input1_aoi, dir_out = tt$tmp), AOI_TYPE_ERROR) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = input1_aoi, dir_out = tt$tmp), AOI_TYPE_ERROR)
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = input1_aoi, dir_out = tt$tmp), AOI_TYPE_ERROR)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = input2_aoi, dir_out = tt$tmp), AOI_UNDEFINED_ERROR) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = input2_aoi, dir_out = tt$tmp), AOI_UNDEFINED_ERROR)
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = input2_aoi, dir_out = tt$tmp), AOI_UNDEFINED_ERROR)
  
  # dir_out
  input1_dir_out <- 10
  error1_dir_out <- type_error_msg(input1_dir_out, "dir_out", CHARACTER)
  input2_dir_out <- "Quirrell"
  error2_dir_out <- dir_out_error_msg(input2_dir_out)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = aoi, dir_out = input1_dir_out), error1_dir_out) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = input1_dir_out), error1_dir_out)
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = input1_dir_out), error1_dir_out)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = aoi, dir_out = input2_dir_out), error2_dir_out) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = input2_dir_out), error2_dir_out)
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = input2_dir_out), error2_dir_out)
  
  # verbose
  input1_verbose <- "of course"
  error1_verbose <- type_error_msg(input1_verbose, "verbose", LOGICAL)
  expect_error(select_unitemporal(records, max_sub_period = max_sub,
                                  aoi = aoi, dir_out = tt$tmp, verbose = input1_verbose), error1_verbose) # UT
  expect_error(select_bitemporal(records, min_distance = min_dist, max_sub_period = max_sub,
                                 aoi = aoi, dir_out = tt$tmp, verbose = input1_verbose), error1_verbose)
  expect_error(select_timeseries(records, min_distance = min_dist, max_sub_period = max_sub,
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
records <- read.csv(construct_filepath(dir_records, SENTINEL2, SUFFIX$records))
# with false input
error_test_select(records, aoi = aoi_test, tt)

# clean input
# ------------------------------------
# TEST 2
# -------
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, SENTINEL1, SUFFIX$records))
# Target: test unitemporal with Sentinel-1
records_select <- select_unitemporal(records, max_sub_period = max_sub_period, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test bitemporal with Sentinel-1
records_select <- select_bitemporal(records, max_sub_period = max_sub_period, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test timeseries with Sentinel-1
records_select <- select_timeseries(records, 
                                    num_timestamps = num_timestamps, 
                                    min_distance = min_distance, 
                                    max_sub_period = max_sub_period, 
                                    dir_out = tt$tmp)
finish_dir(tt$tmp)

# TEST 3
# -------
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, SENTINEL2, SUFFIX$records))
# Target: test unitemporal with Sentinel-2
clean_unitemporal_wrapper(COLS, records, max_sub_period = max_sub_period, 
                          aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test bitemporal with Sentinel-2
clean_bitemporal_wrapper(COLS, records, 
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test timeseries with Sentinel-2
clean_timeseries_wrapper(COLS, records, num_timestamps = num_timestamps,
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)

# TEST 4
# -------
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, SENTINEL3, SUFFIX$records))
# Target: test unitemporal with Sentinel-3
clean_unitemporal_wrapper(COLS, records, max_sub_period = max_sub_period, 
                          aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test bitemporal with Sentinel-3
clean_bitemporal_wrapper(COLS, records, 
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test timeseries with Sentinel-3
clean_timeseries_wrapper(COLS, records, num_timestamps = num_timestamps,
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)

# TEST 5
# -------
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, LANDSAT, SUFFIX$records))
# Target: test unitemporal with Landsat
clean_unitemporal_wrapper(COLS, records, max_sub_period = max_sub_period, 
                          aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test bitemporal with Landsat
clean_bitemporal_wrapper(COLS, records, 
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test timeseries with Landsat
clean_timeseries_wrapper(COLS, records, num_timestamps = num_timestamps,
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)

# TEST 6
# -------
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, MODIS, SUFFIX$records))
# Target: test unitemporal with MODIS
clean_unitemporal_wrapper(COLS, records, max_sub_period = max_sub_period, 
                          aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test bitemporal with MODIS
clean_bitemporal_wrapper(COLS, records, 
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test timeseries with MODIS
clean_timeseries_wrapper(COLS, records, num_timestamps = num_timestamps,
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)

# TEST 7
# -------
initialize_dir(tt$tmp)
records <- read.csv(construct_filepath(dir_records, MIXED, SUFFIX$records))
# Target: test unitemporal with mixed sensors without prio_sensors
clean_unitemporal_wrapper(COLS, records, max_sub_period = max_sub_period, 
                          aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test bitemporal with mixed sensors without prio_sensors
clean_bitemporal_wrapper(COLS, records, 
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test timeseries with mixed sensors without prio_sensors
clean_timeseries_wrapper(COLS, records, num_timestamps = num_timestamps,
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test unitemporal with mixed sensors with prio_sensors
clean_unitemporal_wrapper(COLS, records, max_sub_period = max_sub_period, 
                          aoi = aoi_test, dir_out = tt$tmp, prio_sensors = prio_sensors)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test bitemporal with mixed sensors with prio_sensors
clean_bitemporal_wrapper(COLS, records, 
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp, prio_sensors = prio_sensors)
finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# Target: test timeseries with mixed sensors with prio_sensors
clean_timeseries_wrapper(COLS, records, num_timestamps = num_timestamps,
                         min_distance = min_distance, max_sub_period = max_sub_period,
                         aoi = aoi_test, dir_out = tt$tmp, prio_sensors = prio_sensors)
finish_dir(tt$tmp)
