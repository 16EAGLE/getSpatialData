#' test of records IO through read_records and write_records
# -----------------------------------------------------------------------------------------

initialize_dir(tt$tmp)

records_in <- construct_filepath(tt$resources$records, SENTINEL2, PREFIX$cmasks)
records_out <- file.path(tt$tmp, "records")
name_footprint <- getSpatialData:::name_footprint()

# test get_records_drivers()
drivers <- expect_type(get_records_drivers(), LIST)
geojson <- ".geojson"
gpkg <- ".gpkg"
expect_true(geojson %in% drivers)
expect_true(gpkg %in% drivers)
expect_true(!is.null(names(drivers)))
expect_true(drivers["GeoJSON"] == geojson)
expect_true(drivers["GPKG"] == gpkg)

# test read_records as sf
expect_equal(class(expect_message(read_records(records_in)))[1], SF)
records_sf <-  expect_message(read_records(records_in))
expect_true(inherits(records_sf[[name_footprint]], "sfc"))
records_sf <- expect_equal(class(expect_message(read_records(records_in)))[2], DATAFRAME)
records_sf <-  expect_message(read_records(records_in))
# test read_records as df
records_df <- expect_equal(class(expect_message(read_records(records_in, as_sf = FALSE)))[1], DATAFRAME)
records_df <-  expect_message(read_records(records_in, as_sf = FALSE))
expect_false(inherits(records_df[[name_footprint]], "sfc"))
# test read_records with error
input_file <- "this is not valid"
expect_error(read_records(input_file), paste0("File does not exist: ", input_file))
expect_error(read_records(records_in, verbose = "should not work"))
expect_error(read_records(records_in, as_sf = "should not work"))

columns_given <- function(records_path) {
  records <- expect_message(read_records(records_path))
  names <- names(records)
  expect_true(getSpatialData:::name_footprint() %in% names)
  expect_true(getSpatialData:::name_product() %in% names)
  expect_true(getSpatialData:::name_product_group() %in% names)
  expect_true(getSpatialData:::name_record_id() %in% names)
  expect_true(inherits(records[[name_footprint]], "sfc"))
}

finish_dir(tt$tmp)
initialize_dir(tt$tmp)
# test write_records
for (driver in names(drivers)) {
  for (records in list(records_sf, records_df)) {
    # write with file path
    records_out_with_extension <- paste0(records_out, drivers[[driver]])
    
    written_with_extension <- expect_message(write_records(records, records_out_with_extension))
    written_without_extension <- expect_message(write_records(records, records_out, driver))
    written_with_driver_and_extension <- expect_message(write_records(records, records_out_with_extension, driver))
    written_with_driver_upper_case <- expect_message(write_records(records, records_out, toupper(driver)))
    written_with_driver_lower_case <- expect_message(write_records(records, records_out, tolower(driver)))
    written_with_extension_as_driver <- expect_message(write_records(records, records_out, drivers[[driver]]))
    written_with_extension_as_driver_upper_case <- expect_message(write_records(records, records_out, toupper(drivers[[driver]])))
    written_with_extension_as_driver_lower_case <- expect_message(write_records(records, records_out, tolower(drivers[[driver]])))
    written_without_file <- expect_message(write_records(records, driver = driver, dir_out = tt$tmp))
    
    expect_equal(written_without_extension, written_with_extension)
    expect_equal(written_without_extension, written_with_driver_and_extension)
    expect_equal(written_without_extension, written_with_driver_upper_case)
    expect_equal(written_without_extension, written_with_driver_lower_case)
    expect_equal(written_without_extension, written_with_extension_as_driver)
    expect_equal(written_without_extension, written_with_extension_as_driver_upper_case)
    expect_equal(written_without_extension, written_with_extension_as_driver_lower_case)
    expect_true(file.exists(written_without_file)) # with automatically generated file name
    
    columns_given(written_without_extension)
    columns_given(written_with_extension)
    columns_given(written_with_driver_and_extension)
    columns_given(written_with_driver_upper_case)
    columns_given(written_with_driver_lower_case)
    columns_given(written_with_extension_as_driver)
    columns_given(written_with_extension_as_driver_upper_case)
    columns_given(written_with_extension_as_driver_lower_case)
    columns_given(written_without_file)
    
    expect_error(write_records(records, driver = driver))
    expect_error(write_records(records, records_out_with_extension, driver = "no driver"))
    expect_error(write_records(records, records_out, driver = "no driver"))
    expect_error(write_records(records, "no file path"))
    expect_error(write_records("no records", records_out_with_extension))
    expect_error(write_records(records, records_out, append = "invalid append"))
    expect_error(write_records(records, dir_out = "no dir_out"))
    
    # test append arg
    file <- "nice_file"
    written_records <- expect_message(write_records(records, file = file, dir_out = tt$tmp, driver = driver))
    expect_equal(written_records, normalizePath(file.path(tt$tmp, paste0(file, drivers[[driver]]))))
    records_out_appended <- expect_message(write_records(records, written_records, append=TRUE))
    records_read <- expect_message(read_records(records_out_appended))
    columns_given(records_out_appended)
    expect_true(NROW(records_read) == (NROW(records) * 2))
    unlink(records_out_appended)
  }
}
finish_dir(tt$tmp)
