#' Writes records to a file
#' 
#' @description \code{write_records} write records to a file
#' that has been previously written through \link{write_records}.
#' 
#' @inheritParams get_records
#' 
#' @param dir_out directory where to write the records. A file name will be automatically
#' generated from date and time in case \code{dir_out} is set through \link{set_archive} or provided. 
#' Default is NULL.
#' @param file character complete file path where to write the records. Default is NULL.
#' If a \code{dir_out} is not NULL because it is set through \link{set_archive} or provided, 
#' \code{file} will be ignored.
#' @param driver character specifies the writer. Options are all returned by
#' sf::st_drivers() and "csv". Default is "GPKG".
#' @param append logical if TRUE and a file path is given through \code{file} the record(s)
#' will be appended to \code{file} if possible.
#'  
#' @return character file path to the saved records csv.
#' 
#' @importFrom readr write_csv
#' @importFrom sf st_write
#' 
#' @author Henrik Fisser, 2020
#' 
#' @export
write_records <- function(records, dir_out = NULL, file = NULL, driver = "GPKG", append = FALSE) {
  
  .check_gdal_driver(driver)
  file_given <- !is.null(file)
  as_csv <- ifelse(file_given, endsWith(file, ".csv"), endsWith(tolower(driver), "csv"))
  
  .check_records_type(records)
  records <- .unlist_df(records)
  # if no file given we have to check dir_out
  if (!file_given) {
    dir_out <- .check_dir_out(dir_out) # can throw error
    ext <- ifelse(as_csv, ".csv", .get_driver_extension(driver))
    filename <- .generate_datetime_filename("records", extension = ext)
    file <- file.path(dir_out, filename)
  } 
  
  if (as_csv) {
    write_csv(records, file, append = append)
  } else {
    records <- .check_records(records) # ensure it's sf
    st_write(records, dsn = file, driver = driver, append = append)
    test <- st_read(file)
    
  }
  
  return(file)
  
}



