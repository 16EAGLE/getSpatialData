#' Writes records to a file
#' 
#' @description \code{write_records} write records to a file. They can be read through
#' \link{read_records}.
#' 
#' @inheritParams calc_cloudcov
#' 
#' @param file_name character name of the file to be saved. If it is NULL a file name will be 
#' automatically generated from date and time.
#' @param driver character specifies the writer. Options are all thise returned by
#' sf::st_drivers() plus "csv". Will be ignored in case a fully qualified
#' \code{file} path is provided. Default is "GPKG"
#' @param file character complete file path where to write the records. Must be a full file path
#' including file basename and extension matching a supported driver. Default is NULL.
#' @param append logical, if TRUE and a file path is given through \code{file}, the record(s)
#' will be appended to \code{file} if possible.
#' @param ... further arguments to be passed to sf::st_write.
#'  
#' @return character file path to the saved records.
#' 
#' @importFrom sf st_write
#' 
#' @author Henrik Fisser, 2020
#' 
#' @export
write_records <- function(records, file_name = NULL, driver = "GPKG", dir_out = NULL, 
                          file = NULL, append = FALSE, verbose = TRUE, ...) {
  
  if (is.null(append)) append <- FALSE # could be passed as NULL through ... from higher level
  .check_verbose(verbose)
  .check_gdal_driver(driver)
  file_given <- !is.null(file)
  if (file_given) driver <- NA
  .check_records_type(records)
  records <- .unlist_df(records)
  # if no file given we have to check dir_out
  if (!file_given) {
    dir_out <- .check_dir_out(dir_out) # can throw error
    file <- .generate_records_filename(file_name = file_name, dir_out = dir_out, driver = driver)
  } 
  records <- .check_records(records) # ensure it's sf
  out(paste0("Writing records to ", file), msg=T, type=1)
  if (is.na(driver)) {
    write <- try(st_write(records, dsn = file, append = append, quiet = TRUE))
  } else {
    write <- try(st_write(records, dsn = file, driver = driver, append = append, quiet = TRUE))
  }
  if (inherits(write, "try-error")) {
    out(paste0("Failed to write file: ", file), 3)
  } else {
    out(paste0("Wrote ", NROW(records), " records"), msg=F, type=1)
  }
  return(file)
  
}
