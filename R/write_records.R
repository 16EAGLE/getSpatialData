#' Writes records to file
#' 
#' @description \code{write_records} Write records to a file. Obtain available formats through \link{get_records_drivers}.
#' Records can be read through \link{read_records}.
#' 
#' @inheritParams calc_cloudcov
#' 
#' @param file character file name or file path of file to be saved. If it is a file name it will be written
#' at \code{dir_out} in the format of \code{driver}. If it is a file path without valid file extension 
#' it will be written in the format of \code{driver} at the specified path. If it is a file path with 
#' valid file extension it will be written there. Check for available drivers and extensions through \link{get_records_drivers}.
#' @param driver character specifies the writer. Options are all thise returned by
#' \link{get_records_drivers}. Will be ignored in case a fully qualified \code{file} path is provided. Default is "GPKG".
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
write_records <- function(records, file = NULL, driver = "GPKG", dir_out = NULL, 
                          append = FALSE, verbose = TRUE, ...) {
  
  if (is.null(append)) append <- FALSE # could be passed as NULL through ... from higher level
  .check_verbose(verbose)
  .check_gdal_driver(driver)
  .check_records_type(records)
  records <- .check_records(records) # ensure it's sf
  records <- .unlist_df(records)
  user_file_is_path <- !is.null(file) && file != basename(file)
  if (!user_file_is_path) {
    dir_out <- .check_dir_out()
    # generate a file path from dir_out
    file <- ifelse(user_file_is_path, .generate_records_filename(file_name = file, dir_out = dir_out, driver = driver), file)
  }
  # check if file ends with a valid file extension
  split <- strsplit(file, "\\.")[[1]]
  ext <- split[length(split)]
  out(paste0("Writing records to ", file), msg=T, type=1)
  if (ext %in% .get_records_drivers()) {
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

#' Returns drivers for writing records
#' @description \code{get_records_drivers} provides the driver names that can be used in \link{write_records}.
#' @return list (named) of characters drivers (names) and their extensions (values).
#' @author Henrik Fisser, 2020
#' @export
get_records_drivers <- function() {
  return(.get_records_drivers())
}