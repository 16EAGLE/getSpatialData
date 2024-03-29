#' Write records to file
#' 
#' @description \code{write_records} writes records. Obtain available formats through \link{get_records_drivers}.
#' Records can be read through \link{read_records}.
#' 
#' @inheritParams calc_cloudcov
#' 
#' @param file character file name or file path of file to be saved. If it is a file name it will be written
#' at \code{dir_out} in the format of \code{driver}. If it is a file path without valid file extension 
#' it will be written in the format of \code{driver} at the specified path. If it is a file path with 
#' valid file extension it will be written there. It can also be a character file name, in this case 
#' \code{dir_out} has to be provided. If the specified file exists and \code{append == FALSE} the file will be overwritten.
#' Check for available drivers and extensions through \link{get_records_drivers}.
#' @param driver character specifies the writer. Options are all thise returned by
#' \link{get_records_drivers}. Will be ignored in case a fully qualified \code{file} path is provided. Default is "GeoJSON".
#' @param append logical, if TRUE and a file path is given through \code{file}, the record(s)
#' will be appended to \code{file} if possible.
#' @param ... further arguments to be passed to sf::st_write.
#'  
#' @return character file path to saved records.
#' 
#' @importFrom sf st_write
#' 
#' @author Henrik Fisser, 2020
#' 
#' @export
write_records <- function(records, file = NULL, driver = "GeoJSON", dir_out = NULL, 
                          append = FALSE, verbose = TRUE, ...) {
  
  if (is.null(append)) append <- FALSE # could be passed as NULL through ... from higher level
  if (is.null(dir_out) && is.null(file)) {
    out("One of the two arguments 'file' and 'dir_out' has to be provided", 3)
  }
  .check_verbose(verbose)
  .check_records_type(records)
  records <- .check_records(records) # ensure it's sf
  records <- .unlist_df(records)
  user_file_is_path <- !is.null(file) && file != basename(file)
  
  # resolve driver
  drivers <- get_records_drivers()
  lower_driver_names <- tolower(names(drivers))
  lower_driver <- tolower(driver)
  user_provided_extension <- ifelse(startsWith(lower_driver, "."), 
                                    lower_driver, paste0(".", lower_driver)) %in% drivers # drivers are lower case anyways
  lower_driver <- ifelse(user_provided_extension && !startsWith(lower_driver, "."), paste0(".", lower_driver), lower_driver)
  driver <- ifelse(user_provided_extension, 
                   drivers[which(drivers == lower_driver)],
                   drivers[which(lower_driver_names == lower_driver)])
  extension <- driver[[1]]
  driver <- names(drivers)[which(drivers == extension)]
  .check_gdal_driver(driver)
  
  if (!user_file_is_path) {
    dir_out <- .check_dir_out(dir_out)
    # generate a file path from dir_out
    file <- .generate_records_filename(file_name = file, dir_out = dir_out, driver = driver)
  }
  # check if file ends with a valid file extension
  if (basename(file) == file) file <- file.path(dir_out, file)
  if (!any(sapply(drivers, function(x) {endsWith(file, x)}))) {
    file <- paste0(file, extension)
  }
  
  # collapsing some listst since drivers do only support one list for geometries
  records$preview_url <- sapply(records$preview_url, function(x) paste0(unlist(x), collapse = "; "))
  ## more? ##
  
  out(paste0("Writing records to ", file), msg=T, type=1)
  if (!append && file.exists(file)) unlink(file)
  write <- try(suppressWarnings(st_write(records, dsn = file, append = append, quiet = TRUE)))
  if (inherits(write, "try-error")) {
    out(paste0("Failed to write file: ", file), 3)
  } else {
    out(paste0("Wrote ", NROW(records), " records"), msg=F, type=1)
  }
  return(normalizePath(file))

}

#' Returns drivers for writing records
#' @description \code{get_records_drivers} provides the driver names that can be used in \link{write_records}.
#' @return list (named) of characters drivers (names) and their file extensions (values).
#' @author Henrik Fisser, 2020
#' @export
get_records_drivers <- function() {
  return(.get_records_drivers())
}