#' Writes records to a csv file
#' 
#' @description \code{write_records} write records to a csv file
#' that has been previously written through \link{write_records}.
#' 
#' @inheritParams get_records
#' 
#' @param dir_out directory where to write the records csv. A file name will be automatically
#' generated from date and time in case \code{dir_out} is set through \link{set_archive} or provided. 
#' Default is NULL.
#' @param file character complete file path where to write the records csv. Default is NULL.
#' If a \code{dir_out} is not NULL because it is set through \link{set_archive} or provided, 
#' \code{file} will be ignored.
#'  
#' @return character file path to the saved records csv.
#' 
#' @importFrom readr write_csv
#' 
#' @author Henrik Fisser, 2020
#' 
#' @export
write_records <- function(records, dir_out = NULL, file = NULL, as_sf = TRUE) {
  
  file_given <- !is.null(file)
  
  .check_records_type(records)
  records <- .unlist_df(records)
  # if no file given we have to check dir_out
  if (!file_given) {
    dir_out <- .check_dir_out(dir_out) # can throw error
    file_name <- .generate_datetime_filename("records", extension = ".csv", sep = "_")
    file <- file.path(dir_out, file_name)
  } 
  
  # it is important to write read through readr::read_csv() for correct representation
  # of the records, especially of its footprints
  write_csv(records, file)
  return(file)
  
}