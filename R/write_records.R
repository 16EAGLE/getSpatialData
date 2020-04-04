#' Writes records to a csv file
#' 
#' @description \code{write_records} write records to a csv file
#' that has been previously written through \link{write_records}.
#' 
#' @inheritParams get_records
#' 
#' @param dir_out directory where to write the records csv. Default is NULL.
#' @param file character complete file path where to write the records csv. Default is NULL.
#' If a \code{dir_out} is not NULL because it is set or provided, \code{file} will be ignored.
#'  
#' @return sf object or data.frame depending on \code{as_sf}.
#' 
#' @importFrom readr read_csv cols
#' 
#' @author Henrik Fisser, 2020
#' 
#' @export
write_records <- function(records, dir_out = NULL, file = NULL) {
  
  .check_dir_out(dir_out)
  .check_records_type(records)
  # it is important to write read through readr::read_csv() for correct representation
  # of the records, especially of its footprints
  write_csv(records, file)
  
}