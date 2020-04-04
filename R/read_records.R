#' read records from a csv file
#' 
#' @description \code{read_records} reads records from a csv file
#' that has been previously written through \link{write_records}.
#' 
#' @param file character complete file path from where to read the records csv.
#' @param as_sf logical specify if the records shall be returned as sf object.
#' Otherwise a data.frame will be returned.
#' 
#' @return sf object or data.frame depending on \code{as_sf}.
#' 
#' @importFrom readr read_csv cols
#' 
#' @author Henrik Fisser, 2020
#' 
#' @export
read_records <- function(file, as_sf = TRUE) {
  
  .check_character(file, "file")
  exists <- .is_existing_csv_file(file, out_type = 3) # throws error in case of FALSE
  rm(exists)
  # it is important to write read through readr::read_csv() for correct representation
  # of the records, especially of its footprints
  records <- try(read_csv(file, col_types = cols()))
  is_dataframe <- inherits(records, "data.frame")
  if (is_dataframe) {
    records <- .eval_records_footprints(records, as_sf = as_sf)
    records <- .df_dates_to_chars(records)
    records <- .unlist_df(records)
    return(records)
  } else {
    out("Reading records csv was unsuccessful", 3)
  }

}
