#' read records from a csv file
#' 
#' @description \code{read_records} reads records from a csv file
#' that has been previously written through \link{write_records}.
#' 
#' @param file character complete file path from where to read the records file.
#' @param as_sf logical specifies if the records shall be returned as sf object.
#' Otherwise a data.frame will be returned.
#' 
#' @return sf object or data.frame depending on \code{as_sf}.
#' 
#' @author Henrik Fisser, 2020
#' 
#' @export
read_records <- function(file, as_sf = TRUE, verbose = TRUE) {
  
  .check_verbose(verbose)
  .check_character(file, "file")
  .check_file_exists(file, 3)
  records <- try(st_read(file, stringsAsFactors = FALSE, quiet = !verbose))
  if (inherits(records, "data.frame")) { # sf dataframe
    records <- .eval_records_footprints(records, as_sf = as_sf)
    records <- .df_dates_to_chars(records)
    records <- .unlist_df(records)
    return(records)
  } else {
    out(paste0("Failed to read records: ", file), 3)
  }
  
  names(records)[which(names(records) == "geom")] <- name_footprint()

}
