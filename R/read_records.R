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
  out(paste0("Reading records from ", file), msg=T, type=1)
  records <- try(st_read(file, stringsAsFactors = FALSE, quiet = TRUE))
  if (inherits(records, "data.frame")) { # sf dataframe
    out(paste0("Read ", NROW(records), " records"), msg=F, type=1)
    records <- .uncharacter_dataframe(records)
    records <- .eval_records_footprints(records, as_sf = as_sf)
    records <- .unlist_df(records)
    records[["footprint"]] <- NULL
    names(records)[which(names(records) == "geom")] <- "footprint"
    return(records)
  } else {
    out(paste0("Failed to read records: ", file), 3)
  }
  
}
