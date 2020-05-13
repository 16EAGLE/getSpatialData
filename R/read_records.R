#' read records from file
#' 
#' @description \code{read_records} reads records from a csv file
#' that has been previously written through \link{write_records}.
#' 
#' @param file character file path from where to read the records file.
#' @param as_sf logical specifies if the records shall be returned as sf object.
#' Otherwise a data.frame will be returned. Default is TRUE.
#' @inheritParams calc_cloudcov
#' 
#' @return sf object or data.frame depending on \code{as_sf}.
#' 
#' @importFrom sf st_geometry<- st_read
#' 
#' @author Henrik Fisser, 2020
#' 
#' @export
read_records <- function(file, as_sf = TRUE, verbose = TRUE) {
  GEOM <- "geom"
  FOOTPRINT <- name_footprint()
  .check_verbose(verbose)
  .check_character(file, "file")
  .check_file_exists(file, 3)
  out(paste0("Reading records from ", file), msg=T, type=1)
  records <- try(suppressWarnings(st_read(file, stringsAsFactors = FALSE, quiet = TRUE)))
  if (inherits(records, "data.frame")) { # sf dataframe
    if (as_sf && endsWith(tolower(file), ".csv")) out("Converting to sf data.frame", msg = T, type = 1)
    if (GEOM %in% names(records)) {
      records[[FOOTPRINT]] <- NULL
      names(records)[which(names(records) == GEOM)] <- FOOTPRINT
    }
    st_geometry(records) <- FOOTPRINT
    records <- .uncharacter_dataframe(records)
    records <- .eval_records_footprints(records, as_sf = as_sf)
    records <- .unlist_df(records)
    n_records <- NROW(records)
    out(paste0("Read ", 
               n_records, ifelse(n_records > 1, " records", " record")), msg=F, type=1)
    return(records)
  } else {
    out(paste0("Failed to read records: ", file), 3)
  }
  
}
