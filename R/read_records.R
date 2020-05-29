#' Read records from file
#' 
#' @description \code{read_records} reads records that have been 
#' written through \link{write_records}.
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
  GEOMETRY <- "geometry"
  GEOMETRY_PROP <- "geometryProperty"
  geoms <- c(GEOM, GEOMETRY, GEOMETRY_PROP)
  FOOTPRINT <- name_footprint()
  .check_as_sf(as_sf)
  .check_verbose(verbose)
  .check_character(file, "file")
  .check_file_exists(file, 3)
  out(paste0("Reading records from ", file), msg=T, type=1)
  records <- try(suppressWarnings(st_read(file, stringsAsFactors = FALSE, quiet = TRUE)))
  if (inherits(records, "data.frame")) { # sf dataframe
    names <- names(records)
    if (any(geoms %in% names)) {
      if (as_sf && endsWith(tolower(file), ".csv")) out("Converting to sf data.frame", msg = T, type = 1)
      records[[FOOTPRINT]] <- NULL
      names(records)[which(names(records) == geoms[geoms %in% names])] <- FOOTPRINT
      st_geometry(records) <- FOOTPRINT
      records <- .eval_records_footprints(records, as_sf = as_sf)
    }
    if (as_sf && !inherits(records, "sf")) out(paste0("Cannot convert to sf data.frame"), msg = T, type = 2)
    records <- .uncharacter_dataframe(records)
    records <- .unlist_df(records)
    n_records <- NROW(records)
    out(paste0("Read ", 
               n_records, ifelse(n_records > 1, " records", " record")), msg=F, type=1)
    records <- .check_records(records, as_sf = as_sf)
    return(records)
  } else {
    out(paste0("Failed to read records: ", file), 3)
  }
  
}
