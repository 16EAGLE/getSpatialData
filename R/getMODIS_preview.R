#' Preview a MOSIS image
#'
#' \code{getMODIS_preview} previews a single MODIS record as image on a map or as RGB plot. The function is useful to apply visual checks to records before downloading them.
#'
#' @inheritParams getLandsat_query
#' @param record data.frame, single row data.frame collected from the return of \link{getMODIS_query}, representing the selected record and all its attributes.
#' @param on_map logical, if \code{TRUE}, the preview is displaed corner-georeferenced on a map. If \code{FALSE}, a simple RGB plot is displayed. Default is \code{TRUE}.
#' @param show_aoi logical, if \code{TRUE}, the session AOI defined with \link{set_aoi} is drawn to the map viewer. Ignored, if \code{on_map = FALSE} or if no AOI has been defined with \code{set_aoi}. Default is \code{TRUE}.
#'
#' @return None. A plot/view display is generated.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @seealso \link{getMODIS_names} \link{getMODIS_query} \link{getMODIS_data}
#' @export

getMODIS_preview <- function(record, on_map = TRUE, show_aoi = TRUE, verbose = TRUE){

  .EE_preview(record = record, preview_crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs", 
              on_map = on_map, show_aoi = show_aoi, verbose = verbose)
}
