#' Session-wide area of interest (AOI)
#' 
#' Functions that set, view and get a session-wide area of interest (AOI) that can be used by all \code{getSpatialData} functions.
#'
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. If \code{set_aoi} is called and \code{aoi} is undefined, an interactve \code{mapedit} viewer should open letting you draw an AOI polygon.
#' @param type character, AOI object type, either "matrix", "sf" or "sp".
#' @param aoi_colour chracter, AOI colour.
#' @param ... deprecated arguments
#'
#' @details
#' \code{set_aoi} defines a session AOI that is used for querying data within the running session (if no other AOI is provided with a query function call). If called without argument, an interactive \code{mapedit} viewer is opened letting you draw an AOI polygon. Otherwise, the function supports \code{sf}, \code{sp} or matrix objects as \code{aoi} input  (see argument \code{aoi}).
#'
#' \code{view_aoi} displays the defined session AOI on an interactive \code{mapview}/\code{leaflet} map.
#'
#' \code{get_aoi} returns the defined session AOI.
#'
#' @author Jakob Schwalb-Willmann
#'
#'
#' @export
#' @name aoi
#' @examples
#'
#' ## set aoi (example aoi)
#' data("aoi_data")
#' set_aoi(aoi_data[[1]])
#'
#' \dontrun{
#' ## draw aoi interactively
#' set_aoi() # just call without an argument
#'
#' ## view aoi
#' view_aoi()
#'
#' ## return aoi
#' aoi <- get_aoi(type = "sf")
#' }
#'
#' @seealso \link{get_records}
#' 
#' @importFrom mapedit drawFeatures
#' @export
set_aoi <- function(aoi){
  
  ## draw aoi with mapedit, if aoi is not defined
  if(missing(aoi)){
    aoi <- drawFeatures(crs = 4326)$geometry
    if(length(aoi) != 1) out("Drawn AOI has to be a single polygon.", type = 3)
  }
  
  ## check aoi input
  aoi.sf <- .check_aoi(aoi, type = "sf")
  
  ## set aoi
  options(gSD.aoi=aoi.sf)
  options(gSD.aoi_set=TRUE)
}


#' @rdname aoi
#' @importFrom mapview mapview
#' @export
view_aoi <- function(aoi = NULL, aoi_colour = "deepskyblue", ...){
  
  aoi.sf <- .check_aoi(aoi, type = "sf")
  mapview(aoi.sf, layer.name = "AOI", label = "AOI", lwd = 6, color = aoi_colour, alpha.regions = 0, legend = F, homebutton = T)
}

#' @rdname aoi
#' @importFrom sf st_sfc st_polygon
#' @export
get_aoi <- function(type = "sf"){
  .check_aoi(aoi = NULL, type = type)
}