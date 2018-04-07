#' getSpatialData settings
#'
#' Defines global settings that are used by all \code{getSpatialData} functions
#'
#' @param hub_user character, user name to the Copernicus Open Access Hub. Register on \url{https://scihub.copernicus.eu/}.
#' @param hub_pass character, password to the Copernicus Open Access Hub.
#' @param dir_archive character, directory to the \code{getSpatialData} archive folder.
#' @param aoi nothing, if an interactve \code{mapedit} viewer should open letting you draw an AOI polygon. Otherwise, sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter.
#' @param color chracter, polygon filling color.
#'
#' @details
#' \code{set_login_CopHub} defines the login credentials for the Copernicus Open Access Hub. These will be saved and made available for all \code{getSpatialData} functions during the whole session and will be erased when quitting the session. Leave the \code{hub_user} and \code{hub_pass} arguments of the \code{getSentinel*} functions to their default value (\code{NULL}) to force them to use the global credentials defined with \code{set_login_CopHub}.
#'
#' \code{set_archive} globally defines the directory on your machine (or an external device) where getSpatialData should build up its donwload data archive. Since getSpatialData handles big amounts of data, it is recommended to once define a location where enough free storage is available and then afterwards to not change the archive location. You need to define the archives location for each session after loading getSpatialData. It will then be remembered for the duration of the session. Apart from the archive location, you can manually define a download path when calling the *_data functions. If you do not define a path there, getSpatialData will direct the download to the defined archive. The archive is structred by sensors.
#'
#' \code{set_aoi} globally defines an AOI that is used for querying data within the running session (if no other AOI is provided with a query function call). If called without argument, an interactive \code{mapedit} viewer is opened letting you draw an AOI polygon. Otherwise, the function supports \code{sf}, \code{sp} or matrix objects as \code{aoi} input  (see argument \code{aoi}).
#'
#' \code{view_aoi} displays the defined AOI on an interactive \code{mapview}/\code{leaflet} map.
#'
#' @return None.
#' @author Jakob Schwalb-Willmann
#'
#'
#' @export
#' @name gSD_settings
#' @examples
#' ## Define user credentials for the Copernicus Open Access Hub
#' set_login_CopHub(hub_user = "my_user_name", hub_pass = "my_password")
#'
#' ## set archive directory
#' dir.arc <- tempdir()
#' set_archiv(dir.arc)
#'
#' ## set aoi (example aoi)
#' aoi <- data("aoi")[[1]]
#' set_aoi(aoi)
#'
#' \dontrun{
#' ## draw aoi interactively
#' set_aoi() # just call without an argument
#'
#' ## view aoi
#' view_aoi()
#' }
#'
#' @seealso getSentinel_query
#'
set_login_CopHub <- function(hub_user, hub_pass = NULL){

  if(is.null(hub_pass)){hub_pass <- getPass()}
  char_args <- list(hub_user = hub_user, hub_pass = hub_pass)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  options(gSD.cophub_user=hub_user)
  options(gSD.cophub_pass=hub_pass)
  options(gSD.cophub_set=TRUE)
}

#' @rdname gSD_settings
#' @export

set_archive <- function(dir_archive){

  if(!is.character(dir_archive)){out(paste0("Argument 'dir_archive' needs to be of type 'character'."), type = 3)}
  if(!dir.exists(dir_archive)) out("The defined directory does not exist.", type=3)
  options(gSD.archive=dir_archive)
  options(gSD.archive_set=TRUE)
  #out(paste0("Session archive directory has been set to '", dir_archive, "'."), msg = T)
}


#' @rdname gSD_settings
#' @importFrom mapedit drawFeatures
#' @export

set_aoi <- function(aoi){

  ## draw aoi with mapedit, if aoi is not defined
  if(missing(aoi)){
    aoi <- drawFeatures(crs = 4326)$geometry
    if(length(aoi) != 1) out("Drawn AOI has to be a single polygon.", type = 3)
  }

  ## check aoi input
  aoi.m <- make_aoi(aoi, type = "matrix")

  ## set aoi
  options(gSD.aoi=aoi.m)
  options(gSD.aoi_set=TRUE)
  #out(paste0("Session AOI has been set successfully."), msg = T)
}

#' @rdname gSD_settings
#' @importFrom sf st_sfc st_polygon
#' @importFrom mapview mapview
#' @export

view_aoi <- function(color = "green"){

  if(is.FALSE(getOption("gSD.aoi_set"))) out("No AOI has been set yet, use 'set_aoi()' to define an AOI.", type = 3)
  aoi.m <- getOption("gSD.aoi")

  aoi.sf <- make_aoi(aoi.m, type = "sf", quiet = T)
  mapview(aoi.sf, col.regions = color)
}
