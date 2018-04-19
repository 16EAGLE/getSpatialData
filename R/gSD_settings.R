#' getSpatialData settings
#'
#' Defines global settings that are used by all \code{getSpatialData} functions
#'
#' @param username character, username to the corresponding service. For details on registration, see details.
#' @param password character, password to the corresponding service.
#' @param dir_archive character, directory to the \code{getSpatialData} archive folder.
#' @param aoi nothing, if an interactve \code{mapedit} viewer should open letting you draw an AOI polygon. Otherwise, sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter.
#' @param color chracter, polygon filling color.
#'
#' @details
#' \code{login_CopHub} defines the login credentials for the Copernicus Open Access Hub (register on \url{https://scihub.copernicus.eu/}), \code{login_USGS} defines USGS login credentials Register on \url{https://ers.cr.usgs.gov/register/}. These will be saved and made available for all \code{getSpatialData} functions during the whole session and will be erased when quitting the session. Alternatively, login credentials can be set individually with each \code{get*} function call.
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
#' login_CopHub(username = "my_user_name", password = "my_password")
#'
#' ## Define USGS user credentials
#' login_USGS(username = "my_user_name", password = "my_password")
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
login_CopHub <- function(username, password = NULL){

  if(is.null(password)){password <- getPass()}
  char_args <- list(username = username, password = password)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  options(gSD.cophub_user=username)
  options(gSD.cophub_pass=password)
  options(gSD.cophub_set=TRUE)
}


#' @rdname gSD_settings
#' @export
login_USGS <- function(username, password = NULL){

  if(is.null(password)){password <- getPass()}
  char_args <- list(username = username, password = password)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  options(gSD.usgs_user=username)
  options(gSD.usgs_pass=password)
  options(gSD.usgs_set=TRUE)
  options(gSD.usgs_apikey=usgs_login(username, password))
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
