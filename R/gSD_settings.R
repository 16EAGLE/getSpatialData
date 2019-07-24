#' Session-wide settings and checks
#'
#' Functions that define session-wide archive and AOI settings that are used by all \code{getSpatialData} functions and check for the availability of used online services.
#'
#' @inheritParams getSentinel_records
#' @param dir_archive character, directory to the \code{getSpatialData} archive folder.
#' @param create logical, whether to create directory, if not existing or not.
#' @param aoi nothing, if an interactve \code{mapedit} viewer should open letting you draw an AOI polygon. Otherwise, sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter.
#' @param type character, AOI object type, either "matrix", "sf" or "sp".
#' @param color chracter, polygon filling color.
#' @param value logical, whether to return a data frame containing service status or not. Default is \code{FALSE}.
#'
#' @details
#' \code{set_archive} defines the session directory on your machine (or an external device) where getSpatialData should build up its donwload data archive. Since getSpatialData handles big amounts of data, it is recommended to once define a location where enough free storage is available and then afterwards to not change the archive location. You need to define the archives location for each session after loading getSpatialData. It will then be remembered for the duration of the session. Apart from the archive location, you can manually define a download path when calling the *_data functions. If you do not define a path there, getSpatialData will direct the download to the defined archive. The archive is structred by sensors.
#'
#' \code{get_archive} returns the currently defined archive directory.
#'
#' \code{set_aoi} defines a session AOI that is used for querying data within the running session (if no other AOI is provided with a query function call). If called without argument, an interactive \code{mapedit} viewer is opened letting you draw an AOI polygon. Otherwise, the function supports \code{sf}, \code{sp} or matrix objects as \code{aoi} input  (see argument \code{aoi}).
#'
#' \code{view_aoi} displays the defined session AOI on an interactive \code{mapview}/\code{leaflet} map.
#'
#' \code{get_aoi} returns the defined session AOI.
#' 
#' \code{services_avail} displays the status of all online services used by \code{getSpatialData}. Services that are operating as usual are labeled "available". Returns a \code{data.frame} containing the service status, if argument \code{value} is set to \code{TRUE}.
#'
#' @return None.
#' @author Jakob Schwalb-Willmann
#'
#'
#' @export
#' @name gSD_settings
#' @examples
#' ## set archive directory
#' dir.arc <- tempdir()
#' set_archive(dir.arc)
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
#' @seealso getSentinel_records getLandsat_records getMODIS_records
#'
set_archive <- function(dir_archive, create = T){
  
  if(!is.character(dir_archive)){out(paste0("Argument 'dir_archive' needs to be of type 'character'."), type = 3)}
  if(!dir.exists(dir_archive)) if(isTRUE(create)) dir.create(dir_archive, recursive = T) else out("The defined directory does not exist.", type=3)
  
  options(gSD.archive = path.expand(dir_archive))
  options(gSD.archive_data = paste0(dir_archive, "/data"))
  options(gSD.archive_previews = paste0(dir_archive, "/previews"))
  #options(gSD.archive_prep = paste0(dir_archive, "/prep_data"))
  options(gSD.archive_set = TRUE)
  #out(paste0("Session archive directory has been set to '", dir_archive, "'."), msg = T)
}

#' @rdname gSD_settings
#' @importFrom mapedit drawFeatures
#' @export
get_archive <- function() if(isTRUE(getOption("gSD.archive_set"))) getOption("gSD.archive") else out("Archive has not been set. Use set_archive() to define an getSpatialData archive directory.", msg = T)

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
  aoi.sf <- .make_aoi(aoi, type = "sf")
  
  ## set aoi
  options(gSD.aoi=aoi.sf)
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
  
  aoi.sf <- .make_aoi(aoi.m, type = "sf", quiet = T)
  mapview(aoi.sf, col.regions = color)
}

#' @rdname gSD_settings
#' @importFrom sf st_sfc st_polygon
#' @export
get_aoi <- function(type = "sf"){
  
  if(is.FALSE(getOption("gSD.aoi_set"))) out("No AOI has been set yet, use 'set_aoi()' to define an AOI.", type = 3)
  aoi <- getOption("gSD.aoi")
  .make_aoi(aoi, type = type, quiet = T)
}

#' @rdname gSD_settings
#' @importFrom httr GET http_status
#' @importFrom cli cat_bullet
#' @export
services_avail <- function(value = F, verbose = T){
  
  .check_login()
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  
  # get service URLs
  urls <- getOption("gSD.api")
  urls <- urls[names(urls) != "aws.l8.sl"]
  urls$aws.l8 <- gsub("c1/L8/", "", urls$aws.l8)
  
  # get service status (login for ESPA)
  response <- lapply(urls, function(x) try(gSD.get(x), silent = T))
  if(isTRUE(getOption("gSD.usgs_set"))) response$espa <- try(gSD.get(urls$espa, username = getOption("gSD.usgs_user"), password = getOption("gSD.usgs_pass")))
  
  df <- do.call(rbind, lapply(response, function(x) if(!inherits(x, "try-error")) rbind.data.frame(http_status(x), stringsAsFactors = F) else NA))
  df$code <- sapply(response, function(y) if(!inherits(y, "try-error")) y$status_code else NA)
  df$service <- unlist(getOption("gSD.api.names")[rownames(df)])
  df$id <- rownames(df)
  
  # interpret service status
  df$status <- "available"
  df$colour <- "green"
  df$remark <- "Connection successfully established."
  
  # not ok
  items <- which(df$code != 200)
  if(length(items) > 0){
    df[items,]$status <- "unknown"
    df[items,]$colour <- "blue"
    df[items,]$remark <- as.character(df[items,]$message)
  }
  
  # maintenace
  items <- which(df$code == 301)
  if(length(items) > 0){
    df[items,]$status <- "maintenance"
    df[items,]$colour <- "orange"
  }
  
  # timeout, no connection etc.
  items <- which(is.na(df$code))
  if(length(items) > 0){
    df[items,]$status <- "no connection"
    df[items,]$colour <- "red"
    df[items,]$remark <- "Connection could not be established."
  }
  
  if(isTRUE(getOption("gSD.verbose"))){
    catch <- apply(df, MARGIN = 1, function(x, nc = max(nchar(df$service)), names = colnames(df)){
      y <- rbind.data.frame(x, stringsAsFactors = F)
      colnames(y) <- names
      cat_bullet(paste0(y$service, ":", paste0(rep(" ", times = nc-nchar(y$service)), collapse = ""), "  '", y$status, "' ", paste0(rep(" ", times = (13-nchar(y$status))), collapse = ""), "'", y$remark, "'"), bullet_col = y$colour)
    })
  }
  if(isTRUE(value)) return(df[c("service", "status", "remark", "category", "reason", "message", "code")])
}