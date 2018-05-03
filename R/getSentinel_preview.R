#' Preview a Sentinel image
#'
#' \code{getSentinel_preview} previews single image as RGB plot which had been queried using \link{getSentinel_query}. The function is useful to apply visual checks to records before downloading them.
#'
#' @inheritParams getSentinel_query
#' @param record data.frame, single row data.frame collected from the return of \link{getSentinel_query}, representing the selected record and all its attributes.
#' @param on_map logical, if \code{TRUE}, the preview is displaed corner-georeferenced on a map. If \code{FALSE}, a simple RGB plot is displayed. Default is \code{TRUE}.
#' @param show_aoi logical, if \code{TRUE}, the session AOI defined with \link{set_aoi} is drawn to the map viewer. Ignored, if \code{on_map = FALSE} or if no AOI has been defined with \code{set_aoi}. Default is \code{TRUE}.
#'
#' @return None. A plot/view display is generated.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @examples
#' ## Load packages
#' library(getSpatialData)
#' library(sf)
#' library(sp)
#'
#' ## Define an AOI (either matrix, sf or sp object)
#' data("aoi_data") # example aoi
#'
#' aoi <- aoi_data[[3]] # AOI as matrix object, or better:
#' aoi <- aoi_data[[2]] # AOI as sp object, or:
#' aoi <- aoi_data[[1]] # AOI as sf object
#'
#' ## set AOI for this session
#' set_aoi(aoi)
#' view_aoi() #view AOI in viewer
#' # or, simply call set_aoi() without argument to interactively draw an AOI
#'
#' ## Define time range and platform
#' time_range <-  c("2017-08-01", "2017-08-30")
#' platform <- "Sentinel-2"
#'
#' ## set login credentials and an archive directory
#' \dontrun{
#' login_CopHub(username = "username") #asks for password or define 'password'
#' set_archive("/path/to/archive/")
#'
#' ## Use getSentinel_query to search for data (using the session AOI)
#' records <- getSentinel_query(time_range = time_range, platform = platform)
#'
#' ## Get an overview of the records
#' View(records) #get an overview about the search records
#' colnames(records) #see all available filter attributes
#' unique(records$processinglevel) #use one of the, e.g. to see available processing levels
#'
#' ## Filter the records
#' records_filtered <- records[which(records$processinglevel == "Level-1C"),] #filter by Level
#'
#' ## Preview a single record
#' getSentinel_preview(record = records_filtered[5,])
#'
#' ## Download some datasets
#' files <- getSentinel_data(records = records_filtered[c(4,5,6),])
#' }
#'
#' @seealso \link{getSentinel_query}
#'
#' @importFrom getPass getPass
#' @importFrom httr GET write_disk authenticate
#' @importFrom raster stack plotRGB crs crs<- extent extent<- NAvalue
#' @importFrom sf st_as_sfc st_crs as_Spatial
#' @importFrom mapview viewRGB addFeatures
#'
#' @export

getSentinel_preview <- function(record, on_map = TRUE, show_aoi = TRUE, username = NULL, password = NULL,
                                hub = "auto", verbose = TRUE){

  ## Global Copernicus Hub login
  if(is.TRUE(getOption("gSD.cophub_set"))){
    if(is.null(username)){username <- getOption("gSD.cophub_user")}
    if(is.null(password)){password <- getOption("gSD.cophub_pass")}
  }
  if(!is.character(username)){out("Argument 'username' needs to be of type 'character'. You can use 'login_CopHub()' to define your login credentials globally.", type=3)}
  if(!is.null(password)){password = password}else{password = getPass()}
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)

  ## Intercept false inputs and get inputs
  url.icon <- record$url.icon
  if(is.na(url.icon)){out("Argument 'record' is invalid or no preview is available.", type=3)}
  if(length(url.icon) > 1){out("Argument 'record' must contain only a single record, represented by a single row data.frame.")}
  char_args <- list(url.icon = url.icon)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)

  ## Manage API access
  platform <- record$platformname
  cred <- .CopHub_select(hub, platform, username, password)

  ## Recieve preview
  file_dir <- paste0(tempfile(),".jpg")
  gSD.get(url.icon, cred[1], cred[2], dir.file = file_dir)
  preview <- stack(file_dir)
  #NAvalue(preview) <- 0

  if(is.TRUE(on_map)){

    ## create footprint
    footprint <- st_as_sfc(list(record$footprint))
    st_crs(footprint) <- 4326
    footprint <- as_Spatial(footprint)

    ## create preview
    crs(preview) <- crs(footprint)
    extent(preview) <- extent(footprint)

    ## create map
    map <- viewRGB(preview, r=1, g=2, b=3)

    if(is.TRUE(show_aoi)){
      if(is.FALSE(getOption("gSD.aoi_set"))){
        out("Preview without AOI, since no AOI has been set yet (use 'set_aoi()' to define an AOI).", type = 2)
      } else{
        aoi.sf <- getOption("gSD.aoi")
        #aoi.sf <- .make_aoi(aoi.m, type = "sf", quiet = T)
        map <- addFeatures(map, aoi.sf)
      }
    }
    map # display mapview or leaflet output
  } else{

    ## create simple RGB plot
    plotRGB(preview)
  }
}
