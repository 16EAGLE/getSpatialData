#' Creates an RGB preview of a Landsat product before downloading
#'
#' \code{getLandsat_preview} previews single products as RGB plot which had been queried using \link{getLandsat_query}. The function is useful to apply visual checks to products before downloading them.
#'
#' @inheritParams getLandsat_query
#' @param product data.frame, single row data.frame collected from the return of \link{getLandsat_query}, representing the selected product and all its attributes.
#' @param on_map logical, if \code{TRUE}, the preview is displaed corner-georeferenced on a map. If \code{FALSE}, a simple RGB plot is displayed. Default is \code{TRUE}.
#' @param show_aoi logical, if \code{TRUE}, the session AOI defined with \link{set_aoi} is drawn to the map viewer. Ignored, if \code{on_map = FALSE} or if no AOI has been defined with \code{set_aoi}. Default is \code{TRUE}.
#'
#' @return None. A plot/view display is generated.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @examples
#'
#' @importFrom getPass getPass
#' @importFrom httr GET write_disk authenticate
#' @importFrom raster stack plotRGB crs crs<- extent extent<- NAvalue
#' @importFrom sf st_as_sfc st_crs as_Spatial
#' @importFrom mapview viewRGB addFeatures
#'
#' @seealso \link{getLandsat_names} \link{getLandsat_query} \link{getLandsat_data}
#' @export

getLandsat_preview <- function(product, on_map = TRUE, show_aoi = TRUE, username = NULL, password = NULL){

  ## Global USGS login #### currently not even necessary here! remove?
  if(is.null(username)){
    if(is.TRUE(getOption("gSD.usgs_set"))){
      api.key <- getOption("gSD.usgs_apikey")
    } else {
      out("Argument 'username' needs to be of type 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)
    }
  } else{
    if(is.null(password)) password = getPass()
    api.key <- usgs_login(username, password)
  }

  ## Intercept false inputs and get inputs
  url.icon <- product$browseUrl
  if(is.na(url.icon)){out("Argument 'product' is invalid or no preview is available.", type=3)}
  if(length(url.icon) > 1){out("Argument 'product' must contain only a single product, represented by a single row data.frame.")}
  char_args <- list(url.icon = url.icon)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)

  ## Recieve preview
  file_dir <- paste0(tempfile(),".jpg")
  GET(url.icon, write_disk(path = file_dir))
  preview <- stack(file_dir)
  #NAvalue(preview) <- 0

  if(is.TRUE(on_map)){

    ## create footprint
    footprint <- st_as_sfc(list(product$spatialFootprint))
    st_crs(footprint) <- 4326
    footprint <- as_Spatial(footprint)

    ## create preview
    crs(preview) <- crs(footprint)
    extent(preview) <- extent(footprint)
    preview <- aggregate(preview, 2) # make it faster

    ## create map
    map <- viewRGB(preview, r=1, g=2, b=3)

    if(is.TRUE(show_aoi)){
      if(is.FALSE(getOption("gSD.aoi_set"))){
        out("Preview without AOI, since no AOI has been set yet (use 'set_aoi()' to define an AOI).", type = 2)
      } else{
        aoi.m <- getOption("gSD.aoi")
        aoi.sf <- make_aoi(aoi.m, type = "sf", quiet = T)
        map <- addFeatures(map, aoi.sf)
      }
    }
    map # display mapview or leaflet output
  } else{

    ## create simple RGB plot
    plotRGB(preview)
  }
}
