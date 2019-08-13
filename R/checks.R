#' check logins
#'
#' @param records
#'
#' @keywords internal
#' @noRd
.check_login <- function(services = NULL, records = NULL, verbose = F){
  
  if(is.null(services)){
    if(is.null(records)){
      services <- c("USGS", "Copernicus")
    } else{
      if("Landsat" %in% records$product_group | "MODIS" %in% records$product_group) services <- c(services, "USGS")
      if("Sentinel" %in% records$product_group) services <- c(services, "Copernicus")
    }
  }
  
  if(all("Copernicus" %in% services, !getOption("gSD.dhus_set"))) out("You are not logged in to Copernicus Hub (anymore). Please log in first using login_CopHub().", type = 3) else if(isTRUE(verbose)) out("You are currently logged in to Copernicus Hub.", msg = T)
  if(all("USGS" %in% services, !getOption("gSD.usgs_set"))) out("You are not logged in to USGS ERS (anymore). Please log in first using login_USGS().", type = 3) else if(isTRUE(verbose)) out("You are currently logged in to USGS ERS.", msg = T)
}

#' check recprds
#'
#' @param records
#'
#' @keywords internal
#' @noRd
.check_records <- function(records, col.names = NULL){
  if(!isTRUE(inherits(records, "data.frame"))) out("Argument 'records' must be of class 'data.frame' or 'sf' 'data.frame'.", type = 3)
  if(!is.null(col.names)){
    catch <- lapply(col.names, function(x) if(!(x %in% colnames(records))) out(paste0("A column of 'records' named '", x, "' is required for this action, but is missing."), type = 3))
  }
}

#' check dir_out
#'
#' @param dir_out
#'
#' @keywords internal
#' @noRd
.check_dir_out <- function(dir_out, which = NULL){
  
  ## Check output directory
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)) dir_out <- getOption(paste0("gSD.archive", if(!is.null(which)) paste0("_", which)))
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  return(path.expand(dir_out))
}


#' check if command can be executed on the system command line
#' @param cmd command
#' @importFrom processx process
#' @keywords internal
#' @noRd
.check_cmd <- function(cmd){
  sc <- try(processx::process$new(cmd),silent = TRUE)
  if(inherits(sc, "try-error")){return(FALSE)}else{return(TRUE)}
}


#' check and create aoi from aoi argument
#'
#' @param aoi aoi
#' @keywords internal
#' @importFrom sp SpatialPolygons
#' @importFrom sf st_sfc st_polygon st_crs st_as_sf st_coordinates st_transform st_crs<- as_Spatial
#' @noRd
.check_aoi <- function(aoi, type = "matrix", quiet = F){
  
  ## if not sfc, convert to sfc
  if(!inherits(aoi, c("Spatial", "sfc", "matrix"))) out("Argument 'aoi' needs to be a 'SpatialPolygons' or 'sfc_POLYGON' or 'matrix' object.", type = 3)
  if(inherits(aoi, "matrix")){
    if(!all(aoi[1,] == aoi[length(aoi[,1]),])) aoi <- rbind(aoi, aoi[1,])
    aoi <- st_sfc(st_polygon(list(aoi)), crs = 4326)
    if(isFALSE(quiet)) out(paste0("Argument 'aoi' is a matrix, assuming '", st_crs(aoi)$proj4string, "' projection."), type = 2)
  }
  if(inherits(aoi, "Spatial")) aoi <- st_as_sf(aoi)
  
  ## check projection
  if(is.na(st_crs(aoi))){
    st_crs(aoi) <- 4326
    if(isFALSE(quiet)) out(paste0("Argument 'aoi' has no projection, assuming '", st_crs(aoi)$proj4string, "' projection."), type = 2)
  }
  if(length(grep("WGS84", grep("longlat", st_crs(aoi)$proj4string, value = T), value = T)) != 1){
    aoi <- st_transform(aoi, 4326)
  }
  
  ## get coordinates
  aoi.m <- st_coordinates(aoi)[,c(1,2)]
  aoi.sf <- st_sfc(st_polygon(list(aoi.m)), crs = 4326)
  aoi.sp <- as_Spatial(aoi.sf)
  
  if(type == "matrix") return(aoi.m)
  if(type == "sf") return(aoi.sf)
  if(type == "sp") return(aoi.sp)
}

