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

#' check records
#'
#' @param records object.
#' @param col.names character vector.
#' @param as_df logical.
#'
#' @keywords internal
#' @noRd
.check_records <- function(records, col.names = NULL, as_df = FALSE){
  if(!isTRUE(inherits(records, "data.frame"))) out("Argument 'records' must be of class 'data.frame' or 'sf' 'data.frame'.", type = 3)
  if(!is.null(col.names)){
    catch <- lapply(col.names, function(x) if(!(x %in% colnames(records))) out(paste0("A column of 'records' named '", x, "' is required for this action, but is missing."), type = 3))
  }
  if (as_df) records <- as.data.frame(records)
  return(records)
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

#' checks the prio_sensors argument
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @return nothing. In case of problematic input in prio_sensors: error.
#' @keywords internal
#' @noRd
.select_check_prio_sensors <- function(prio_sensors) {
  
  if (class(prio_sensors) != "character") out("Argument 'prio_sensors' has to be of class character (vector)",3)
  optical_sensors <- c("Sentinel-2","Sentinel-3","Landsat-5","Landsat-7","Landsat-8","MODIS")
  some_wrong <- isFALSE(any(sapply(prio_sensors,function(x) check <- x %in% optical_sensors)))
  if (some_wrong) {
    out("Argument 'prio_sensors' has to be provided with sensor names in the same format as returned by get_names()",3)
  }
  
}

#' checks if all files in a vector of paths are on disk
#' @param paths character vector of paths to files to check on.
#' @param item_name character name of the type of files checking on. E.g. "preview_file".
#' @return In case all are given: NA. Else: it throws a warning
#' @keywords internal
#' @noRd
.select_check_files <- function(paths, item_name) {
  
  paths <- paths[intersect(which(!is.na(paths)),which(paths != "NONE"))]
  exist <- sapply(paths,function(p) file.exists(p))
  all_on_disk <- isTRUE(all(exist))
  if (all_on_disk) {
    return(NA)
  } else {
    number_not_found <- which(exist == FALSE)
    out(
      paste0("All files in '",item_name,"' have to be saved at the location as indicated
             in the paths. Out of ",length(paths)," files ",number_not_found," cannot be located"),2)
  }
  
}

#' creates an error if requested coverage is higher than sensor revisit time
#' @param sensor character vector of sensor(s).
#' @param period character vector of start and end date.
#' @param num_timestamps numeric number of timestamps. 
#' @return nothing. Console communication
#' @keywords internal
#' @noRd
.select_handle_revisit <- function(sensor, period, num_timestamps) {
  
  revisit_times <- list("LANDSAT_8_C1"=8,"LANDSAT_ETM_C1"=8,"LANDSAT_TM_C1"=16,"LANDSAT_MSS_C1"=16,
                        "MODIS_MOD09A1_V6"=8,"MODIS_MYD09A1_V6"=8,"MODIS_MOD09Q1_V6"=8,
                        "MODIS_MOD09Q1_V6"=8,"MODIS_MOD09GA_V6"=1,"MODIS_MYD09GA_V6"=1,
                        "MODIS_MOD09GQ_V6"=1,"MODIS_MYD09GQ_V6"=1,
                        "MODIS_MOD09CMG_V6"=1,"MODIS_MYD09CMG_V6"=1,
                        "Sentinel-2"=5,"Sentinel-3"=2,"MODIS"=2)
  r <- min(sapply(sensor,function(x) {revisit_times[[x]]}))
  sub_period <- (as.numeric(as.Date(period[2]) - as.Date(period[1]))) / num_timestamps
  info <- paste0("Selected number of timestamps (",num_timestamps)
  s <- ifelse(length(sensor)==1,paste0("\n- Sensor: ",sensor),paste0("\nSensors: ",sensor))
  out(cat("Number of timestamps selected:",num_timestamps,s))
  if (sub_period < r) {
    out(paste0(info,") results in shorter coverage frequency than sensor revisit time (",r,"). Decrease 'num_timestamps'"),3)
  } else if (sub_period == r) {
    out(paste0(info,") results in equal coverage frequency as revisit time (",r,"). It is unlikely to get cloud-free coverage this frequent"),1)
  }
  
}

#' checks if columns are given in records and if they have values
#' @param records data.frame.
#' @param cols character vector of column names to be checked on.
#' @return nothing. Console error if column is not given.
#' @keywords internal
#' @noRd
.catch_missing_columns <- function(records, cols) {
  
  missing <- c()
  empty <- c()
  for (col in cols) {
    is_missing <- isFALSE(col %in% names(records))
    is_empty <- all(is.na(records[[col]]))
    if (isTRUE(is_missing)) {
      missing <- c(missing,col)
    }
    if (isTRUE(is_empty)) {
      empty <- c(empty,col)
    }
  }
  for (fail in unique(c(missing,empty))) {
    if (fail %in% missing) out(paste0("Argument 'records' lack needed columns:\n",fail,"\n"),2)
    if (fail %in% empty) out(paste0("Arguent 'records' has empty columns that should have values:\n",fail,"\n"),2)
  }
  
  error <- sapply(c(missing,empty),function(x) !is.null(x))
  if (TRUE %in% error) return(TRUE) else return(FALSE)
  
}

#' wrapper of all checks in select
#' @param records data.frame.
#' @param aoi aoi.
#' @param period character vector.
#' @param num_timestamps numeric.
#' @param prio_sensors character vector.
#' @param par list.
#' @param dir_out character.
#' @param verbose logical.
#' @return Throwing an error if a check is positive. Otherwise a list of dir_out
#' and aoi is returned (both optionally modified). If dir_out does not exist it
#' is created through check_dir_out.
#' @keywords internal
#' @noRd
.select_checks <- function(records, aoi, period, num_timestamps, prio_sensors = NULL,
                           par, dir_out, verbose) {
  
  options("gSD.verbose"=verbose)
  aoi <- .check_aoi(aoi,"sf",quiet=T)
  dir_out <- .check_dir_out(dir_out)
  # check if all columns are provided
  has_error <- .catch_missing_columns(records,cols=c(par$aoi_cc_col,par$aoi_cc_prb_col,
                                                     par$preview_col,par$cloud_mask_col))
  if (has_error) out("Argument 'records' cannot be processed as it lacks needed columns/values",3)
  if (!is.null(prio_sensors)) .select_check_prio_sensors(prio_sensors)
  .select_handle_revisit(unlist(records$product),period,num_timestamps)
  # check if needed files exist
  out("Checking if all needed clouds mask and preview rasters exist..",msg=T)
  check <- sapply(list(preview_file=records$preview_file,
                       cloud_mask_file=records$cloud_mask_file),function(x) {
    .select_check_files(x,names(x))
  })
  if (any(!is.na(check))) out("Cannot find (some) files on disk",3)
  return(records)

}
