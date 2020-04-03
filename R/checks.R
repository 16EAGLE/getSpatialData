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
    rm(catch)
  }
  if(as_df) records <- as.data.frame(records) else records <- st_sf(records, sfc_last = F)
  return(records)
}

#' check dir_out
#'
#' @param dir_out
#'
#' @keywords internal
#' @noRd
.check_dir_out <- function(dir_out, which = NULL){
  
  if (!is.null(dir_out)) .check_character(dir_out)
  
  ## Check output directory
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)) dir_out <- getOption(paste0("gSD.archive", if(!is.null(which)) paste0("_", which)))
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  dir_out <- path.expand(dir_out)
  if (!dir.exists(dir_out)) {
    # Be careful when changing message, it is checked on in unit tests
    out(paste0("Directory 'dir_out' does not exist: ", dir_out), 3)
  } else {
    return(path.expand(dir_out))
  }
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
  
  type <- tolower(type)
  
  if(is.null(aoi)){
    if(is.TRUE(getOption("gSD.aoi_set"))){
      aoi <- getOption("gSD.aoi")
    } else{
      out("Argument 'aoi' is undefined and no session AOI could be obtained. Define aoi or use set_aoi() to define a session AOI.", type = 3)
    }
  }
  
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
  #aoi.sf <- st_sfc(st_polygon(list(aoi.m)), crs = 4326)
  aoi.sf <- aoi
  aoi.sp <- as_Spatial(aoi.sf)
  
  if(type == "matrix") return(aoi.m)
  if(type == "sf") return(aoi.sf)
  if(type == "sp") return(aoi.sp)
}

#' checks if object has a crs
#' @param raster, sp or sf object
#' @return nothing. In case of flawed preview: error
#' @importFrom sf st_crs
#' @keywords internal
#' @noRd
.check_crs <- function(preview) {
  .check_rasterStack(preview, "preview")
}

#' checks if an error of a http request is likely to be related to an expired login
#' and tries to login with saved or given credentials.
#' @param response a caught error message.
#' @param record data.frame (single line used in the requested that returned an error).
#' @param username character username.
#' @param password character password.
#' @return nothing. In case of failed login: error.
#' @importFrom R.utils withTimeout
#' @keywords internal
#' @noRd
.check_http_error <- function(response, record, username = NULL, password = NULL, verbose = FALSE) {
  
  response_char <- as.character(response)
  is_http401_err <- all(c("http_401","error") %in% class(response)) && 
    grepl("unauthorized",response_char,ignore.case=T)
  
  if (is_http401_err) {
    
    service <- ifelse("Landsat" %in% record$product_group | "MODIS" %in% record$product_group,"usgs",
                      ifelse("Sentinel" %in% record$product_group,"dhus",NA))
    
    valid_input <- ifelse(is.na(service),FALSE,TRUE)
    
    if (valid_input) {
      
      if (any(is.null(c(username,password)))) {
        username <- getOption(paste0("gSD.",service,"_user"))
        password <- getOption(paste0("gSD.",service,"_pass"))
      }
      
      # login
      out("Renewing login..",msg=T,verbose=verbose)
      if (service=="usgs") {
        login <- try(withTimeout(login_USGS(username,password),onTimeout="silent",timeout=30))
      } else {
        login <- try(withTimeout(login_CopHub(username,password),onTimeout="silent",timeout=30))
      }
      
      if (inherits(login,"try-error")) {
        out(paste0("You are not logged in to ",
                   ifelse(service=="usgs","USGS ERS","Copernicus Hub")," (anymore). Please log in first using login_CopHub()."),3)
      }
      
    }
  }
  
}

#' checks the prio_sensors argument
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @return nothing. In case of flawed input in prio_sensors: error.
#' @keywords internal
#' @noRd
.select_check_prio_sensors <- function(prio_sensors) {
  if (!.is_empty_array(prio_sensors)) {
    .check_type(prio_sensors, "prio_sensors", "character")
    MODIS <- name_product_group_modis()
    optical_sensors <- get_cloudcov_supported() # because Sentinel-1 is not allowed for prio_products
    some_wrong <- isFALSE(any(sapply(prio_sensors, function(x) {
      check <- x %in% optical_sensors || x %in% c("Landsat", "MODIS")
      check <- ifelse(isTRUE(check), check, startsWith(x, MODIS))
    })))
    if (some_wrong) {
      out("Argument 'prio_products' has to be provided with sensor names in the 
        same format as returned by get_select_supported()",3)
    }
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
.select_check_revisit <- function(sensor, period, num_timestamps) {
  revisit_times <- list()
  revisit_times[[name_product_sentinel1()]] <- 4
  revisit_times[[name_product_sentinel2()]] <- 5
  revisit_times[[name_product_sentinel3()]] <- 2
  revisit_times[[name_product_landsatmss()]] <- 16
  revisit_times[[name_product_landsat5()]] <- 16
  revisit_times[[name_product_landsat7()]] <- 8
  revisit_times[[name_product_landsat8()]] <- 8
  revisit_times[[name_product_group_modis()]] <- 2
  revisit_times[["MODIS_MOD09A1_V6"]] <- 8
  revisit_times[["MODIS_MYD09A1_V6"]] <- 8
  revisit_times[["MODIS_MOD09Q1_V6"]] <- 8
  revisit_times[["MODIS_MOD09Q1_V6"]] <- 8
  revisit_times[["MODIS_MOD09GA_V6"]] <- 1
  revisit_times[["MODIS_MYD09GA_V6"]] <- 1
  revisit_times[["MODIS_MOD09GQ_V6"]] <- 1
  revisit_times[["MODIS_MYD09GQ_V6"]] <- 1
  revisit_times[["MODIS_MOD09CMG_V6"]] <- 1
  revisit_times[["MODIS_MYD09CMG_V6"]] <- 1
  r <- min(sapply(sensor,function(x) {revisit_times[[x]]}))
  sub_period <- (as.numeric(as.Date(period[2]) - as.Date(period[1]))) / num_timestamps
  info <- paste0("Selected number of timestamps (",num_timestamps)
  s <- ifelse(length(sensor)==1,"\n- Sensor: ","\nSensors: ")
  out(cat("\n- Number of timestamps selected:",num_timestamps,s,sensor))
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
.check_missing_columns <- function(records, cols) {
  
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
  
  dir_out <- .check_dir_out(dir_out)
  .check_verbose(verbose)
  aoi <- .check_aoi(aoi,"sf",quiet=T)
  # check if all columns are provided
  has_error <- .check_missing_columns(records,cols=c(par$aoi_cc_col,par$aoi_cc_prb_col,
                                                     par$preview_col,par$cloud_mask_col))
  if (has_error) out("Argument 'records' cannot be processed as it lacks needed columns/values",3)
  if (!is.null(prio_sensors)) .select_check_prio_sensors(prio_sensors)
  .select_check_revisit(unique(unlist(records$product)),period,num_timestamps)
  # check if needed files exist
  out("Checking if all needed clouds mask and preview rasters exist..",msg=T)
  check <- sapply(list(preview_file=records$preview_file,
                       cloud_mask_file=records$cloud_mask_file),function(x) {
    .select_check_files(x,names(x))
  })
  if (any(!is.na(check))) out("Cannot find (some) files on disk",3)
  
}

#' call .gsd_compact and return NA if list is empty afterwards
#' @param x list.
#' @return x without NAs and NULLs
#' @keywords internal
#' @noRd
.check_list <- function(x) {
  
  x <- .gsd_compact(x)
  if (length(x) == 0) return(NA) else return(x)
  
}


#' checks time_range argument
#' @param time_range char vector
#' @return nothing, breaks, if something is wrong
#' @keywords internal
#' @noRd
.check_time_range <- function(time_range){
  if(any(!is.character(time_range), length(time_range) != 2, all(nchar(time_range) != c(10,10)))){
    out("Argument 'time_range' must be a character vector containing two elements, formatted as c('YYYY-MM-DD', 'YYYY-MM-DD'), indicating start and stop date.", type=3)
  }
}

#' checks name argument
#' @param name char vector
#' @return nothing, breaks, if something is wrong
#' @keywords internal
#' @noRd
.check_products <- function(products, products_available = NULL){
  if(any(!is.character(products), length(products) < 1)){
    out("Argument 'products' must be a character containing at least one element.", type=3)
  } 
  if(!is.null(products_available)){
    products_valid <- unlist(sapply(products, function(x) any(grepl(x, products_available)), simplify = F))
    if(!all(products_valid)) out(paste0("Unknown product(s): ", paste0("'", paste0(products[!products_valid], collapse = "', '"), "'"), ". Please use get_products() to obtain the names of all available products."), type = 3)
  }
}

#' checks verbose argument
#' @param verbose logical
#' @return nothing
#' @keywords internal
#' @noRd
.check_verbose <- function(verbose) {
  .check_logical(verbose, "verbose")
  if (any(c(is.null(verbose), is.na(verbose)))) verbose <- TRUE # default value
  options(gSD.verbose = verbose)
}

#' checks input, generates a type error message and throws it if invalid
#' Does NOT check for NULL and NA. If any of these, test is skipped
#' @param input variable of any type
#' @param arg_name Character arg_name the name is it will appear in error message if raised
#' @param type Character type name
#' @return nothing, raises error
#' @keywords internal
#' @noRd
.check_type <- function(input, arg_name, type) {
  check_possible <- !is.null(input)
  check_possible <- ifelse(inherits(input, "list"), check_possible[[1]], check_possible[1])
  if (check_possible) {
    if (!class(input) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
      check_possible <- !is.na(input)
    }
    if (check_possible) {
      if (!inherits(input, type)) {
        out(paste0("Argument '", arg_name, "' must be of type '", type, "' but is '", class(input),"'"), 3)
      }
    }
  }
}

#' checks if input is numeric
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not numeric
#' @keywords internal
#' @noRd
.check_numeric <- function(input, arg_name) {
  .check_type(input, arg_name, "numeric")
}

#' checks if input is character
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not character
#' @keywords internal
#' @noRd
.check_character <- function(input, arg_name) {
  .check_type(input, arg_name, "character")
}

#' checks if input is data.frame
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not data.frame
#' @keywords internal
#' @noRd
.check_dataframe <- function(input, arg_name) {
  .check_type(input, arg_name, "data.frame")
}

#' checks if input is list
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not list
#' @keywords internal
#' @noRd
.check_list <- function(input, arg_name) {
  .check_type(input, arg_name, "list")
}

#' checks if input is RasterBrick or RasterStack
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not RasterBrick or RasterStack
#' @keywords internal
#' @noRd
.check_rasterStack <- function(input, arg_name) {
  .check_type(input, arg_name, "RasterStack' or 'RasterBrick")
}

#' checks if input is RasterLayer
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not RasterLayer
#' @keywords internal
#' @noRd
.check_raster <- function(input, arg_name) {
  .check_type(input, arg_name, "RasterLayer")
}

#' checks if input is logical
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not logical
#' @keywords internal
#' @noRd
.check_logical <- function(input, arg_name) {
  .check_type(input, arg_name, "logical")
}

#' checks if spatial object has a crs and assigns EPSG 4326 if not
#' @param sf, sp or raster (stack) object
#' @return input with crs. No assignment if it already has one
#' @keywords internal
#' @noRd
.check_crs <- function(x) {
  if (is.na(crs(x))) {
    if (is.na(st_crs(x))) {
      st_crs(x) <- st_crs(4326)
    } else {
      crs(x) <- st_crs(4326)$proj4string
    }
  }
  return(x)
}

#' checks if a record is a Sentinel-3 OLCI record
#' @param record data.frame one line
#' @return logical
#' @keywords internal
#' @noRd
.record_is_olci <- function(record) {
  return(strsplit(record[[name_record_id()]], "_")[[1]][2] == "OL")
}

#' checks if a record is a MODIS reflectance/radiance product
#' @param record data.frame one line
#' @return logical
#' @keywords internal
#' @noRd
.record_is_refl_modis <- function(record) {
  # e.g. 'MODIS_MCD19A1'
  return(any(startsWith(.cloudcov_products(), substr(record[[name_product()]], 1, 13)))) 
}




