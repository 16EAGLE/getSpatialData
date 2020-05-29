#' check logins
#'
#' @param records
#' 
#' @keywords internal
#' @noRd
.check_login <- function(services = NULL, records = NULL, verbose = F){
  
  if(is.null(services)){
    if(is.null(records)){
      services <- c("USGS", "Copernicus", "earthdata")
    } else{
      if("Landsat" %in% records$product_group) services <- c(services, "USGS")
      if("MODIS" %in% records$product_group) services <- c(services, "USGS", "earthdata")
      if("Sentinel" %in% records$product_group) services <- c(services, "Copernicus")
    }
  }
  
  if(all("Copernicus" %in% services, !getOption("gSD.dhus_set"))) out("You are not logged in to Copernicus Hub. Please login first using login_CopHub().", type = 3)
  if("USGS" %in% services){
    if(!getOption("gSD.usgs_set")) out("You are not logged in to USGS ERS. Please login first using login_USGS().", type = 3)
    
    # refresh session if needed
    if(difftime(Sys.time(), getOption("gSD.usgs_time"), units = "mins") > getOption("gSD.usgs_refresh")){
      login_USGS(getOption("gSD.usgs_user"), getOption("gSD.usgs_pass"), verbose = F)
    }
  }
  if("earthdata" %in% services){
    if(!getOption("gSD.ed_set")) out("You are not logged in to NASA URS EarthData. Please login first using login_earthdata().", type = 3)
    
    # refresh session if needed
    if(difftime(Sys.time(), getOption("gSD.ed_time"), units = "mins") > getOption("gSD.ed_refresh")){
      login_earthdata(getOption("gSD.ed_user"), getOption("gSD.ed_pass"), verbose = F)
    }
  }
}

#' check credentials
#'
#' @param username user
#' @param password passw
#' @param service service
#'
#' @keywords internal
#' @noRd
.check_credentials <- function(username, password, service){
  
  if(service == "Copernicus"){
    if(is.TRUE(getOption("gSD.dhus_set"))){
      if(is.null(username)) username <- getOption("gSD.dhus_user")
      if(is.null(password)) password <- getOption("gSD.dhus_pass")
    }
    if(!is.character(username)) out("Argument 'username' needs to be of type 'character'. You can use 'login_CopHub()' to define your login credentials globally.", type=3)
    if(!is.null(password)){ password = password} else{ password = getPass("Password (Copernicus Open Access Hub):")}
    api.key <- NULL
  }
  
  if(service == "USGS"){
    if(is.null(username)){
      if(is.TRUE(getOption("gSD.usgs_set"))){
        username <- getOption("gSD.usgs_user")
        password <- getOption("gSD.usgs_pass")
        api.key <- getOption("gSD.usgs_apikey")
      } else {
        out("Argument 'username' needs to be of type 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)
      }
    } else{
      if(is.null(password)) password = getPass("Password (USGS EROS Registration System):")
      api.key <- .ERS_login(username, password)
    }
  }
  return(list("username" = username, "password" = password, "api.key" = api.key))
}

#' check records
#'
#' @param records object.
#' @param col.names character vector.
#' @param as_df logical.
#'
#' @keywords internal
#' @noRd
.check_records <- function(records, col.names = NULL, as_sf = TRUE){
  .check_records_type(records)
  if(!is.null(col.names)){
    catch <- .lapply(col.names, function(x) if(!(x %in% colnames(records))) {
      out(paste0("A column of 'records' named '", x, "' is required for this action, but is missing."), type = 3)
    })
    rm(catch)
  }
  if (as_sf) {
    records <- st_sf(records, sfc_last = F)
  } else if (!as_sf || !name_footprint() %in% names(records)) {
    records <- as.data.frame(records) 
  }
  return(records)
}

#' checks the records type (special case: 'data.frame' or 'sf')
#' @param records data.frame or sf
#' @keywords internal
#' @noRd
.check_records_type <- function(records) {
  correct <- inherits(records, "data.frame")
  if (!correct) {
    out("Argument 'records' must be of class 'data.frame' or 'sf' 'data.frame'.", type = 3)
  }
}

#' check dir_out
#'
#' @param dir_out
#'
#' @keywords internal
#' @noRd
.check_dir_out <- function(dir_out, which = NULL){
  
  msg1 <- "Directory 'dir_out' is neither set through 'set_archive' nor provided as argument"
  msg2 <- "Directory 'dir_out' does not exist: "
  
  archive_set <- is.TRUE(getOption("gSD.archive_set"))
  argument_set <- !is.null(dir_out)
  
  if (!archive_set && !argument_set) out(msg1, 3)
  if (argument_set) .check_character(dir_out, "dir_out")
  
  ## Check output directory
  if(archive_set){
    if(is.null(dir_out)) dir_out <- getOption(paste0("gSD.archive", if(!is.null(which)) paste0("_", which)))
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  
  dir_out <- path.expand(dir_out)
  if (dir.exists(dir_out)) {
    return(path.expand(dir_out))
  } else {
    # Be careful when changing message, it is checked on in unit tests
    out(paste0(msg2, dir_out), 3)
  }
}

# #' check if command can be executed on the system command line
# #' @param cmd command
# #' @importFrom processx process
# #' @keywords internal
# #' @noRd
# .check_cmd <- function(cmd){
#   sc <- try(processx::process$new(cmd),silent = TRUE)
#   if(inherits(sc, "try-error")){return(FALSE)}else{return(TRUE)}
# }

#' check and create aoi from aoi argument
#'
#' @param aoi aoi
#' @keywords internal
#' @importFrom sf st_sfc st_polygon st_crs st_as_sf st_coordinates st_transform st_crs<- as_Spatial st_zm
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
  aoi.sf <- st_union(st_zm(aoi))
  st_crs(aoi.sf) <- st_crs(4326)
  aoi.sp <- as_Spatial(aoi.sf) # st_zm drops z dim if given
  
  if(type == "matrix") return(aoi.m)
  if(type == "sf") return(aoi.sf)
  if(type == "sp") return(aoi.sp)
}

#' checks if an error of a http request is likely to be related to an expired login
#' and tries to login with saved or given credentials.
#' @param response a caught error message.
#' @param record data.frame (single line used in the requested that returned an error).
#' @param username character username.
#' @param password character password.
#' @return nothing. In case of failed login: error.
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
        login <- try(login_USGS(username,password))
      } else {
        login <- try(login_CopHub(username,password))
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
#' @param records sf data.frame
#' @return nothing. In case of flawed input in prio_sensors: error.
#' @keywords internal
#' @noRd
.select_check_prio_sensors <- function(prio_sensors, records) {
  if (!.is_empty_array(prio_sensors)) {
    .check_type(prio_sensors, "prio_sensors", CHARACTER())
    # check if prio products are given in records at all
    not_in_product_group <- !any(prio_sensors %in% records[[name_product_group()]])
    not_in_product <- !any(prio_sensors %in% records[[name_product()]])
    prio_prods_not_in_records <- not_in_product && not_in_product_group
    if (prio_prods_not_in_records) out("No product name provided in 'prio_products' existing in 'records'", 3)
    MODIS <- name_product_group_modis()
    optical_sensors <- get_cloudcov_supported() # because Sentinel-1 is not allowed for prio_products
    all_valid <- all(sapply(prio_sensors, function(x) {
      check <- x %in% optical_sensors || x %in% c(name_product_group_landsat(), name_product_group_modis())
      check <- ifelse(isTRUE(check), check, startsWith(x, MODIS))
    }))
    if (name_product_sentinel1() %in% prio_sensors) out(paste0(name_product_sentinel1(), " cannot be handled in 'prio_products'"))
    if (!all_valid) {
      out("Argument 'prio_products' has to be provided with sensor names in the 
        same format as returned by get_select_supported()", 3)
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
  
  paths <- paths[intersect(which(!is.na(paths)), which(paths != "NONE"))]
  exist <- .sapply(paths,function(p) file.exists(p))
  
  all_on_disk <- isTRUE(all(exist))
  if (all_on_disk) {
    return(NA)
  } else {
    number_not_found <- which(!exist)
    out(
      paste0("All files in '", item_name, "' have to be saved at the location as indicated
             in the paths. ", number_not_found, " of ", length(paths), " files cannot be found"), 2)
  }
}

#' creates an error if requested coverage is higher than sensor revisit time
#' @param sensors character vector of sensor(s).
#' @param period character vector of start and end date.
#' @param num_timestamps numeric number of timestamps. 
#' @return nothing. Console communication
#' @keywords internal
#' @noRd
.select_check_revisit <- function(sensors, period, num_timestamps) {
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

  sel_num_timestamps <- "Number timestamps"
  s <- ifelse(length(sensors)==1,"\nProduct: ","\nProducts: ")
  n_spaces <- 4
  one_space <- " "
  spaces1 <- paste(rep(one_space, times = n_spaces), collapse="")
  spaces2 <- paste(rep(one_space, times=nchar(sel_num_timestamps) + 3 - nchar(s) + n_spaces), collapse="")
  out(paste0(sel_num_timestamps, ": ", spaces1, num_timestamps, s, spaces2, paste(sensors, collapse=", ")))
  for (sensor in sensors) {
    r <- min(sapply(sensor,function(x) {revisit_times[[x]]}))
    sub_period <- (as.numeric(as.Date(period[2]) - as.Date(period[1]))) / num_timestamps
    not_unitemporal <- num_timestamps > 1
    info <- paste0(sel_num_timestamps, " (",num_timestamps)
    if (sub_period < r && not_unitemporal) {
      out(paste0(info,") results in shorter coverage frequency than sensor revisit time (",r," days). Decrease 'num_timestamps'"), 3)
    } else if (sub_period == r && not_unitemporal) {
      out(paste0(info,") results in coverage frequency equal to revisit time (", r, " days) of product '", 
                 sensor, "'"), 2)
    }
  }
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
                           params, dir_out, verbose) {
  dir_out <- .check_dir_out(dir_out, "select")
  aoi <- .check_aoi(aoi, SF(), quiet=T)
  # check if all columns are provided
  needed_cols <- c(params$aoi_cc_col, params$preview_col, params$cloud_mask_col)
  if (.has_SAR(records[[name_product()]]) != 100) {
    checked <- .check_records(records, col.names = needed_cols)
    rm(checked)
  }
  if (!is.null(prio_sensors)) .select_check_prio_sensors(prio_sensors, records)
  .select_check_revisit(unique(unlist(records$product)), period, num_timestamps)
  # check if needed files exist
  check <- sapply(list(preview_file=records$preview_file,
                       cloud_mask_file=records$cloud_mask_file),function(x) {
    .select_check_files(x, names(x))
  })
  if (any(!is.na(check))) out("Cannot find (some) files on disk",3)
}

#' checks if a driver name is available
#' @param driver character driver name.
#' @return nothing. Throws error in case driver cannot be found.
#' @importFrom sf st_drivers
#' @keywords internal
#' @noRd
.check_gdal_driver <- function(driver) {
  throw_error <- !is.character(driver)
  if (!throw_error) {
    drivers <- tolower(as.vector(names(get_records_drivers())))
    throw_error <- !tolower(driver) %in% drivers
  }
  if (throw_error) out(paste0("Driver: '", driver, "' cannot be found. Use a driver given in get_records_drivers()"), 3)
}

#' call .gsd_compact and return NA if list is empty afterwards
#' @param x list.
#' @return x without NAs and NULLs
#' @keywords internal
#' @noRd
.check_compact_list <- function(x) {
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
    products_valid <- unlist(.sapply(products, function(x) any(grepl(x, products_available)), simplify = F))
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
  .set_verbose(verbose)
}

#' checks as_sf argument
#' @param as_sf logical
#' @return nothing
#' @keywords internal
#' @noRd
.check_as_sf <- function(as_sf) {
  .check_logical(as_sf, "as_sf")
}

#' checks preview and returns if it is broken (no values above DN 20)
#' @param RasterStack preview
#' @return logical if preview is spoiled
#' @importFrom raster nlayers
#' @keywords internal
#' @noRd
.check_preview <- function(preview) {
  max <- 20
  if (nlayers(preview) < 3) out("Preview is not RGB", 3)
  .check_rasterStack(preview, "preview")
  if (all(cellStats(preview, "max") < max)) {
    prev_vals <- as.integer(as.vector(values(preview)))
    return(all(prev_vals < max))
  } else {
    return(FALSE)
  }
}

#' checks if preview has valid observations (optinally in aoi)
#' @param preview raster stack
#' @param record sf
#' @param aoi sf
#' @return preview raster stack
#' @keywords internal
#' @noRd
.preview_has_valid_values <- function(preview, record, aoi = NULL) {
  # Check if preview is broken
  is_not_broken <- !.check_preview(preview)
  # Check for valid observations in aoi
  if (!is_not_broken) {
    if (!is.null(aoi)) {
      preview <- .mask_raster_by_polygon(preview, aoi)
      is_not_broken <- !.check_preview(preview)
    }
  }
  return(is_not_broken)
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
  raster_classes <- c(RASTER_LAYER(), RASTER_STACK(), RASTER_BRICK())
  is_raster <- any(class(input) %in% raster_classes)
  check_possible <- !is.null(input)
  is_list <- inherits(check_possible, LIST())
  check_possible <- ifelse(is_list, check_possible[[1]], check_possible[1])
  if (check_possible) {
    if (!is_raster) {
      check_possible <- !is.na(input)
      check_possible <- ifelse(is_list, check_possible[[1]], check_possible[1])
    }
    if (check_possible) {
      if (!inherits(input, type)) {
        if (is_raster) type <- paste0(raster_classes[1], "' or '")
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
  .check_type(input, arg_name, NUMERIC())
}

#' checks if input is character
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not character
#' @keywords internal
#' @noRd
.check_character <- function(input, arg_name) {
  .check_type(input, arg_name, CHARACTER())
}

# #' checks if input is data.frame
# #' @param input variable of any type
# #' @param character arg_name the name as it will appear in error message if raised
# #' @return nothing, raises error if input is not data.frame
# #' @keywords internal
# #' @noRd
.check_dataframe <- function(input, arg_name) {
  .check_type(input, arg_name, DATAFRAME())
}

# #' checks if input is list
# #' @param input variable of any type
# #' @param character arg_name the name as it will appear in error message if raised
# #' @return nothing, raises error if input is not list
# #' @keywords internal
# #' @noRd
.check_list <- function(input, arg_name) {
  .check_type(input, arg_name, LIST())
}

#' checks if input is RasterBrick or RasterStack
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not RasterBrick or RasterStack
#' @keywords internal
#' @noRd
.check_rasterStack <- function(input, arg_name) {
  if (!inherits(input, RASTER_STACK()) && !inherits(input, RASTER_BRICK())) {
    .check_type(input, arg_name, RASTER_STACK())
  }
}

# #' checks if input is RasterLayer
# #' @param input variable of any type
# #' @param character arg_name the name as it will appear in error message if raised
# #' @return nothing, raises error if input is not RasterLayer
# #' @keywords internal
# #' @noRd
.check_raster <- function(input, arg_name) {
  .check_type(input, arg_name, RASTER_LAYER())
}

#' checks if input is logical
#' @param input variable of any type
#' @param character arg_name the name as it will appear in error message if raised
#' @return nothing, raises error if input is not logical
#' @keywords internal
#' @noRd
.check_logical <- function(input, arg_name) {
  .check_type(input, arg_name, LOGICAL())
}

#' checks if spatial object has a crs and assigns EPSG 4326 if not
#' @param sf, sp or raster (stack) object
#' @return input with crs. No assignment if it already has one
#' @keywords internal
#' @noRd
.check_crs <- function(x) {
  if (is.na(crs(x))) {
    is_sfc <- .is_sfc(x)
    is_sf <- .is_sf(x)
    if (any(is_sfc, is_sf) && is.na(st_crs(x))) {
      st_crs(x) <- st_crs(4326)
    } else if (!is_sfc && !is_sf) {
      crs(x) <- st_crs(4326)$proj4string
    }
  }
  return(x)
}

#' checks if a file exists on disk
#' @param file character file path
#' @param out_type integer in case of FALSE: what type of out to be thrown? NA for nothing.
#' @return logical TRUE if file exists
#' @importFrom utils file_test
#' @keywords internal
#' @noRd
.check_file_exists <- function(file, out_type = NA) {
  .check_character(file, "internal file")
  exists <- file_test("-f", file)
  if (exists) {
    return(exists)
  } else {
    if (!is.na(out_type)) out(paste0("File does not exist: ", file), out_type)
    return(exists)
  }
}

# logical type checks

#' check if inherits sf
#' @param x of any type
#' @return logical if it inherits sf
#' @keywords internal
#' @noRd
.is_sf <- function(x) {
  return(inherits(x, SF()))
}

#' check if inherits sfc
#' @param x of any type
#' @return logical if it inherits sfc
#' @keywords internal
#' @noRd
.is_sfc <- function(x) {
  return(inherits(x, SFC()))
}


#' check if url
#' @param url a url
#' @keywords internal
#' @noRd
is.url <- function(url) grepl("www.|http:|https:", url)

#' Simplifies check of variables being TRUE
#'
#' @param evaluate variable or expression to be evaluated
#'
#' @keywords internal
#' @noRd
is.TRUE <- isTRUE <- function (x) is.logical(x) && length(x) == 1L && !is.na(x) && x

