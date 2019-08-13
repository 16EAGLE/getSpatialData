#' @rdname get_records
#'
#' @importFrom getPass getPass
#' @importFrom httr content
#' @importFrom sf st_as_sfc st_sf
#'
#' @export

getMODIS_records <- function(time_range, name, aoi = NULL, as_sf = TRUE, rename_cols = TRUE, ..., verbose = TRUE){

  # downward compatibility
  if(missing(name)) name <- "all"
  
  ## check ... filters
  extras <- list(...)
  if(!is.null(extras$username)) username <- extras$username else username <- NULL
  if(!is.null(extras$password)) password <- extras$password else password <- NULL
  
  ## Global USGS login
  if(is.null(username)){
    if(is.TRUE(getOption("gSD.usgs_set"))){
      username <- getOption("gSD.usgs_user")
      password <- getOption("gSD.usgs_pass")
      api.key <- getOption("gSD.usgs_apikey")
    } else {
      out("Argument 'username' needs to be of type 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)
    }
  } else{
    if(is.null(password)) password = getPass()
    api.key <- .ERS_login(username, password)
  }
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)

  ## Global AOI
  if(is.null(aoi)){
    if(is.TRUE(getOption("gSD.aoi_set"))){
      aoi <- getOption("gSD.aoi")
    } else{
      out("Argument 'aoi' is undefined and no session AOI could be obtained. Define aoi or use set_aoi() to define a session AOI.", type = 3)
    }
  }
  aoi <- .check_aoi(aoi, type = "sf", quiet = T)

  ## check time_range and name
  char_args <- list(time_range = time_range, name = name)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(length(time_range) != 2) out("Argument 'time_range' must contain two elements (start and stop time).", type = 3)
  if(length(name) != 1) out("Argument 'name' must contain a single element of type character.", type = 3)

  if(name == "all") name <- getMODIS_names()
  meta.fields <- c("Acquisition Start Date", "Acquisition End Date", "Horizontal Tile Number", "Vertical Tile Number",
                   "Auto Quality Flag", "Auto Quality Flag Explanation", "Science Quality Flag",
                   "Science Quality Flag Expln","Missing Data Percentage")

  records <- .EE_query(aoi, time_range, name, api.key, meta.fields)
  
  # convert expected numeric fields
  fields.numeric <- names(records)[sapply(names(records), function(x, y = c("HorizontalTileNumber", "VerticalTileNumber", "MissingDataPercentage")) x %in% y, USE.NAMES = F)]
  records[,fields.numeric] <- sapply(fields.numeric, function(x) as.numeric(records[,x]))
  
  # translate record column names
  records <- if(isTRUE(rename_cols)) .translate_records(records, name)
  
  # convert to sf
  colnames(records)[colnames(records) == "spatialFootprint"] <- "footprint"
  records$footprint <- st_as_sfc(records$footprint, crs = 4326)
  if(isTRUE(as_sf)) records <- st_sf(records, sfc_last = F)
  
  if(!is.null(records)){ return(records)
  } else { out("No results could be obtained for this request.", msg = T) }
}


#' @rdname getSpatialData-deprecated
#' @export
getMODIS_query <- function(...){
  .Deprecated("getMODIS_records", "getSpatialData")
  getMODIS_records(...)
}
