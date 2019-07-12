#' @rdname get_records
#'
#' @importFrom getPass getPass
#' @importFrom httr content
#' @importFrom sf st_as_sfc st_sf
#'
#' @export

getLandsat_records <- function(time_range, name, aoi = NULL, as_sf = TRUE, rename_cols = TRUE, ..., verbose = TRUE){

  # downward compatibility
  if(missing(name)) name <- "all"
  
  ## check ... extras
  extras <- list(...)
  if(is.null(extras$level_filter)) extras$level_filter <- "all"
  if(is.null(extras$maxCloudScene)) extras$maxCloudScene <- 100
  if(is.null(extras$maxCloudLand)) extras$maxCloudLand <- 100
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
  aoi <- .make_aoi(aoi, type = "sf", quiet = T)

  ## check time_range and name
  char_args <- list(time_range = time_range, name = name)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(length(time_range) != 2) out("Argument 'time_range' must contain two elements (start and stop time).", type = 3)
  if(length(name) != 1) out("Argument 'name' must contain a single element of type character.", type = 3)
  
  if(name == "all") name <- getLandsat_names()
  meta.fields <- c("Start Time", "Stop Time", "WRS Path", "WRS Row", "Land Cloud Cover", "Scene Cloud Cover",
                   "Sun Elevation", "Sun Azimuth", "Sensor Identifier", "Image Quality")
  
  ## query
  records <- .EE_query(aoi, time_range, name, api.key, meta.fields)
  if(!is.null(records)){

    ## Connect to ESPA to revieve available products for (no use of entityId, displayId instead)
    out("Recieving available product levels from USGS-EROS ESPA...", msg = T)
    records <- do.call(rbind.data.frame, apply(records, MARGIN = 1, function(x, names = colnames(records)){
      
      x <- rbind.data.frame(x, stringsAsFactors = F)
      colnames(x) <- names
      
      tryCatch({
        response <- gSD.get(url = paste0(getOption("gSD.api")$espa, "available-products/", x$displayId), username = username, password = password)
        response <- if(all(names(content(response)) == "not_implemented")) "'l1'" else unlist(content(response)[[1]]$products) #paste0("'", paste0(content(t)[[1]]$products,  collapse = "', '"), "'")
      }, error = function(e) response <- "l1")
      
      x <- do.call(rbind.data.frame, rep(list(x), length(response)))
      x$level <- response
      return(x)
    }))
    
    # avail.products <- lapply(records$displayId, function(x){
    #   tryCatch({
    #     t <- gSD.get(url = paste0(getOption("gSD.api")$espa, "available-products/", x), username = username, password = password)
    #     if(all(names(content(t)) == "not_implemented")) return("'l1'") else paste0("'", paste0(content(t)[[1]]$products,  collapse = "', '"), "'")
    #     }, error = function(e) return("l1"))
    # })
    # records <- cbind(records, avail.products, stringsAsFactors = FALSE)
    # colnames(records)[ncol(records)] <- "levels_available"

    ## Correct WRS fields
    wrs.sub <- grep("WRS", colnames(records))
    records[,wrs.sub] <- apply(records[,wrs.sub], MARGIN = 2, function(x) sapply(x, function(y){
      z <- strsplit(y, " ")[[1]]
      z[length(z)]
    }, USE.NAMES = F))
    
    # convert expected numeric fields
    fields.numeric <- names(records)[sapply(names(records), function(x, y = c("WRSPath", "WRSRow", "LandCloudCover", "SceneCloudCover", "ImageQuality")) x %in% y, USE.NAMES = F)]
    records[,fields.numeric] <- sapply(fields.numeric, function(x) as.numeric(records[,x]))
    
    # only return levels defined in level_filter
    if(all(extras$level_filter != "all")){
      lfilter <- sapply(strsplit(extras$level_filter, ','), function(x) paste0("'", x, "'"))
      lcol <- paste(lfilter, collapse = ".*" )
      ind <- grep(lcol, records$levels_available)
      records <- records[ind,]
    }
    
    # cloud cover filter
    if(extras$maxCloudLand < 100 ){records <- records[records$LandCloudCover <= extras$maxCloudLand,]}
    if(extras$maxCloudScene < 100 ){records <- records[records$SceneCloudCover <= extras$maxCloudScene,]}
    
    # translate record column names
    records <- if(isTRUE(rename_cols)) .translate_records(records, name)
    
    # convert to sf
    colnames(records)[colnames(records) == "spatialFootprint"] <- "footprint"
    records$footprint <- st_as_sfc(records$footprint, crs = 4326)
    if(isTRUE(as_sf)) records <- st_sf(records, sfc_last = F)
    
    return(records)
  } else { out("No results could be obtained for this request.", msg = T) }
}


#' @rdname getSpatialData-deprecated
#' @export
getLandsat_query <- function(...){
  .Deprecated("getLandsat_records", "getSpatialData")
  getLandsat_records(...)
}
