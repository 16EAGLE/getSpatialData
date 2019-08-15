#' Query available product records
#'
#' \code{get_records} queries a service for available records using basic input search parameters such as product name (see \code{\link{get_names}}), AOI and time range. The function returns a data frame of records that can be further filtered and that other \code{getSpatialData} functions use as input.
#' 
#' @param time_range character, a vector of two elements: the query's starting date and stopping date, formatted "YYYY-MM-DD", e.g. \code{c("2017-05-15", "2017-06-15")}
#' @param name character, product name(s). Use \code{\link{get_names}} to get a full list of all available products. If multiple products are supplied, the returned records are combined across products.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param as_sf logical, whether records should be returned as \code{sf} \code{data.frame} or a simple \code{data.frame}. In both cases, spatial geometries are stored in column \code{footprint}.
#' @param rename_cols logical, whether to rename columns to a product-independent standard to make it possible to combine records of different products recieved from different sources.
#' @param verbose logical, whether to display details on the function's progress or output on the console.
#' @param ... additional, sensor-specific arguments:
#' \itemize{
#'    \item \code{gnss}, logical, whether to query for Sentinel GNSS RINEX records instead of remote sensing instrument records. If \code{TRUE}, only records of the dual-frequency GPS recievers mounted on Sentinel-1, -2, and -3 are returned and \code{aoi} settings are ignored. If \code{FALSE} (default), remote sensing instrument records, queried including \code{aoi} settings, are returned (see section \code{Sentinel}).
#'    \item \code{hub}, character, Copernicus Hub selection for Sentinel queries. Either
#' \itemize{
#'    \item "auto" (default) to automatically select a suitable Copernicus hub depending on the selected products
#'    \item "dhus" to look for operational Open Hub records only,
#'    \item "s3" to look for Sentinel-3 pre-operational records only,
#'    \item "s5p" to look for Sentinel-5P precursor pre-operational records only,
#'    \item "GNSS" to look for GNSS RINEX records only,
#'    \item or a valid API URL.
#' }
#' }
#'
#' @return A data frame of records (by default an \code{sf} data frame, see argument \code{as_sf}). Each row represents one record. The data frame can be further filtered by its columnwise attributes or plotted to view their spatial footprints. The records data frame can be used as input to other \code{getSpatialData} functions.
#' 
#' @details
#' To use these functions, you need to be logged in at the required services: To query Sentinel records, login with your ESA Copernicus Open Access Hub credentials using \link{login_CopHub}. To query MODIS and Landsat records, login with your USGS EROS Registration System (ERS) credentials using \link{login_USGS}. See \code{\link{login}} for details.
#' 
#' @section GNSS:
#' If you are instead interested in (AOI-independent) GNSS records of the dual-frequency GPS recievers mounted on Sentinel-1, -2, and -3, set argument \code{gnss} to \code{TRUE}. GNSS data originally have been only used to precisely calculate the satellites' orbits, but then have been released to the scientific public due to their potential scientifc uses (for details, see \url{https://earth.esa.int/web/sentinel/missions/sentinel-3/news/-/article/new-gnss-l1b-rinex-data-release-for-sentinel-1-2-and-3} and \url{https://earth.esa.int/documents/247904/351187/GMES_Sentinels_POD_Service_File_Format_Specification}). 
#'
#' 
#' @author Jakob Schwalb-Willmann
#'
#' @export
#' 
#' @name get_records
#' @export
get_records <- function(time_range, name, aoi = NULL, as_sf = TRUE, rename_cols = TRUE, ..., verbose = TRUE){
  
  groups <- c("Sentinel", "Landsat", "MODIS")
  names.valid <- unlist(sapply(tolower(name), function(x) any(sapply(tolower(groups), grepl, x, USE.NAMES = F)), simplify = F, USE.NAMES = F))
  
  if(any(!names.valid)){
    out(paste0("Unknown product name(s): ", paste0("'", paste0(name[!names.valid], collapse = "', '"), "'"),
               ". Please use valid product names (call get_names() to get a list of all available product names)."), type = 3)
  } else{
    
    # select fun per name
    which.fun <- sapply(tolower(groups), grepl, tolower(name), simplify = F)
    groups <- groups[sapply(which.fun, any)]
    which.fun <- which.fun[sapply(which.fun, any)]
    
    # get records
    records <- mapply(x.fun = which.fun, x.name = name, function(x.fun, x.name){
      eval(parse(text = paste0("get", groups[x.fun], "_records(time_range = time_range, name = x.name, aoi = aoi, as_sf = as_sf, ..., verbose = verbose)")))
    }, USE.NAMES = F, SIMPLIFY = F)
    
    if(length(records) > 1){
      records <- rbind.different(records)
      return(records)
    } else{
      return(records[[1]])
    }
  }
}

#' @rdname get_records
#' 
#' @importFrom getPass getPass
#' @importFrom httr GET content
#' @importFrom xml2 xml_contents as_xml_document
#' @importFrom sf st_as_sfc st_sf
#' 
#' @export

getSentinel_records <- function(time_range, name, aoi = NULL, as_sf = TRUE, rename_cols = TRUE, ..., verbose = TRUE){
  
  ## check ... extras
  extras <- list(...)
  if(!is.null(extras$platform)) name <- extras$platform # support for deprecated argument name
  if(!is.null(extras$username)) username <- extras$username else username <- NULL
  if(!is.null(extras$password)) password <- extras$password else password <- NULL
  if(!is.null(extras$check_avail)) out("Argument 'check_avail' is deprecated. Use check_availability() to check whether records are available on-demand or not.", type = 2)
  if(!is.null(extras$hub)) hub <- extras$hub else hub <- "auto"
  if(!is.null(extras$gnss)) gnss <- extras$gnss else gnss <- FALSE
  
  ## Global Copernicus Hub login
  if(is.TRUE(getOption("gSD.dhus_set"))){
    if(is.null(username)) username <- getOption("gSD.dhus_user")
    if(is.null(password)) password <- getOption("gSD.dhus_pass")
  }
  if(!is.character(username)) out("Argument 'username' needs to be of type 'character'. You can use 'login_CopHub()' to define your login credentials globally.", type=3)
  if(!is.null(password)){ password = password} else{ password = getPass()}
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  
  ## Global AOI
  if(!isTRUE(gnss)){
    if(is.null(aoi)){
      if(is.TRUE(getOption("gSD.aoi_set"))){
        aoi <- getOption("gSD.aoi")
      } else{
        out("Argument 'aoi' is undefined and no session AOI could be obtained. Define aoi or use set_aoi() to define a session AOI.", type = 3)
      }
    }
    aoi <- st_as_sfc(st_bbox(.check_aoi(aoi, type = "sf"))) # create bounding box instead of checking npts
    aoi <- .check_aoi(aoi, type = "matrix")
  } else{
    aoi <- NULL
  }
  
  ## check time_range and platform
  char_args <- list(time_range = time_range, name = name)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(length(time_range) != 2){out("Argument 'time_range' must contain two elements (start and stop time).", type = 3)}
  if(!name %in% getSentinel_names()) out(paste0("The selected product name is not supported. Select either ", paste0("'", paste0(getSentinel_names(), collapse = "', '"), "'")), type = 3)
  
  ## url assembler function
  cop.url <- function(ext.xy, url.root, name, time.range, row.start){
    if(name == "Sentinel-5P") name <- "Sentinel-5"
    qs <- list(url.root = paste0(url.root, "/"),
               search = c("search?start=", "&rows=100&q="),  #"search?q=", #start=0&rows=1000&
               and = "%20AND%20",
               aoi.poly = c("footprint:%22Intersects(POLYGON((", ")))%22"),
               platformname = "platformname:",
               time = list("[" = "beginposition:%5b", "to" = "%20TO%20", "]" = "%5d"))
    time.range <- sapply(time.range, function(x) paste0(x, "T00:00:00.000Z"), USE.NAMES = F)
    if(!is.null(ext.xy)) aoi.str <- paste0(apply(ext.xy, MARGIN = 1, function(x) paste0(x, collapse = "%20")), collapse = ",")
    
    return(paste0(qs$url.root, qs$search[1], toString(row.start), qs$search[2], "(", if(!is.null(ext.xy)) paste0(qs$aoi.poly[1], aoi.str, qs$aoi.poly[2], qs$and) else "",
                  qs$platformname, name, qs$and,
                  qs$time$`[`, time.range[1], qs$time$to, time.range[2], qs$time$`]`, ")"))
  }
  
  ## Manage API access
  cred <- .CopHub_select(x = hub, p = if(isTRUE(gnss)) "GNSS" else name, user = username, pw = password)
  
  ## query API
  row.start <- -100; re.query <- T; give.return <- T
  query.list <- list()
  
  out(paste0("Searching records for product name '", name, "'..."))
  while(is.TRUE(re.query)){
    row.start <- row.start + 100
    
    query <- gSD.get(url = cop.url(ext.xy = aoi, url.root = cred[3], name = name, time.range = time_range, row.start = row.start), username = cred[1], password = cred[2])
    query.xml <- suppressMessages(xml_contents(as_xml_document(content(query, as = "text"))))
    query.list <- c(query.list, lapply(query.xml[grep("entry", query.xml)], function(x) xml_contents(x)))
    
    if(length(query.list) == 0 & row.start == 0){
      out("No results could be obtained for this request.", msg = T)
      re.query <- F
      give.return <- F
    }
    if(length(query.list) != row.start+100 ) re.query <- F
    #if(length(query.list) != 100) re.query <- F
  }
  
  ## build query result data frame
  if(is.TRUE(give.return)){
    
    ## predefine tags not having 'name' tag
    field.tag <- c("title", "link href", 'link rel="alternative"', 'link rel="icon"', "summary", "name")
    field.names <- c("title", "url", "url.alt", "url.icon", "summary")
    
    ## get field.tag contents and assemble unique names vector
    query.cont <- lapply(query.list, function(x, field = field.tag) unlist(lapply(field, function(f, y = x) grep(f, y, value = T))))
    query.names <- lapply(query.cont, function(x, field.n = field.names) c(field.n, sapply(x[(length(field.n)+1):length(x)], function(y) strsplit(y, '\"')[[1]][2], USE.NAMES = F)))
    
    ## make fields and treat url tags differently, as required field data is no content here
    query.fields <- lapply(query.cont, function(x) sapply(x, function(y) strsplit(strsplit(y, ">")[[1]][2], "<")[[1]][1], USE.NAMES = F))
    query.fields <- lapply(1:length(query.fields), function(i, qf = query.fields, qc = query.cont, qn = query.names){
      x <- qf[[i]]
      x[which(is.na(x) == T)] <- sapply(qc[[i]][which(is.na(x) == T)], function(y) strsplit(strsplit(y, 'href=\"')[[1]][2], '\"/>')[[1]][1], USE.NAMES = F)
      names(x) <- qn[[i]]
      return(x)
    })
    
    records.names <- unique(unlist(query.names))
    records <- as.data.frame(stats::setNames(replicate(length(records.names),numeric(0), simplify = F), records.names))
    records <- do.call(rbind.data.frame, lapply(query.fields, function(x, rn = records.names,  rdf = records){
      rdf[1, match(names(x), rn)] <- sapply(x, as.character)
      return(rdf)
    }))
    if(isTRUE(check_avail)) records$download_available <- as.logical(toupper(unlist(.get_odata(records$uuid, cred, field = "Online/$value"))))
    
    # convert expected numeric fields
    fields.numeric <- names(records)[sapply(names(records), function(x, y = c("orbitnumber", "relativeorbitnumber", "cloudcoverpercentage", "highprobacloudspercentage", "mediumprobacloudspercentage",
                                                                              "snowicepercentage", "vegetationpercentage", "waterpercentage", "baresoilpercentage", "lowprobacloudspercentage")) x %in% y, USE.NAMES = F)]
    records[,fields.numeric] <- sapply(fields.numeric, function(x) as.numeric(records[,x]))
    
    # translate record column names
    records <- if(isTRUE(rename_cols)) .translate_records(records, name)
    records$is_gnss <- gnss
    
    # sf geometry
    records$footprint <- st_as_sfc(records$footprint, crs = 4326)
    if(isTRUE(as_sf)) records <- st_sf(records, sfc_last = F)
    
    return(records)
  }
}

#' @rdname getSpatialData-deprecated
#' @export
getSentinel_query <- function(...){
  .Deprecated("getSentinel_records", "getSpatialData")
  getSentinel_records(...)
}

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
  aoi <- .check_aoi(aoi, type = "sf", quiet = T)
  
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
