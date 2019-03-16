#' Query Landsat data
#'
#' \code{getLandsat_query} queries the USGS Earth Explorer for Landsat data by some basic input search parameters. The function returns a data frame that can be further filtered.
#'
#' @inheritParams getSentinel_query
#' @param name character, optional. Identifies the name of the product to be queried. If set to "all" (default), every available Landsat product is searched for results and included in the output. Use \link{getLandsat_names} to revcieve a vector with all available Landsat products from Earth Explorer, if you want to select a specific one.
#' @param username character, a valid user name to the USGS EROS Registration System (ERS). If \code{NULL} (default), the session-wide login credentials are used (see \link{login_USGS} for details on registration).
#' @param password character, the password to the specified user account. If \code{NULL} (default) and no seesion-wide password is defined, it is asked interactively ((see \link{login_USGS} for details on registration).
#' @param ... one of the following filters by which the collected records should be filtered before return:
#' \itemize{
#'   \item \code{level_filter}, character, filtering records by processing level. Either "all" for all levels (default) or a vector of levels
#'   \item \code{maxCloudScene}, numeric, filtering records by maximum scene cloud coverage (in percent). Default is 100.
#'   \item \code{maxCloudLand}, numeric, filtering records by maximum land cloud coverage (in percent). Default is 100.
#' }
#' 
#' @return A data frame of found records. Each row represents one record. The data frame can be further filtered by its columnwise attributes. The selected rows can be handed over to the other getLandsat functions for previewing or downloading.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @examples
#' ## Load packages
#' library(getSpatialData)
#' library(sf)
#'
#' ## set aoi and time range for the query
#' set_aoi(aoi_data[[1]])
#' time_range <-  c("2017-08-01", "2017-08-30")
#'
#' ## Login to USGS ERS
#' \dontrun{
#' login_USGS("username")
#'
#' ## set archive directory
#' set_archive("/path/to/archive/")
#'
#' ## get available products and select one
#' product_names <- getLandsat_names()
#'
#' ## query for records for your AOI, time range and product
#' query <- getLandsat_query(time_range = time_range, name = product_names[7])
#'
#' ## preview a record
#' getLandsat_preview(query[5,])
#'
#' #print available levels for a record
#' query[5,]$levels_available
#'
#' ## download record 5 with level "l1" (will direct to AWS automaticaly)
#' files <- getLandsat_data(records = query[5,], level = "l1", source = "auto")
#'
#' ## download record 5 with level "sr" (will be processed on demand by ESPA)
#' files <- getLandsat_data(records = query[5,], level = "sr", source = "auto")
#' # this can take very long, since the function will wait,
#' # until the processing by ESPA is done
#'
#' ## you can abort the function while it is waiting for ESPA and resume later:
#' files <- getLandsat_data(espa_order = "espa-XYZA@host.com-YOUR-ORDER-ID")
#' # the order IDs are displayed and send by mail, use them to resume the task
#' }
#'
#' @importFrom getPass getPass
#' @importFrom httr content
#'
#' @seealso \link{getLandsat_names} \link{getLandsat_preview} \link{getLandsat_data}
#' @export

getLandsat_query <- function(time_range, name = "all" , aoi = NULL, username = NULL, password = NULL, ..., verbose = TRUE){

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
  
  ## check ... filters
  df.filters <- list(...)
  if(is.null(df.filters$level_filter)) df.filters$level_filter <- "all"
  if(is.null(df.filters$maxCloudScene)) df.filters$maxCloudScene <- 100
  if(is.null(df.filters$maxCloudLand)) df.filters$maxCloudLand <- 100
  
  ## query
  records <- .EE_query(aoi, time_range, name, api.key, meta.fields)
  if(!is.null(records)){

    ## Connect to ESPA to revieve available products for (no use of entityId, displayId instead)
    out("Recieving available product levels from USGS-EROS ESPA...")
    avail.products <- as.character(sapply(records$displayId, function(x){
      tryCatch({
        t <- gSD.get(url = paste0(getOption("gSD.api")$espa, "available-products/", x), username = username, password = password)
        if(all(names(content(t)) == "not_implemented")) return("'l1'") else paste0("'", paste0(content(t)[[1]]$products,  collapse = "', '"), "'")
        }, error = function(e) return("l1"))
    }, USE.NAMES = F))
    records <- cbind(records, avail.products, stringsAsFactors = FALSE)
    colnames(records)[ncol(records)] <- "levels_available"

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
    if(all(df.filters$level_filter != "all")){
      lfilter <- sapply(strsplit(df.filters$level_filter, ','), function(x) paste0("'", x, "'"))
      lcol <- paste(lfilter, collapse = ".*" )
      ind <- grep(lcol, records$levels_available)
      records <- records[ind,]
    }
    
    # cloud cover filter
    if(df.filters$maxCloudLand < 100 ){records <- records[records$LandCloudCover <= df.filters$maxCloudLand,]}
    if(df.filters$maxCloudScene < 100 ){records <- records[records$SceneCloudCover <= df.filters$maxCloudScene,]}
    
    return(records)
  } else { out("No results could be obtained for this request.", msg = T) }
}
