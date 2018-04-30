#' Query Landsat data
#'
#' \code{getLandsat_query} queries the USGS Earth Explorer for Landsat data by some basic input search parameters. The function returns a data frame that can be further filtered.
#'
#' @inheritParams getSentinel_query
#' @param name character, optional. Identifies the name of the dataset to be queried. If set to "all" (default), every available Landsat dataset is searched for results and included in the output. Use \link{getLandsat_names} to revcieve a vector with all available Landsat datasets from Earth Explorer, if you want to select a specific one.
#'
#' @return A data frame; each row represents one dataset. The data frame can be further filtered by its columnwise attributes. The selected rows can be handed over to the other getLandsat functions for previewing or downloading.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom httr content
#'
#' @seealso \link{getLandsat_names} \link{getLandsat_preview} \link{getLandsat_data}
#' @export

getLandsat_query <- function(time_range, name = "all" , aoi = NULL, username = NULL, password = NULL, verbose = TRUE){

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
    api.key <- usgs_login(username, password)
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
  aoi <- make_aoi(aoi, type = "sf", quiet = T)

  ## check time_range and name
  char_args <- list(time_range = time_range, name = name)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(length(time_range) != 2) out("Argument 'time_range' must contain two elements (start and stop time).", type = 3)
  if(length(name) != 1) out("Argument 'name' must contain a single element of type character.", type = 3)

  if(name == "all") name <- getLandsat_names()
  meta.fields <- c("Start Time", "Stop Time", "WRS Path", "WRS Row", "Land Cloud Cover", "Scene Cloud Cover",
                   "Sun Elevation", "Sun Azimuth", "Sensor Identifier", "Image Quality")

  return.df <- usgs_query(aoi, time_range, name, api.key, meta.fields)
  if(!is.null(return.df)){

    ## Connect to ESPA to revieve available products for dataset results (no use of entityId, displayId instead)
    out("Recieving available product levels from USGS-EROS ESPA...")
    avail.products <- as.character(sapply(return.df$displayId, function(x){
      t <- gSD.get(paste0(getOption("gSD.api")$espa, "available-products/", x), getOption("gSD.usgs_user"), getOption("gSD.usgs_pass"))
      paste0("'", paste0(content(t)[[1]]$products,  collapse = "', '"), "'")
    }, USE.NAMES = F))
    return.df <- cbind(return.df, avail.products, stringsAsFactors = FALSE)
    colnames(return.df)[ncol(return.df)] <- "levels_available"

    ## Correct WRS fields
    wrs.sub <- grep("WRS", colnames(return.df))
    return.df[,wrs.sub] <- apply(return.df[,wrs.sub], MARGIN = 2, function(x) sapply(x, function(y){
      z <- strsplit(y, " ")[[1]]
      z[length(z)]
    }, USE.NAMES = F))

    return(return.df)
  } else { out("No results could be obtained for this request.", msg = T) }
}
