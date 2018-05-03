#' Query MODIS data
#'
#' \code{getMODIS_query} queries USGS Earth Explorer for MODIS data by some basic input search parameters. The function returns a data frame that can be further filtered.
#'
#' @inheritParams getLandsat_query
#' @param name character, optional. Identifies the name of the product to be queried. If set to "all" (default), every available MODIS product is searched for results and included in the output. Use \link{getMODIS_names} to revcieve a vector with all available MODIS products from Earth Explorer, if you want to select a specific one.
#'
#' @return A data frame of records. Each row represents one record. The data frame can be further filtered by its columnwise attributes. The selected rows can be handed over to the other getMODIS functions for previewing or downloading.
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
#' product_names <- getMODIS_names()
#' product <- grep("MOD13Q1", product_names, value = T)
#'
#' ## query for records for your AOI, time range and product
#' query <- getMODIS_query(time_range = time_range, name = product)
#'
#' ## preview a record
#' getMODIS_preview(query[1,])
#'
#' ## download records 1 and 2
#' files <- getMODIS_data(query[1:2,])
#' }
#'
#' @importFrom getPass getPass
#' @importFrom httr content
#'
#' @seealso \link{getMODIS_names} \link{getMODIS_preview} \link{getMODIS_data}
#' @export

getMODIS_query <- function(time_range, name = "all" , aoi = NULL, username = NULL, password = NULL, verbose = TRUE){

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

  if(name == "all") name <- getMODIS_names()
  meta.fields <- c("Acquisition Start Date", "Acquisition End Date", "Horizontal Tile Number", "Vertical Tile Number",
                   "Auto Quality Flag", "Auto Quality Flag Explanation", "Science Quality Flag",
                   "Science Quality Flag Expln","Missing Data Percentage")

  return.df <- .EE_query(aoi, time_range, name, api.key, meta.fields)
  if(!is.null(return.df)){ return(return.df)
  } else { out("No results could be obtained for this request.", msg = T) }
}
