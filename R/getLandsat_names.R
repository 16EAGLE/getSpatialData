#' Get Landsat product names from USGS Earth Explorer
#'
#' \code{getLandsat_names} obtains names of available Landsat products from the USGS Earth Explorer. They can optionally be used with the \link{getLandsat_query} function for querying a specific Landsat product instead of all.
#' @inheritParams getLandsat_query
#'
#' @return A character vector
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
#'
#' @importFrom getPass getPass
#'
#' @seealso \link{getLandsat_query}
#' @export
#'
getLandsat_names <- function(username = NULL, password = NULL){

  if(is.null(username)){
    if(is.TRUE(getOption("gSD.usgs_set"))){
      api.key <- getOption("gSD.usgs_apikey")
    } else {
      out("Argument 'username' needs to be of type 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)
    }
  } else{
    if(is.null(password)) password = getPass()
    api.key <- .ERS_login(username, password)
  }
  .EE_ds(api.key, "LANDSAT_")
}
