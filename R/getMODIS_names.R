#' Get MODIS product names from USGS Earth Explorer
#'
#' \code{getMODIS_names} obtains names of available MODIS products from USGS Earth Explorer. They can optionally be used with the \link{getMODIS_query} function for querying a specific MODIS product instead of all.
#'
#' @inheritParams getLandsat_names
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
#' @importFrom MODIS getProduct
#'
#' @seealso \link{getMODIS_query}
#' @export
#'
getMODIS_names <- function(username = NULL, password = NULL){

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

  mp <- as.character(getProduct()$PRODUCT)
  ee.names <- .EE_ds(api.key, "MODIS_")
  ee.products <- .convMODIS_names(ee.names)
  ap <- intersect(mp, ee.products)
  ee.names[sapply(ap, function(x, ee.p = ee.products) which(ee.p == x), USE.NAMES = F)]
}
