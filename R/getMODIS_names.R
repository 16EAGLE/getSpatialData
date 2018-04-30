#' Get MODIS dataset names from USGS Earth Explorer
#'
#' \code{getMODIS_names} obtains names of available MODIS datasets from USGS Earth Explorer. They can optionally be used with the \link{getMODIS_query} function for querying a specific MODIS dataset instead of all.
#'
#' @inheritParams getLandsat_names
#' @return A character vector
#'
#' @author Jakob Schwalb-Willmann
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
    api.key <- usgs_login(username, password)
  }

  mp <- as.character(getProduct()$PRODUCT)
  ee.names <- usgs_ds(api.key, "MODIS_")
  ee.products <- sapply(ee.names, function(x) paste0(tail(head(strsplit(x, "_")[[1]], n=-1), n=1), collapse = "_"), USE.NAMES = F)
  ap <- intersect(mp, ee.products)

  ee.names[sapply(ap, function(x, ee.p = ee.products) which(ee.p == x), USE.NAMES = F)]
}
