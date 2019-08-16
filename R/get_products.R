#' Get product names
#'
#' \code{get_products} obtains the names of all products available through \code{getSpatialData}. The returned names can be used with the \link{get_records} functions for querying a specific product.
#' @inheritParams login
#' @param grouped logical, whether to return a list of names grouped by platform/sensor (\code{TRUE}) or not.
#' 
#' @note To use this function, you must be logged in at the services required for your request. See the examples and \link{login} for details.
#' 
#' @return A character vector
#'
#' @author Jakob Schwalb-Willmann
#' @examples
#' \dontrun{
#' library(getSpatialData)
#' 
#' # login
#' login_USGS(username = "your_username")
#' login_CopHub(username = "your_username")
#' 
#' # get all available products
#' products <- get_products()
#' products$Sentinel
#' products$Landsat
#' products$MODIS
#' 
#' # or get products by service
#' products <- getSentinel_products()
#' products <- getLandsat_products()
#' products <- getMODIS_products()
#' }
#' @seealso getSentinel_records getLandsat_records getMODIS_records
#' 
#' @name get_products
#' @export
get_products <- function(grouped = FALSE){
  .check_login(services = c("USGS", "Copernicus"))
  products <- list("Sentinel" = getSentinel_products(),
                   "Landsat" = getLandsat_products(),
                   "MODIS" = getMODIS_products())
  if(isTRUE(grouped)) products else unlist(products, use.names = F)
}

#' @rdname get_products
#' @export
getSentinel_products <- function(){
  getOption("gSD.copnames")$name[getOption("gSD.copnames")$name != "GNSS"]
}

#' @rdname get_products
#' @export
getLandsat_products <- function(username = NULL, password = NULL){
  
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
  x <- grep("LSR", .EE_ds(api.key, "LANDSAT_"), value = T, invert = T) #not show LSR, since higher level products are queried at ESPA directly
  if(length(x) == 0) out("Product names could not be accessed, are you (still) logged in? USGS ERS sessions expire after some time, use login_USGS() or define arguments 'username' and 'password'.", type = 3) else return(x)
}

#' @rdname get_products
#' @export
getMODIS_products <- function(username = NULL, password = NULL){
  
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
  
  .EE_ds(api.key, "MODIS_") # no comparison so far
}

#' @rdname getSpatialData-deprecated
#' @export
getSentinel_names <- function(...){
  .Deprecated("getSentinel_products", "getSpatialData")
  getSentinel_products(...)
}

#' @rdname getSpatialData-deprecated
#' @export
getLandsat_names <- function(...){
  .Deprecated("getLandsat_products", "getSpatialData")
  getLandsat_products(...)
}

#' @rdname getSpatialData-deprecated
#' @export
getMODIS_names <- function(...){
  .Deprecated("getMODIS_products", "getSpatialData")
  getMODIS_products(...)
}