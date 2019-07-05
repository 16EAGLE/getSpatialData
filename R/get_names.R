#' Get product names
#'
#' \code{get_names} obtains names of available products from included services. The returned names can be used with the \link{get_records} functions for querying a specific product.
#' @inheritParams gSD_login
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
#' products <- get_names()
#' products$Sentinel
#' products$Landsat
#' products$MODIS
#' 
#' # or get products by service
#' products <- getSentinel_names()
#' products <- getLandsat_names()
#' products <- getMODIS_names()
#' }
#' @seealso getSentinel_records getLandsat_records getMODIS_records
#' 
#' @name get_names
#' @export
get_names <- function(grouped = FALSE){
  .check_login(c("USGS", "Copernicus"))
  products <- list("Sentinel" = getSentinel_names(),
                   "Landsat" = getLandsat_names(),
                   "MODIS" = getMODIS_names())
  if(isTRUE(grouped)) products else unlist(products, use.names = F)
}

#' @rdname get_names
#' @export
getSentinel_names <- function(){
  getOption("gSD.copnames")$name[getOption("gSD.copnames")$name != "GNSS"]
}

#' @rdname get_names
#' @export
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
  x <- grep("LSR", .EE_ds(api.key, "LANDSAT_"), value = T, invert = T) #not show LSR, since higher level products are queried at ESPA directly
  if(length(x) == 0) out("Names could not be accessed, are you (still) logged in? USGS ERS sessions expire after some time, use login_USGS() or define arguments 'username' and 'password'.", type = 3) else return(x)
}

#' @rdname get_names
#' @export
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
  
  .EE_ds(api.key, "MODIS_") # no comparison so far
}