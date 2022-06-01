#' Get product names
#'
#' \code{get_products} obtains the names of all products available through \code{getSpatialData}. The returned names can be used with the \link{get_records} functions for querying a specific product.
#' @param product_groups character, either "all" or a combination of product names. Supported names are "sentinel", "landsat", "modis", "srtm".
#' @param grouped logical, whether to return a list of names grouped by platform/sensor (\code{TRUE}) or not.
#' @param update_online logical, whether to update the internal product list online (default, requires login) or not.
#' @param ... deprecated arguments.
#' 
#' @note If \code{update_online = TRUE}, you must be logged in at the services required for your request to use this function. See the examples and \link{login} for details.
#' 
#' @section GNSS:
#' GNSS products (such as "sentinel-1_gnss") retrieved from the dual-frequency GPS recievers mounted on Sentinel-1, -2, and -3 represent a special type of product, as they are AOI-independent and thus only referenced by mission time. GNSS data originally have been used only to precisely calculate the satellites' orbits, but then have been released to the scientific public due to their potential scientifc uses (for details, see \url{https://earth.esa.int/web/sentinel/missions/sentinel-3/news/-/article/new-gnss-l1b-rinex-data-release-for-sentinel-1-2-and-3} and \url{https://earth.esa.int/documents/247904/351187/GMES_Sentinels_POD_Service_File_Format_Specification}).
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
#' 
#' # get all available products as one vector
#' products <- get_products()
#' 
#' # or grouped
#' products <- get_products(grouped = T)
#' products$sentinel
#' products$landsat
#' products$modis
#' 
#' # or get products by a specific service
#' products <- get_products("sentinel")
#' products <- get_products("landsat")
#' products <- get_products("modis")
#' 
#' }
#' @seealso getSentinel_records getLandsat_records getMODIS_records getSRTM_records
#' 
#' @name get_products
#' @export
get_products <- function(product_groups = "all", grouped = FALSE, update_online = FALSE){
  
  # get offline products list
  products <- .prod.list
  
  # login if required
  product_groups <- tolower(sort(product_groups))
  if(product_groups == "all") product_groups <- tolower(names(products))
  
  if(isTRUE(update_online)){
    if(any("landsat" %in% product_groups, "modis" %in% product_groups)){
      .check_login(services = c("USGS"))
      api.key <- getOption("gSD.usgs_apikey")
    }
    # offline
    if("sentinel" %in% product_groups){
      products[["sentinel"]] <- c(
        getOption("gSD.copnames")$name[getOption("gSD.copnames")$name != "gnss"], 
        paste0(getOption("gSD.copnames")$name[getOption("gSD.copnames")$name != "gnss" & getOption("gSD.copnames")$name != "sentinel-5p"], "_gnss")
      )
    }
    # online
    if("landsat" %in% product_groups){
      x <- grep("LSR", .EE_ds(api.key, wildcard = "landsat_"), value = T, invert = T) #not show LSR, since higher level products are queried at ESPA directly
      x <- x[grepl("landsat", x)] # CONSIDER REMOVING!
      x <- x[!grepl("band", x)] # CONSIDER REMOVING!
      if(length(x) == 0) out("Product names could not be accessed, are you (still) logged in? USGS ERS sessions expire after some time, use login_USGS() to (re-)login.", type = 3)
      products[["landsat"]] <-  x
    }
    # online
    if("modis" %in% product_groups){
      x <- .EE_ds(api.key, "modis_")
      x <- x[grepl("modis", x)] # CONSIDER REMOVING!
      if(length(x) == 0) out("Product names could not be accessed, are you (still) logged in? USGS ERS sessions expire after some time, use login_USGS() (re-)login.", type = 3)
      products[["modis"]] <-  x
    }
    # offline
    if("srtm" %in% product_groups){
      products[["modis"]] <- grep("srtm", names(.getCMR_id()))
    }
  }
  
  # set option
  options("gSD.products" = products)
  
  # return only requested products groups
  products <- products[product_groups]
  if(isTRUE(grouped)) products else unlist(products, use.names = F)
}

#' @rdname get_products
#' @export
getSentinel_products <- function(){
  get_products("sentinel")
}

#' @rdname get_products
#' @export
getLandsat_products <- function(..., update_online = TRUE){
  get_products("landsat", update_online = update_online)
}

#' @rdname get_products
#' @export
getMODIS_products <- function(..., update_online = TRUE){
  get_products("modis", update_online = update_online)
}

#' @rdname get_products
#' @export
getSRTM_products <- function(..., update_online = TRUE){
  get_products("srtm", update_online = update_online)
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