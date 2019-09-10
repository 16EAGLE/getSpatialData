#' Get product names
#'
#' \code{get_products} obtains the names of all products available through \code{getSpatialData}. The returned names can be used with the \link{get_records} functions for querying a specific product.
#' @inheritParams login
#' @param product_groups character, either "all" or a combination of product names. Supported names are "Sentinel", "Landsat", "MODIS", "SRTM".
#' @param grouped logical, whether to return a list of names grouped by platform/sensor (\code{TRUE}) or not.
#' @param update_online logical, whether to update the internal product list online (default, requires login) or not.
#' 
#' @note If \code{update_online = TRUE}, you must be logged in at the services required for your request to use this function. See the examples and \link{login} for details.
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
#' products$Sentinel
#' products$Landsat
#' products$MODIS
#' 
#' # or get products by a specific service
#' products <- get_products("Sentinel")
#' products <- get_products("Landsat")
#' products <- get_products("MODIS")
#' 
#' }
#' @seealso getSentinel_records getLandsat_records getMODIS_records getSRTM_records
#' 
#' @name get_products
#' @export
get_products <- function(product_groups = "all", grouped = FALSE, update_online = TRUE){
  
  # assemble offline products list
  products <- getOption("gSD.products")
  if(is.null(products)){
    products <- list("Sentinel" = getOption("gSD.copnames")$name[getOption("gSD.copnames")$name != "GNSS"],
                     "Landsat" = c("LANDSAT_ETM_C1", "LANDSAT_MSS_C1", "LANDSAT_TM_C1", "LANDSAT_8_C1"),
                     "MODIS" = c("MODIS_MCD43D28_V6", "MODIS_MYD21A1N_V6", "MODIS_MCD43A1_V6", "MODIS_MCD43A2_V6", "MODIS_MCD43A3_V6", 
                                 "MODIS_MCD43A4_V6", "MODIS_MCD43C1_V6",  "MODIS_MCD43C2_V6", "MODIS_MCD43C3_V6", "MODIS_MCD43C4_V6", 
                                 "MODIS_MCD43D01_V6", "MODIS_MCD43D02_V6", "MODIS_MCD43D03_V6", "MODIS_MCD43D04_V6", "MODIS_MCD43D05_V6",
                                 "MODIS_MCD43D06_V6", "MODIS_MCD43D07_V6", "MODIS_MCD43D08_V6", "MODIS_MCD43D09_V6", "MODIS_MCD43D10_V6",
                                 "MODIS_MCD43D11_V6", "MODIS_MCD43D12_V6", "MODIS_MCD43D13_V6", "MODIS_MCD43D14_V6", "MODIS_MCD43D15_V6",
                                 "MODIS_MCD43D16_V6", "MODIS_MCD43D17_V6", "MODIS_MCD43D18_V6", "MODIS_MCD43D19_V6", "MODIS_MCD43D20_V6",
                                 "MODIS_MCD43D21_V6", "MODIS_MCD43D22_V6", "MODIS_MCD43D23_V6", "MODIS_MCD43D24_V6", "MODIS_MCD43D25_V6",
                                 "MODIS_MCD43D26_V6", "MODIS_MCD43D27_V6", "MODIS_MCD43D29_V6", "MODIS_MCD43D30_V6", "MODIS_MCD43D31_V6",
                                 "MODIS_MCD43D32_V6", "MODIS_MCD43D33_V6", "MODIS_MCD43D34_V6", "MODIS_MCD43D35_V6", "MODIS_MCD43D36_V6",
                                 "MODIS_MCD43D37_V6", "MODIS_MCD43D38_V6", "MODIS_MCD43D39_V6", "MODIS_MCD43D40_V6", "MODIS_MCD43D41_V6",
                                 "MODIS_MCD43D42_V6", "MODIS_MCD43D43_V6", "MODIS_MCD43D44_V6", "MODIS_MCD43D45_V6", "MODIS_MCD43D46_V6",
                                 "MODIS_MCD43D47_V6", "MODIS_MCD43D48_V6", "MODIS_MCD43D49_V6", "MODIS_MCD43D50_V6", "MODIS_MCD43D51_V6",
                                 "MODIS_MCD43D52_V6", "MODIS_MCD43D53_V6", "MODIS_MCD43D54_V6", "MODIS_MCD43D55_V6", "MODIS_MCD43D56_V6",
                                 "MODIS_MCD43D57_V6", "MODIS_MCD43D58_V6", "MODIS_MCD43D59_V6", "MODIS_MCD43D60_V6", "MODIS_MCD43D61_V6", 
                                 "MODIS_MCD43D62_V6", "MODIS_MCD43D63_V6", "MODIS_MCD43D64_V6", "MODIS_MCD43D65_V6", "MODIS_MCD43D66_V6",
                                 "MODIS_MCD43D67_V6", "MODIS_MCD43D68_V6", "MODIS_MYD09A1_V6", "MODIS_MYD09CMG_V6", "MODIS_MYD09GA_V6",
                                 "MODIS_MYD09GQ_V6", "MODIS_MYD09Q1_V6", "MODIS_MYD13A1_V6", "MODIS_MYD13A2_V6", "MODIS_MYD13A3_V6",
                                 "MODIS_MYD13C1_V6", "MODIS_MYD13C2_V6", "MODIS_MYD13Q1_V6", "MODIS_MYD14_V6",   "MODIS_MYD14A1_V6",
                                 "MODIS_MYD14A2_V6", "MODIS_MYD15A2H_V6", "MODIS_MYD17A2H_V6", "MODIS_MYD21A2_V6", "MODIS_MYD21_V6",
                                 "MODIS_MYD21A1D_V6", "MODIS_MYD11A1_V6", "MODIS_MYD11A2_V6", "MODIS_MYD11B1_V6", "MODIS_MYD11C1_V6",
                                 "MODIS_MYD11C2_V6", "MODIS_MYD11C3_V6", "MODIS_MYD11_L2_V6", "MODIS_MYD16A2_V6", "MODIS_MCD15A2H_V6",
                                 "MODIS_MCD15A3H_V6", "MODIS_MOD11A1_V6", "MODIS_MOD09A1_V6", "MODIS_MOD09CMG_V6", "MODIS_MOD09GA_V6",
                                 "MODIS_MOD09GQ_V6", "MODIS_MOD09Q1_V6", "MODIS_MOD11A2_V6", "MODIS_MOD11B1_V6", "MODIS_MOD11B2_V6",
                                 "MODIS_MOD11B3_V6", "MODIS_MOD11C1_V6", "MODIS_MOD11C2_V6", "MODIS_MOD11C3_V6", "MODIS_MOD11_L2_V6",
                                 "MODIS_MOD13A1_V6", "MODIS_MOD13A2_V6", "MODIS_MOD13A3_V6", "MODIS_MOD13C1_V6", "MODIS_MOD13C2_V6",
                                 "MODIS_MOD13Q1_V6", "MODIS_MOD14_V6",  "MODIS_MOD14A1_V6", "MODIS_MOD14A2_V6", "MODIS_MOD15A2H_V6",
                                 "MODIS_MOD17A2H_V6", "MODIS_MYD11B2_V6",  "MODIS_MYD11B3_V6", "MODIS_MOD44W_V6",  "MODIS_MOD16A2_V6",
                                 "MODIS_MOD44B_V6",  "MODIS_MCD12C1_V6",  "MODIS_MCD12Q1_V6", "MODIS_MCD19A3_V6", "MODIS_MCD19A2_V6",
                                 "MODIS_MCD19A1_V6", "MODIS_MCD64A1_V6", "MODIS_MODOCGA_V6", "MODIS_MODTBGA_V6", "MODIS_MYDOCGA_V6", 
                                 "MODIS_MYDTBGA_V6", "EMODIS_GLOBAL_LST_V6", "EMODIS_NDVI_V6", "EMODIS_PHEN_METRICS"),
                     "SRTM" = grep("SRTM", names(.getCMR_id()), value = T))
  }
  
  # login if required
  product_groups <- sort(product_groups)
  if(product_groups == "all") product_groups <- names(products)
  
  if(isTRUE(update_online)){
    if(any("Landsat" %in% product_groups, "MODIS" %in% product_groups)){
      .check_login(services = c("USGS"))
      api.key <- getOption("gSD.usgs_apikey")
    }
    
    if("Landsat" %in% product_groups){
      x <- grep("LSR", .EE_ds(api.key, "LANDSAT_"), value = T, invert = T) #not show LSR, since higher level products are queried at ESPA directly
      if(length(x) == 0) out("Product names could not be accessed, are you (still) logged in? USGS ERS sessions expire after some time, use login_USGS() to (re-)login.", type = 3)
      products[["Landsat"]] <-  x
    }
    if("MODIS" %in% product_groups){
      x <- .EE_ds(api.key, "MODIS_")
      if(length(x) == 0) out("Product names could not be accessed, are you (still) logged in? USGS ERS sessions expire after some time, use login_USGS() (re-)login.", type = 3)
      products[["MODIS"]] <-  x
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
  get_products("Sentinel")
}

#' @rdname get_products
#' @export
getLandsat_products <- function(..., update_online = TRUE){
  get_products("Landsat", update_online = update_online)
}

#' @rdname get_products
#' @export
getMODIS_products <- function(..., update_online = TRUE){
  get_products("MODIS", update_online = update_online)
}

#' @rdname get_products
#' @export
getSRTM_products <- function(){
  get_products("SRTM", update_online = update_online)
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