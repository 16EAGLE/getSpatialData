#' Get product names
#'
#' \code{get_products} obtains the names of all products available through \code{getSpatialData}. The returned names can be used with the \link{get_records} functions for querying a specific product.
#' @param product_groups character, either "all" or a combination of product names. Supported names are "Sentinel", "Landsat", "MODIS", "SRTM".
#' @param grouped logical, whether to return a list of names grouped by platform/sensor (\code{TRUE}) or not.
#' @param update_online logical, whether to update the internal product list online (default, requires login) or not.
#' @param ... deprecated arguments.
#' 
#' @note If \code{update_online = TRUE}, you must be logged in at the services required for your request to use this function. See the examples and \link{login} for details.
#' 
#' @section GNSS:
#' GNSS products (such as "Sentinel-1_GNSS") retrieved from the dual-frequency GPS recievers mounted on Sentinel-1, -2, and -3 represent a special type of product, as they are AOI-independent and thus only referenced by mission time. GNSS data originally have been used only to precisely calculate the satellites' orbits, but then have been released to the scientific public due to their potential scientifc uses (for details, see \url{https://earth.esa.int/web/sentinel/missions/sentinel-3/news/-/article/new-gnss-l1b-rinex-data-release-for-sentinel-1-2-and-3} and \url{https://earth.esa.int/documents/247904/351187/GMES_Sentinels_POD_Service_File_Format_Specification}).
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
get_products <- function(product_groups = "all", grouped = FALSE, update_online = FALSE){
  
  # assemble offline products list
  products <- list("sentinel" = c(getOption("gSD.copnames")$name[getOption("gSD.copnames")$name != "GNSS"], 
                                  paste0(getOption("gSD.copnames")$name[getOption("gSD.copnames")$name != "GNSS" & getOption("gSD.copnames")$name != "Sentinel-5P"], "_GNSS")),
                   "landsat" = c("landsat_8_c1", "lsr_landsat_8_c1", "landsat_ot_c2_l1", "landsat_ot_c2_l2", 
                                 "landsat_etm_c1", "lsr_landsat_etm_c1", "landsat_etm_c2_l1", "landsat_etm_c2_l2", 
                                 "landsat_tm_c1", "lsr_landsat_tm_c1", "landsat_tm_c2_l1", "landsat_tm_c2_l2",
                                 "landsat_mss_c1", "landsat_mss_c2_l1", "landsat_band_files_c2_l1",
                                 "landsat_band_files_c2_l2"),
                   "modis" = c("modis_mcd64a1_v6", "modis_mod09a1_v6", "modis_mod09cmg_v6", "modis_mod14_v6", 
                               "modis_mod09ga_v6", "modis_mod14a1_v6", "modis_mod09gq_v6", "modis_mod14a2_v6", 
                               "emodis_global_lst_v6", "modis_mod09q1_v6", "modis_modocga_v6", "modis_myd14_v6", 
                               "emodis", "modis_modtbga_v6", "modis_myd14a1_v6", "emodis_ndvi_v6", "modis_myd09a1_v6", 
                               "modis_myd14a2_v6", "emodis_phen_metrics", "modis_myd09cmg_v6", "modis_myd09ga_v6", 
                               "modis_myd09gq_v6", "modis_myd09q1_v6", "modis_mydocga_v6", "modis_mydtbga_v6", 
                               "lpcs_modis_mcd12q1", "lpcs_modis_mcd43a3", "lpcs_modis_mod09a1", "lpcs_modis_mod09ga", 
                               "lpcs_modis_mod09gq", "lpcs_modis_mod09q1", "lpcs_modis_mod11a1", "lpcs_modis_mod13a1",
                               "lpcs_modis_mod13a2", "lpcs_modis_mod13a3", "lpcs_modis_mod13q1", "lpcs_modis_myd09a1", 
                               "lpcs_modis_myd09ga", "lpcs_modis_myd09gq", "lpcs_modis_myd09q1", "lpcs_modis_myd11a1", 
                               "lpcs_modis_myd13a1", "lpcs_modis_myd13a2", "lpcs_modis_myd13a3", "lpcs_modis_myd13q1",
                               "modis_mcd12c1_v6", "modis_mcd12q1_v6", "modis_mcd12q2_v6", "modis_mcd15a2h_v6", "modis_mcd15a3h_v6",
                               "modis_mcd19a1_v6", "modis_mcd19a2_v6", "modis_mcd19a3_v6", "modis_mcd43a1_v6", "modis_mcd43a2_v6",
                               "modis_mcd43a3_v6", "modis_mcd43a4_v6", "modis_mcd43c1_v6", "modis_mcd43c2_v6", "modis_mcd43c3_v6",
                               "modis_mcd43c4_v6", "modis_mcd43d01_v6", "modis_mcd43d02_v6", "modis_mcd43d03_v6", "modis_mcd43d04_v6",
                               "modis_mcd43d05_v6", "modis_mcd43d06_v6", "modis_mcd43d07_v6", "modis_mcd43d08_v6", "modis_mcd43d09_v6",
                               "modis_mcd43d10_v6", "modis_mcd43d11_v6", "modis_mcd43d12_v6", "modis_mcd43d13_v6", "modis_mcd43d14_v6", 
                               "modis_mcd43d15_v6", "modis_mcd43d16_v6", "modis_mcd43d17_v6", "modis_mcd43d18_v6", "modis_mcd43d19_v6", 
                               "modis_mcd43d20_v6", "modis_mcd43d21_v6", "modis_mcd43d22_v6", "modis_mcd43d23_v6", "modis_mcd43d24_v6",
                               "modis_mcd43d25_v6", "modis_mcd43d26_v6", "modis_mcd43d27_v6", "modis_mcd43d28_v6", "modis_mcd43d29_v6",
                               "modis_mcd43d30_v6", "modis_mcd43d31_v6", "modis_mcd43d32_v6", "modis_mcd43d33_v6", "modis_mcd43d34_v6",
                               "modis_mcd43d35_v6", "modis_mcd43d36_v6", "modis_mcd43d37_v6", "modis_mcd43d38_v6", "modis_mcd43d39_v6",
                               "modis_mcd43d40_v6", "modis_mcd43d41_v6", "modis_mcd43d42_v6", "modis_mcd43d43_v6", "modis_mcd43d44_v6",
                               "modis_mcd43d45_v6", "modis_mcd43d46_v6", "modis_mcd43d47_v6", "modis_mcd43d48_v6", "modis_mcd43d49_v6",
                               "modis_mcd43d50_v6", "modis_mcd43d51_v6", "modis_mcd43d52_v6", "modis_mcd43d53_v6", "modis_mcd43d54_v6",
                               "modis_mcd43d55_v6", "modis_mcd43d56_v6", "modis_mcd43d57_v6", "modis_mcd43d58_v6", "modis_mcd43d59_v6",
                               "modis_mcd43d60_v6", "modis_mcd43d61_v6", "modis_mcd43d62_v6", "modis_mcd43d63_v6", "modis_mcd43d64_v6",
                               "modis_mcd43d65_v6", "modis_mcd43d66_v6", "modis_mcd43d67_v6", "modis_mcd43d68_v6", "modis_mod11a1_v6",
                               "modis_mod11a2_v6", "modis_mod11b1_v6", "modis_mod11b2_v6", "modis_mod11b3_v6", "modis_mod11c1_v6",
                               "modis_mod11c2_v6", "modis_mod11c3_v6", "modis_mod11_l2_v6", "modis_mod13a1_v6", "modis_mod13a2_v6",
                               "modis_mod13a3_v6", "modis_mod13c1_v6", "modis_mod13c2_v6", "modis_mod13q1_v6", "modis_mod15a2h_v6",
                               "modis_mod16a2_v6", "modis_mod17a2h_v6", "modis_mod44b_v6", "modis_mod44w_v6", "modis_myd11a1_v6",
                               "modis_myd11a2_v6", "modis_myd11b1_v6", "modis_myd11b2_v6", "modis_myd11b3_v6", "modis_myd11c1_v6",
                               "modis_myd11c2_v6", "modis_myd11c3_v6", "modis_myd11_l2_v6", "modis_myd13a1_v6", "modis_myd13a2_v6",
                               "modis_myd13a3_v6", "modis_myd13c1_v6", "modis_myd13c2_v6", "modis_myd13q1_v6", "modis_myd15a2h_v6",
                               "modis_myd16a2_v6", "modis_myd17a2h_v6", "modis_myd21a1d_v6", "modis_myd21a1n_v6", "modis_myd21a2_v6", 
                               "modis_myd21_v6"),
                   "srtm" = grep("srtm", names(.getCMR_id()), value = T))
  
  # login if required
  product_groups <- tolower(sort(product_groups))
  if(product_groups == "all") product_groups <- tolower(names(products))
  
  if(isTRUE(update_online)){
    if(any("landsat" %in% product_groups, "modis" %in% product_groups)){
      .check_login(services = c("USGS"))
      api.key <- getOption("gSD.usgs_apikey")
    }
    
    if("landsat" %in% product_groups){
      x <- grep("LSR", .EE_ds(api.key, wildcard = "landsat_"), value = T, invert = T) #not show LSR, since higher level products are queried at ESPA directly
      x <- x[grepl("landsat", x)] # CONSIDER REMOVING!
      if(length(x) == 0) out("Product names could not be accessed, are you (still) logged in? USGS ERS sessions expire after some time, use login_USGS() to (re-)login.", type = 3)
      products[["landsat"]] <-  x
    }
    if("modis" %in% product_groups){
      x <- .EE_ds(api.key, "modis_")
      x <- x[grepl("modis", x)] # CONSIDER REMOVING!
      if(length(x) == 0) out("Product names could not be accessed, are you (still) logged in? USGS ERS sessions expire after some time, use login_USGS() (re-)login.", type = 3)
      products[["modis"]] <-  x
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
getSRTM_products <- function(..., update_online = TRUE){
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