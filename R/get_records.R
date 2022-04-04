#' Query available product records
#'
#' \code{get_records} queries a service for available records using basic input search parameters such as product name (see \code{\link{get_products}}), AOI and time range. The function returns a data frame of records that can be further filtered and that other \code{getSpatialData} functions use as input.
#' 
#' @param time_range character, a vector of two elements: the query's starting date and stopping date, formatted "YYYY-MM-DD", e.g. \code{c("2017-05-15", "2017-06-15")}
#' @param products character, product name(s). Use \code{\link{get_products}} to get a full list of all available products. If multiple products are supplied, the returned records are combined across products.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param as_sf logical, whether records should be returned as \code{sf} \code{data.frame} or a simple \code{data.frame}. In both cases, spatial geometries are stored in column \code{footprint}.
#' @param rename_cols logical, whether to rename columns to a product-independent standard to make it possible to combine records of different products received from different sources.
#' @param simplify_cols logical, whether to simplify the returned records table by removing fields that are not standard across different products.
#' @param check_products logical, check whether product names (\code{products}) are valid (recommended) or not. If \code{FALSE}, product names are not checked and directly used to attempt a query even if they are not supported or spelled wrong.
#' @param verbose logical, whether to display details on the function's progress or output on the console.
#' @param ... additional, sensor-specific arguments:
#' \itemize{
#'    \item \code{hub}, character, Copernicus Hub selection for Sentinel queries. Either
#' \itemize{
#'    \item "auto" (default) to automatically select a suitable Copernicus hub depending on the selected products
#'    \item "dhus" to look for operational Open Hub records only,
#'    \item "s5p" to look for Sentinel-5P precursor pre-operational records only,
#'    \item "GNSS" to look for GNSS RINEX records only,
#'    \item or a valid API URL.
#' }
#' }
#'
#' @return A data frame of records (by default an \code{sf} data frame, see argument \code{as_sf}). Each row represents one record. The data frame can be further filtered by its columnwise attributes or plotted to view their spatial footprints. The records data frame can be used as input to other \code{getSpatialData} functions.
#' 
#' @details
#' To use these functions, you need to be logged in at the required services: To query Sentinel records, login with your ESA Copernicus Open Access Hub credentials using \link{login_CopHub}. To query MODIS and Landsat records, login with your USGS EROS Registration System (ERS) credentials using \link{login_USGS}. See \code{\link{login}} for details.
#'
#' @section GNSS:
#' GNSS products (such as "sentinel-1_gnss") retrieved from the dual-frequency GPS recievers mounted on Sentinel-1, -2, and -3 represent a special type of product, as they are AOI-independent and thus only referenced by mission time (argument \code{time_range}). GNSS data originally have been used only to precisely calculate the satellites' orbits, but then have been released to the scientific public due to their potential scientifc uses (for details, see \url{https://earth.esa.int/web/sentinel/missions/sentinel-3/news/-/article/new-gnss-l1b-rinex-data-release-for-sentinel-1-2-and-3} and \url{https://earth.esa.int/documents/247904/351187/GMES_Sentinels_POD_Service_File_Format_Specification}).
#' 
#' @author Jakob Schwalb-Willmann
#'
#' @export
#' 
#' @name get_records
#' @export
get_records <- function(time_range, products, aoi = NULL, as_sf = TRUE, rename_cols = TRUE, check_products = TRUE, simplify_cols = TRUE, ..., verbose = TRUE){
  
  # check deprecated arguments
  if(missing(products)){
    extras <- list(...)
    if(!is.null(extras$check_avail)) out("Argument 'check_avail' is deprecated. Use check_availability() to check whether records are available on-demand or not.", type = 2)
    
    products <- extras$name
    if(is.null(products)) products <- extras$platform
  }
  products <- tolower(products)
  
  .check_verbose(verbose)
  .check_time_range(time_range)
  if(isTRUE(check_products)) .check_products(products, products_available = get_products(update_online = F))
  
  # use appropriate clients
  clients <- sapply(products, function(x){
    if(grepl("sentinel", x)) return("CopHub")
    if(grepl("landsat", x)) return("EE")
    if(grepl("modis", x)) return("EE")
    if(grepl("srtm", x)) return("CMR")
  })
  if(is.null(unlist(clients))) out("Could not find appropriate client(s) for this/these product(s). This/these product(s) is/are not supported.", type = 3)
  
  # get records
  records <- mapply(client = clients, product_name = products, function(client, product_name){
    if(is.null(client)) out(paste0("Could not find appropriate client for product '", product_name, "'. This product is not supported."), type = 2) else{
      eval(parse(text = paste0(".records_", client, "(time_range = time_range, product_name = product_name, aoi = aoi, rename_cols = rename_cols, simplify_cols = simplify_cols, ..., verbose = verbose)"))) 
    }
  }, USE.NAMES = F, SIMPLIFY = F)
  
  # bind records
  records <- records[!sapply(records, is.null)]
  if(length(records) > 1) records <- rbind.different(.gsd_compact(records)) else records <- records[[1]]
  
  if(!is.null(records)){
    out(paste0("Found a total of ", nrow(records), " records."))
    
    # fill missing tile IDs
    records <- .make_tileid(records)
    if(all(is.na(records$tile_id))) records$tile_id <- NULL
    
    # missing fields
    if(is.null(records$level)){
      records$level <- NA
    }
    
    
    # sort records
    used_names <- sapply(unique(getOption("gSD.clients_dict")$gSD), function(x) x %in% colnames(records))
    sorted_names <- unique(getOption("gSD.clients_dict")$gSD)[used_names]
    undefined_names <- colnames(records)[!sapply(colnames(records), function(x) x %in% sorted_names, USE.NAMES = F)]
    
    if(length(undefined_names) > 0){
      records <- cbind(records[, sorted_names], records[, undefined_names])
      colnames(records) <- c(sorted_names, undefined_names)
    }
    
    if(is.null(records$footprint)){
      out("Retrieved records have no spatial footprint and thus argument 'as_sf' is ignored.", type = 2)
      as_sf <- FALSE
    }
    
    # convert to sf
    return(.check_records(records, as_sf = as_sf))
  }
}

#' @rdname get_records
#' 
#' @export
getSentinel_records <- get_records

#' @rdname get_records
#' @export

getLandsat_records <- get_records

#' @rdname get_records
#' @export

getMODIS_records <- get_records

#' @rdname get_records
#' @export

getSRTM_records <- get_records



#' @rdname getSpatialData-deprecated
#' @export
getSentinel_query <- function(...){
  .Deprecated("getSentinel_records", "getSpatialData")
  getSentinel_records(...)
}

#' @rdname getSpatialData-deprecated
#' @export
getLandsat_query <- function(...){
  .Deprecated("getLandsat_records", "getSpatialData")
  getLandsat_records(...)
}

#' @rdname getSpatialData-deprecated
#' @export
getMODIS_query <- function(...){
  .Deprecated("getMODIS_records", "getSpatialData")
  getMODIS_records(...)
}


