#' Returns product names supported by \code{select_*} functionalities
#' 
#' @description \code{get_select_supported} provides all product names supported by:
#' \itemize{
#' \item \link{select_unitemporal}
#' \item \link{select_bitemporal}
#' \item \link{select_timeseries}
#' }
#' Other products than the returned cannot be processed by \code{select_*}.
#' Note that among the Sentinel-3 sensors only OLCI products are supported.
#' 
#' @return character vector of product names supported by \code{select_*}.
#' @author Henrik Fisser, 2020
#' @export
get_select_supported <- function() {
  optical_sensors <- .cloudcov_products()
  optical_sensors[which(optical_sensors %in% .get_select_supported_modis())] <- NA
  supported_products <- c(.gsd_compact(optical_sensors), "Sentinel-1")
  return(supported_products)
}
