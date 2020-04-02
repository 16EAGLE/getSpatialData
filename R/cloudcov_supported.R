#' Returns product names supported by \code{calc_cloudcov}
#' 
#' @description \code{cloudcov_supported} provides all product names supported by \link{calc_cloudcov}.
#' Note that among the Sentinel-3 sensors only OLCI products are supported.
#' 
#' @return character vector of product names supported by \link{calc_cloudcov}.
#' @author Henrik Fisser, 2020
#' @export
cloudcov_supported <- function() {
  return(.cloudcov_products())
}
