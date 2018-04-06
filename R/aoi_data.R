#' An example AOI of three different classes
#'
#' This dataset is a list containing three objects of different class, all representing the same AOI. \code{getSpatialData} handles AOIs provided as matrix, sf or sp object. An AOI can be set globally for the running session (see \link{set_aoi}) or directly passed to a query function (e.g. \link{getSentinel_query})
#'
#' @format The example AOI is provided as the following classes:
#' \itemize{
#'   \item \code{aoi_sf}, a \code{sf_POLYGON} object representing a single multi-point polygon, projected in lat/lon
#'   \item \code{aoi_sp}, a \code{SpatialPolygons} single multi-point polygon, projected in lat/lon
#'   \item \code{aoi_matrix}, a two-column (longitute & latitude), multi-row matrix without projection
#' }
#' @details You can load the AOI objects using data("aoi")
"aoi_data"
