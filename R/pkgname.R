#' Overview of getSpatialData
#' 
#' @section Introduction:
#' 
#' \code{getSpatialData} is an R package in an early development stage that ultimately aims to enable homogeneous and reproducible workflows to query, preview, analyse, select, order and download various kinds of spatial datasets from open sources.
#' 
#' Currently, \code{getSpatialData} supports a variety of products, including Sentinel-1, Sentinel-2, Sentinel-3, Sentinel-5P, Landsat 8 OLI, Landsat ETM, Landsat TM, Landsat MSS, MODIS (Terra & Aqua) and SRTM DEMs. For this, \code{getSpatialData} facilitates access to multiple services implementing clients to public APIs of ESA Copernicus Open Access Hub, USGS EarthExplorer, USGS EROS ESPA, Amazon Web Services (AWS), NASA DAAC LAADS and NASA CMR search. A full list of all supported products can be found below.
#' 
#' @section Workflow:
#' 
#' The \code{getSpatialData} workflow to query, preview, analyse, select, order and download spatial data is designed to be as reproducible as possible and is made of six steps: 
#' 
#' \itemize{
#' \item \strong{querying} products of interest (see \link{get_products}) for available records by an area of interest (AOI) and time (see \link{get_records}),
#' \item \strong{previewing} geometries and previews of the obtained records (see \link{get_previews}, \link{view_records} and \link{view_previews}),
#' \item \strong{analysing} records by deriving scene and AOI cloud distribution and coverage directly from preview imagery (see \link{calc_cloudcov}),
#' \item \strong{selecting} records based on user-defined measures (see \link{select_unitemporal}, \link{select_bitemporal} and \link{select_timeseries}),
#' \item \strong{ordering} datasets that are not available for immediate download (on-demand) but need to be ordered or restored before download (see \link{check_availability} and \link{order_data}), and lastly
#' \item \strong{downloading} the full datasets for those records that have been selected in the process, solely based on meta and preview-dervied data.
#' }
#' 
#' For all steps, \code{getSpatialData} supports local chaching to reduce bandwith usage and uneccasary downloads.
#'
#' Further details are included in the README file, which is constantly updated on \url{https://github.com/16EAGLE/getSpatialData}.
#'
#' @author Jakob Schwalb-Willmann
#' @author Henrik Fisser
"_PACKAGE"
