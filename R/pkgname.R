#' Overview of getSpatialData tools
#'
#' \code{getSpatialData} is an R package that aims to provide homogeneous function bundles to query, preview, download, prepare and transform various kinds of spatial datasets from open sources, e.g. Satellite sensor data and higher-level environmental data products.
#'
#' @details At the moment, the package includes the following functions:
#'
#' \strong{Sentinel}
#' \itemize{
#'   \item \link{getSentinel_query} querys the Copernicus Open Access Hubs for Sentinel-1, -2 and -3 data and returns a data frame containing the found records (rows) and their attributes (columns).
#'   \item \link{getSentinel_preview} uses the output of \code{getSentinel_query} to preview (quick-look) a user-selected, individual dataset even before downloading it. By default, the preview is displayed corner-georeferenced in a map viewer in relation to the session AOI.
#'   \item \link{getSentinel_data} uses the output of \code{getSentinel_query} to download Sentinel data.
#' }
#'
#'
#'
#' \strong{Landsat}
#' \itemize{
#'   \item \link{getLandsat_names} obtains available Landsat product names from USGS Earth Explorer, which can be optionally used with \link{getLandsat_query} to narrow the search.
#'   \item \link{getLandsat_query} querys USGS Earth Explorer for Landsat data and returns a data frame containing the found records (rows) and their attributes (columns).
#'   \item \link{getLandsat_preview} uses the output of \link{getLandsat_query} to preview (quick-look) a user-selected, individual dataset. By default, the preview is displayed corner-georeferenced in a map viewer in relation to the session AOI.
#'   \item \link{getLandsat_data} uses the output of \link{getLandsat_query} to order and download Landsat data.
#'   \itemize{
#'     \item supports order (on-demand processing) and download of higher-level products (all Landsat products), e.g. top-of-atmosphere (TOA), surface reflectance (SR) or different indices, from USGS-EROS ESPA.
#'     \item supports direct download of Level-1 products (Landsat-8 only) via Amazon Web Services (AWS).
#'     \item will support direct download of Level-1 products (all Landsat datasets) via USGS EarthExplorer (requires a USGS user profile with machine-to-machine download permission)
#'   }
#' }
#'
#'
#' \strong{MODIS}
#' \itemize{
#'   \item \link{getMODIS_names} obtains available MODIS product names from USGS Earth Explorer, which can be optionally used with \link{getMODIS_query} to narrow the search.
#'   \item \link{getMODIS_query} querys USGS Earth Explorer for MODIS data and returns a data frame containing the found datasets (rows) and their attributes (columns).
#'   \item \link{getMODIS_data} uses the output of \link{getMODIS_query} to order and download MODIS data from LAADS.
#' }
#'
#'
#' \strong{Login}
#' \itemize{
#'   \item \link{login_CopHub} define your Copernicus Open Access login credentials once for the present R session to be able to call each \code{getSentinel} function without defining login arguments each time you use them.
#'   \item \link{login_USGS} define your USGS login credentials once for the present R session to be able to call each \code{get*} function that connects to a USGS service without defining login arguments each time you use them.
#' }
#'
#'
#' \strong{Session settings}
#' \itemize{
#'   \item \link{set_archive} define a \code{getSpatialData} archive directory to which all \code{*_data} functions will download data.
#'   \item \link{set_aoi} draw or define an AOI as sf, sp or matrix object for the running session that can be used by all query functions.
#'   \item \link{view_aoi} display the session AOI in an interactive \code{mapview}/\code{leaflet} map viewer.
#'   \item \link{get_aoi} get the session AOI you have defined or drawn before as sf, sp or matrix object.
#' }
#'
#'
#' Further details are included in the README file, which is constantly updated on \url{https://github.com/16EAGLE/getSpatialData}.
#'
#' @author Jakob Schwalb-Willmann
"_PACKAGE"
