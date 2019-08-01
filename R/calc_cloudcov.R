#' Calculate the cloud cover of Sentinel, Landsat or MODIS in an aoi before large data download
#' 
#' \code{calc_cloudcov} requests previews using \code{get_preview} and calculates the aoi cloud cover
#' upon these images. Cloud cover is computed currently using one of the following options:
#' \itemize{
#' \item Haze-Optimal-Transformation (HOT) (Zhu & Helmer (2018)).
#' } 
#' @note if a \code{dir_out} is given cloud mask rasters and a record csv for each record is saved in \code{dir_out}.
#' 
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{get_records}.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param maxDeviation numeric, the maximum allowed deviation of calculated scene cloud cover from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover \% given by the data distributor. Default is \code{maxDeviation = 20}.
#' @param cloudPrbThreshold numeric, the threshold of the HOT cloud probability layer (0-100 \%) below which pixels are considered as clear sky. Default is \code{cloudPrbThreshold = 40}. It will be dynamically adjusted according to the input in \code{maxDeviation}.
#' @param slopeDefault numeric, value taken as slope ONLY if least-alternate deviation regression fails.  Default is 1.4, proven to work well for common land surfaces.f default values. In this case cloud cover will be set to 9999 \% for the given record.
#' @param interceptDefault numeric, value taken as intercept ONLY if least-alternate deviation regression fails. Default is -10, proven to work well for common land surfaces.
#' @param dir_out character, optional. Full path to target directory where to save the cloud masks. If \code{NULL}, cloud masks are not saved.
#' @param username character, a valid user name to the ESA Copernicus Open Access Hub. If \code{NULL} (default), the session-wide login credentials are used (see \link{login_CopHub} for details on registration).
#' @param password character, the password to the specified user account. If \code{NULL} (default) and no seesion-wide password is defined, it is asked interactively ((see \link{login_CopHub} for details on registration).
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#' 
#' @return \code{records} data.frame with two added columns holding the estimated cloud cover for the whole preview and for the aoi.
#' 
#' @author Henrik Fisser
#' 
#' @examples
#' ## Load packages
#' library(getSpatialData)
#' library(raster)
#' library(sf)
#' library(sp)
#'
#' ## Define an AOI (either matrix, sf or sp object)
#' data("aoi_data") # example aoi
#'
#' aoi <- aoi_data[[3]] # AOI as matrix object, or better:
#' aoi <- aoi_data[[2]] # AOI as sp object, or:
#' aoi <- aoi_data[[1]] # AOI as sf object
#' # or, simply call set_aoi() without argument to interactively draw an AOI
#'
#' ## set AOI for this session
#' set_aoi(aoi)
#' view_aoi() #view AOI in viewer
#'
#' ## Define time range and platform
#' time_range <-  c("2017-08-01", "2017-08-30")
#' platform <- "Sentinel-2"
#'
#' ## set login credentials and an archive directory
#' \dontrun{
#' login_CopHub(username = "username") #asks for password or define 'password'
#' set_archive("/path/to/archive/")
#'
#' ## Use getSentinel_query to search for data (using the session AOI)
#' records <- getSentinel_query(time_range = time_range, platform = platform)
#'
#' ## Get an overview of the records
#' View(records) #get an overview about the search records
#' colnames(records) #see all available filter attributes
#' unique(records$processinglevel) #use one of the, e.g. to see available processing levels
#' 
#' ## Calculate cloud cover within the aoi
#' records_cloudcov <- calcSentinel_aoi_cloudcov(records = records, aoi = aoi) # cloud cov. calc.
#' 
#' ## Filter the records
#' records_filtered <- records_cloudcov[which(records_cloudcov$processinglevel == "Level-1C"),]
#'
#' ## Preview a single record
#' getSentinel_preview(record = records_filtered[5,])
#'
#' ## Download some datasets
#' datasets <- getSentinel_data(records = records_filtered[c(4,5,6),])
#'
#' ## Make them ready to use
#' datasets_prep <- prepSentinel(datasets, format = "tiff")
#'
#' ## Load them to R
#' r <- stack(datasets_prep[[1]][[1]][1]) #first dataset, first tile, 10m resoultion
#' }
#' 
#' @export

calc_cloudcov <- function(records, aoi = NULL,  
                          maxDeviation = 20, cloudPrbThreshold = 40, 
                          slopeDefault = 1.4, interceptDefault = -10, 
                          dir_out = NULL, username = NULL, password = NULL, verbose = TRUE) {
  
  sensor <- unique(records$sensor)
  records <- .cloudcov_bridge(sensor=sensor,sceneCloudCoverCol=sceneCloudCoverCol,records=records,aoi=aoi,maxDeviation=maxDeviation,
                        cloudPrbThreshold=cloudPrbThreshold,slopeDefault=slopeDefault,
                        interceptDefault=interceptDefault,dir_out=dir_out,username=username,password=password,verbose=verbose)
  
  return(records)
  
}