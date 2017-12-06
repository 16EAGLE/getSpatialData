#' Query Sentinel-1, -2 and -3 data from the Copernicus Open Access Hubs
#'
#' \code{getSentinel_query} queries the Copernicus Open Access Hubs for Sentinel data by some basic input search parameters. The function returns a data frame that can be further filtered.
#'
#' @param ext extent object, representing the requested area of interest.
#' @param time_range character, containing two elements: the query's starting date and stopping date, formatted "YYYYMMDD", e.g. "20170515"
#' @param platform character, identifies the platform. Either "Sentinel-1", "Sentinel-2" or "Sentinel-3".
#' @param hub_user character, a valid user name to the Copernicus Open Access Hub. Register on \url{https://scihub.copernicus.eu/}.
#' @param hub_pass character, the password to the specified user account. If \code{NULL}, the password will be asked interactively (default).
#' @param hub_access character, either "operational" to look for ESA's operational products from the Open Hub,  "pre-ops" to look for pre-operational products from the Pre-Ops Hub (currently all Sentinel-3 products), or an URL to a Hub. Default is "operational".
#' @param py_path character, define path to the python command manually. If \code{NULL}, python is searched for automatically (default). To be used, of multiple versions of Python are installed (e.g. Anaconda).
#'
#' @return A data frame; each row represents one dataset, recognized by an individual UUID. The data frame can be further filtered by its columnwise attributes. The UUIDs of the selected datasets can be handed over to the other getSentinel functions for downloading.
#' @details The \code{getSentinel*} function bundle makes use of the python library \code{sentinelsat}, serving as interface to the SciHub API. Python needs to be installed on your system.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom reticulate py_available use_python
#' @importFrom raster extent
#'
#' @examples
#' ## Load packages
#' library(getSpatialData)
#' library(raster)
#'
#' ## Define an extent, a time range and a platform
#' ext <- extent(10.29048,11.75558,45.93350,46.94617)
#' time_range <-  c("20170801", "20170803")
#' platform <- "Sentinel-2"
#'
#' ## Define your Hub credentials
#' hub_user <- "your_username"
#'
#' ## Use getSentinel_query to search for data
#' \dontrun{
#' products <- getSentinel_query(ext = ext, time_range = time_range, platform = platform,
#'                               hub_user = hub_user)
#' #if you do not want to retype your password for every call, use the 'hub_pass' argument
#'
#' ## Get an overview of the products
#' View(products) #get an overview about the search products
#' colnames(products) #see all available filter attributes
#' unique(products$processinglevel) #use one of the, e.g. to see available processing levels
#'
#' ## Preview a single product
#' getSentinel_preview(product = products[1,], hub_user = hub_user)
#'
#' ## Filter the products
#' products_filtered <- products[which(products$processinglevel == "Level-1C"),] #filter by Level
#'
#' ## Download datasets
#' #dir_out <- "your/output/directory"
#' dir_out <- tempdir() #or some temporary directory for this example
#' files <- getSentinel_data(products = products_filtered, dir_out = dir_out,
#'                           hub_user = hub_user)
#' }
#' @seealso \link{getSentinel_data}
#' @export

getSentinel_query <- function(ext, time_range, platform, hub_user, hub_pass = NULL,
                              hub_access = "operational", py_path = NULL){

  ## Intercept false inputs and get inputs
  if(class(ext) != "Extent"){out("Argument 'ext' needs to be of type 'Extent'.", type = 3)}
  char_args <- list(time_range = time_range, platform = platform, hub_user = hub_user,
                    if(!is.null(hub_pass)){hub_pass = hub_pass}else{hub_pass = getPass()})
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}else{TRUE}
  }
  if(length(time_range) != 2){out("Argument 'time_range' must contain two elements (start and stop time)", type = 3)}


  ## Python connection
  gSD_ini()
  sat <- sat()


  ## Manage hub connection
  if(hub_access == "operational"){hub_access <- 'https://scihub.copernicus.eu/dhus'}
  if(hub_access == "pre-ops"){hub_access <- 'https://scihub.copernicus.eu/s3'}


  ## Convert raster::extetn to geojson
  ext.xy <- rbind(c(ext@xmin, ext@ymin),c(ext@xmin, ext@ymax),c(ext@xmax, ext@ymax),c(ext@xmax, ext@ymin))
  ext.gj <- paste0('{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":{"type":"Polygon","coordinates":[[[',toString(ext.xy[1,1]),',',toString(ext.xy[1,2]),'],[',toString(ext.xy[2,1]),',',toString(ext.xy[2,2]),'],[',toString(ext.xy[3,1]),',',toString(ext.xy[3,2]),'],[',toString(ext.xy[4,1]),',',toString(ext.xy[4,2]),'],[',toString(ext.xy[1,1]),',',toString(ext.xy[1,2]),']]]}}]}')

  tmp.gj <- paste0(tempfile(),".geojson")
  tmp.file <- file(tmp.gj)
  writeLines(ext.gj, tmp.file)
  close(tmp.file)


  ## Query through sentinelsat API
  api <- sat$SentinelAPI(hub_user, hub_pass, hub_access)
  out("Connected to Copernicus Open Access Hub.", msg = TRUE)

  aoi <- sat$geojson_to_wkt(sat$read_geojson(tmp.gj))
  file.remove(tmp.gj)
  products = api$query(area = aoi, platformname=platform, date = time_range)


  ## Building return data.frame
  pd_list <- lapply(names(products), function(x, p = products){ eval(parse(text = paste0("p$`",x,"`"))) })
  pd_names <- (unique(unlist(sapply(pd_list, names))))
  pd_frame <- as.data.frame(stats::setNames(replicate(length(pd_names),numeric(0), simplify = F), pd_names))
  for( i in 1:length(pd_list)){
    subs <- match(names(pd_list[[i]]), pd_names)
    pd_frame[i,subs] <- sapply(pd_list[[i]], as.character)
  }

  return(pd_frame)
}

