#' Download Sentinel-1, -2 and -3 datasets by UUIDs
#'
#' \code{getSentinel_data} downloads complete Sentinel datasets including imagery and meta files from the Copernicus Open Access Hubs for Sentinel data. The datasets are identified by their UUIDs, which can be extracted using \link{getSentinel_query}.
#'
#' @inheritParams getSentinel_query
#' @param products data.frame, one or multiple prodcuts (each represented by one row), as it is returned by \link{getSentinel_query}.
#' @param dir_out character, full path to download target directory
#'
#' @return List of files that had been downloaded
#' @details The \code{getSentinel*} function bundle makes use of the python library \code{sentinelsat}, serving as interface to the SciHub API. Python needs to be installed on your system.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom reticulate py_available use_python
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
#' @seealso \link{getSentinel_query}
#' @export

getSentinel_data <- function(products, dir_out, hub_user, hub_pass = NULL,
                             hub_access = "operational", py_path = NULL){

  ## Intercept false inputs and get inputs
  uuid <- products$uuid
  char_args <- list(uuid = uuid, dir_out = dir_out, hub_user = hub_user,
                    if(!is.null(hub_pass)){hub_pass = hub_pass}else{hub_pass = getPass()})
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}else{TRUE}
  }


  ## Python connection
  ini()
  sat <- py_lib("sentinelsat")


  ## Manage hub connection
  if(hub_access == "operational"){hub_access <- 'https://scihub.copernicus.eu/dhus'}
  if(hub_access == "pre-ops"){hub_access <- 'https://scihub.copernicus.eu/s3'}


  ## Query through sentinelsat API
  api <- sat$SentinelAPI(hub_user, hub_pass, hub_access)
  out("Connected to Copernicus Open Access Hub.", msg = TRUE)

  files.dir_out <- list.files(dir_out, full.names = TRUE)
  if(length(uuid) == 1){
    products = api$download(id = uuid, directory_path=dir_out)
  }else{
    products = api$download_all(products = uuid, directory_path=dir_out)
  }
  files.downlaod <- list.files(dir_out, full.names = TRUE)
  files.downlaod <- files.downlaod[which(is.na(match(files.downlaod, files.dir_out)))]

  return(files.downlaod)
}

