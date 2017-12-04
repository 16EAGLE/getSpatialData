#' Download Sentinel-1, -2 and -3 datasets by UUIDs
#'
#' \code{getSentinel_data} downloads complete Sentinel datasets including imagery and meta files from the Copernicus Open Access Hubs for Sentinel data. The datasets are identified by their UUIDs, which can be extracted using \link{getSentinel_query}.
#'
#' @inheritParams getSentinel_query
#' @param uuid character vector, one or multiple UUIDs identifying the datasets to be downloaded.
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
#'
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
#' hub_user <- "your_account"
#'
#' ## Use getSentinel_query to search for data
#' \dontrun{
#' results <- getSentinel_query(ext = ext, time_range = time_range, platform = platform,
#'                              hub_user = hub_user)
#'
#' ## View the results
#' View(results) #get an overview about the search results
#' colnames(results) #see all available filter attributes
#' unique(results$processinglevel) #use one of the, e.g. to see available processing levels
#'
#' ## Filter the results
#' results_filtered <- results[which(results$processinglevel == "Level-1C"),] #filter by Level
#'
#' ## Extract UUIDs
#' uuid <- results_filtered$uuid #get UUIDs of your filtered query results
#' uuid <- uuid[c(1:3)] #take the first three datasets to be downloaded
#'
#' ## Download datasets
#' #dir_out <- "your/output/directory"
#' dir_out <- tempdir() #or some temporary directory for this example
#' files <- getSentinel_data(uuid = uuid, dir_out = dir_out,
#'                           hub_user = hub_user)
#' }
#' @seealso \link{getSentinel_query}
#' @export

getSentinel_data <- function(uuid, dir_out, hub_user, hub_pass = NULL,
                             hub_access = "operational", py_path = NULL){

  ## Intercept false inputs and get inputs
  char_args <- list(uuid = uuid, dir_out = dir_out, hub_user = hub_user,
                    if(!is.null(hub_pass)){hub_pass = hub_pass}else{hub_pass = getPass()})
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}else{TRUE}
  }


  ## Manage hub connection
  if(hub_access == "operational"){hub_access <- 'https://scihub.copernicus.eu/dhus'}
  if(hub_access == "pre-ops"){hub_access <- 'https://scihub.copernicus.eu/s3'}


  ## sentinelsat connection
  sat <- try(py_load("sentinelsat")$sentinelsat)
  if(class(sat)[1] == "try-error"){out("Could not load/install the 'sentinelsat' python library.", type = 3)}


  ## Connect to API and download data
  api <- sat$SentinelAPI(hub_user, hub_pass, hub_access)
  files.dir_out <- list.files(dir_out, full.names = TRUE)
  if(length(uuid) == 1){api$download(uuid, dir_out)}else{api$download_all(uuid, dir_out)}
  files.downlaod <- list.files(dir_out, full.names = TRUE)
  files.downlaod <- files.downlaod[which(is.na(match(files.downlaod, files.dir_out)))]

  return(files.downlaod)
}

