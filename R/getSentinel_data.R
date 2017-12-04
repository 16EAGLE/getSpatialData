#' Download Sentinel-1, -2 and -3 datasets by UUIDs
#'
#' \code{getSentinel_data} downloads complete Sentinel datasets including imagery and meta files from the Copernicus Open Access Hubs for Sentinel data. The datasets are identified by their UUIDs, which can be extracted using \url{\code{getSentinel_query}}.
#'
#' @inheritParams getSentinel_query
#' @param uuid character vector, one or multiple UUIDs identifying the datasets to be downloaded.
#' @param dir_out character, full path to download target directory
#'
#' @return List of files that had been downloaded
#' @details The \code{getSentinel*} function bundle makes use of the python library \code{sentinelsat}, serving as interface to the SciHub API. Python needs to be installed on your system.
#'
#' @seealso \url{getSentinel_query}, \url{getSentinel_meta}
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom reticulate py_available use_python
#'
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

