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
#' time_range <-  c("20170801", "20170830")
#' platform <- "Sentinel-2"
#'
#' ## Prior to calling getSentinel* functions,
#' ## define your Copernicus Open Access Hub credentials :
#' \dontrun{
#' set_cophub_login(hub_user = "16eagle") #asks for password, if 'hub_pass' is not defined
#'
#' ## Use getSentinel_query to search for data
#' products <- getSentinel_query(ext = ext, time_range = time_range, platform = platform)
#'
#' ## Get an overview of the products
#' View(products) #get an overview about the search products
#' colnames(products) #see all available filter attributes
#' unique(products$processinglevel) #use one of the, e.g. to see available processing levels
#'
#' ## Filter the products
#' products_filtered <- products[which(products$processinglevel == "Level-1C"),] #filter by Level
#'
#' ## Preview a single product
#' getSentinel_preview(product = products_filtered[10,])
#'
#' ## Download datasets
#' #dir_out <- "your/output/directory"
#' dir_out <- tempdir() #for this example, we use a temporary directory
#' files <- getSentinel_data(products = products_filtered, dir_out = dir_out)
#' }
#' @seealso \link{getSentinel_query}
#' @export

getSentinel_data <- function(products, dir_out, hub_user = NULL, hub_pass = NULL,
                             hub_access = "auto"){

  ## Global Copernicus Hub login
  if(is.TRUE(getOption("gSD.cophub_def"))){
    if(is.null(hub_user)){hub_user <- getOption("gSD.cophub_user")}
    if(is.null(hub_pass)){hub_pass <- getOption("gSD.cophub_pass")}
  }
  if(!is.character(hub_user)){out("Argument 'hub_user' needs to be of type 'character'. You can use 'set_cophub_login()' to define your login credentials globally.", type=3)}
  if(!is.null(hub_pass)){hub_pass = hub_pass}else{hub_pass = getPass()}

  ## Intercept false inputs and get inputs
  uuid <- products$uuid
  char_args <- list(uuid = uuid, dir_out = dir_out)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}else{TRUE}
  }

  ## Python connection
  ini()
  sat <- py_lib("sentinelsat")

  ## Manage API access
  platform <- unique(products$platformname)
  if(length(platform) > 1){out(paste0("Argument 'products' contains multiple platforms: ", paste0(platform,collapse = ", "), ". Please use only a single platform per call."), type = 3)}
  cred <- access_API(hub_access, platform, hub_user, hub_pass)

  ## Query through sentinelsat API
  api <- sat$SentinelAPI(cred[1], cred[2], cred[3])
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

