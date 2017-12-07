#' Creates an RGB preview of a Sentinel product before downloading
#'
#' \code{getSentinel_preview} previews single products as RGB plot which had been queried using \link{getSentinel_query}. The function is useful to apply visual checks to products before downloading them.
#'
#' @inheritParams getSentinel_query
#' @param product data.frame, single row data.frame collected from the return of \link{getSentinel_query}, representing the selected product and all its attributes.
#'
#' @return None. A plot is generated.
#' @details The \code{getSentinel*} function bundle makes use of the python library \code{sentinelsat}, serving as interface to the SciHub API. Python needs to be installed on your system.
#'
#' @author Jakob Schwalb-Willmann
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
#' set_cophub_login(hub_user = "16eagle") #asks for your password, if argument 'hub_pass' is not defined
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
#'
#' @importFrom getPass getPass
#' @importFrom httr GET write_disk authenticate
#' @importFrom raster stack plotRGB
#'
#' @export

getSentinel_preview <- function(product, hub_user = NULL, hub_pass = NULL,
                             hub_access = "auto"){

  ## Global Copernicus Hub login
  if(is.TRUE(getOption("gSD.cophub_def"))){
    if(is.null(hub_user)){hub_user <- getOption("gSD.cophub_user")}
    if(is.null(hub_pass)){hub_pass <- getOption("gSD.cophub_pass")}
  }
  if(!is.character(hub_user)){out("Argument 'hub_user' needs to be of type 'character'. You can use 'set_cophub_login()' to define your login credentials globally.", type=3)}
  if(!is.null(hub_pass)){hub_pass = hub_pass}else{hub_pass = getPass()}

  ## Intercept false inputs and get inputs
  link_icon <- product$link_icon
  if(is.na(link_icon)){out("Argument 'product' is invalid or no preview is available.", type=3)}
  if(length(link_icon) > 1){out("Argument 'product' must contain only a single product, represented by a single row data.frame.")}
  char_args <- list(link_icon = link_icon)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}else{TRUE}
  }

  ## Manage API access
  platform <- product$platformname
  cred <- access_API(hub_access, platform, hub_user, hub_pass)

  ## Build URL
  file_dir <- paste0(tempfile(),".jpg")
  GET(link_icon, authenticate(cred[1], cred[2]), write_disk(path = file_dir))

  ## Plot preview
  preview <- stack(file_dir)
  plotRGB(preview)
}
