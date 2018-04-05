#' Creates an RGB preview of a Sentinel product before downloading
#'
#' \code{getSentinel_preview} previews single products as RGB plot which had been queried using \link{getSentinel_query}. The function is useful to apply visual checks to products before downloading them.
#'
#' @inheritParams getSentinel_query
#' @param product data.frame, single row data.frame collected from the return of \link{getSentinel_query}, representing the selected product and all its attributes.
#'
#' @return None. A plot is generated.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @examples
#' ## Load packages
#' library(getSpatialData)
#'
#' ## Define an AOI (last row needs to equal first row, closing the polygon)
#' aoi <- rbind(c(9.29168701, 49.93884750),
#'              c(9.36584472, 49.58400677),
#'              c(10.1458740, 49.44312875),
#'              c(10.6814575, 49.75642885),
#'              c(9.75860595, 50.20327530),
#'              c(9.29168701, 49.93884750)) # last row must equal first row
#'
#' time_range <-  c("2017-08-01", "2017-08-30")
#' platform <- "Sentinel-2"
#'
#' ## set login credentials and archive directory
#' \dontrun{
#' set_login_CopHub(hub_user = "16eagle") #asks for your password, if argument 'hub_pass' is not defined
#' set_archive("/home/jakob/Dokumente/wd/tmp/gSD/")
#'
#' ## Use getSentinel_query to search for data
#' products <- getSentinel_query(aoi = aoi, time_range = time_range, platform = platform)
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
#' getSentinel_preview(product = products_filtered[5,])
#'
#' ## Download some datasets to your archive directory
#' files <- getSentinel_data(products = products_filtered[c(4,5,6), ])
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
  if(is.TRUE(getOption("gSD.cophub_set"))){
    if(is.null(hub_user)){hub_user <- getOption("gSD.cophub_user")}
    if(is.null(hub_pass)){hub_pass <- getOption("gSD.cophub_pass")}
  }
  if(!is.character(hub_user)){out("Argument 'hub_user' needs to be of type 'character'. You can use 'set_login_CopHub()' to define your login credentials globally.", type=3)}
  if(!is.null(hub_pass)){hub_pass = hub_pass}else{hub_pass = getPass()}

  ## Intercept false inputs and get inputs
  url.icon <- product$url.icon
  if(is.na(url.icon)){out("Argument 'product' is invalid or no preview is available.", type=3)}
  if(length(url.icon) > 1){out("Argument 'product' must contain only a single product, represented by a single row data.frame.")}
  char_args <- list(url.icon = url.icon)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}else{TRUE}
  }

  ## Manage API access
  platform <- product$platformname
  cred <- access_API(hub_access, platform, hub_user, hub_pass)

  ## Build URL
  file_dir <- paste0(tempfile(),".jpg")
  GET(url.icon, authenticate(cred[1], cred[2]), write_disk(path = file_dir))

  ## Plot preview
  preview <- stack(file_dir)
  plotRGB(preview)
}
