#' Download Sentinel-1, -2 and -3 data
#'
#' \code{getSentinel_data} downloads complete Sentinel datasets including imagery and meta files from the Copernicus Open Access Hubs for Sentinel data. The datasets are identified by the query return of \link{getSentinel_query}.
#'
#' @inheritParams getSentinel_query
#' @param products data.frame, one or multiple prodcuts (each represented by one row), as it is returned by \link{getSentinel_query}.
#' @param dir_out character, full path to download target directory. Optional. If not set, \code{getSentinel_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData  archive folder.
#' @param force logical. If \code{TRUE}, download is forced even if file already exisits in the download directory. Default is \code{FALSE}.
#'
#' @return Character vector of files that had been downloaded.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom tools md5sum
#'
#' @examples
#' ## Load packages
#' library(getSpatialData)
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
#' products <- getSentinel_query(time_range = time_range, platform = platform)
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
#' ## Download some datasets
#' files <- getSentinel_data(products = products_filtered[c(4,5,6),])
#' }
#'
#' @seealso \link{getSentinel_query}
#' @export

getSentinel_data <- function(products, dir_out = NULL, force = FALSE, username = NULL, password = NULL,
                             hub = "auto"){

  ## Global Copernicus Hub login
  if(is.TRUE(getOption("gSD.cophub_set"))){
    if(is.null(username)){username <- getOption("gSD.cophub_user")}
    if(is.null(password)){password <- getOption("gSD.cophub_pass")}
  }
  if(!is.character(username)){out("Argument 'username' needs to be of type 'character'. You can use 'login_CopHub()' to define your login credentials globally.", type=3)}
  if(!is.null(password)){password = password}else{password = getPass()}

  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)){dir_out <- paste0(getOption("gSD.archive"), "/COPERNICUS/")}
    if(!dir.exists(dir_out)) dir.create(dir_out)
  }

  ## Intercept false inputs and get inputs
  uuid <- products$uuid
  char_args <- list(uuid = uuid, dir_out = dir_out)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(!dir.exists(dir_out)) out("The defined output directory does not exist.", type=3)

  ## Manage API access
  platform <- unique(products$platformname)
  if(length(platform) > 1){out(paste0("Argument 'products' contains multiple platforms: ", paste0(platform,collapse = ", "), ". Please use only a single platform per call."), type = 3)}
  cred <- cophub_api(hub, platform, username, password)

  ## assemble md5 checksum url
  url.md5 <- sapply(products$url.alt, function(x) paste0(x, "Checksum/Value/$value"), USE.NAMES = F)
  md5 <- sapply(url.md5, function(x) content(gSD.get(x, cred[1], cred[2])), USE.NAMES = F)

  url.ds <- products$url
  prod.id <- products$identifier
  file.ds <- sapply(prod.id, function(x) paste0(dir_out, "/", x, ".zip"), USE.NAMES = F) #download to file

  ## download not parallelized (2 downstreams max)
  down.status <- rep(FALSE, length(url.ds))
  max.retry <- 3  #maximum retries berfore giving up
  down.retry <- max.retry
  while(all(down.status) == F & down.retry > 0){

    if(down.retry != max.retry) out("Reattempting failed downloads...", msg =T)

    for(i in which(down.status == F)){
      if(!file.exists(file.ds[i]) | force == T){
        if(file.exists(file.ds[i])) file.remove(file.ds[i]) #remove in case of force being TRUE

        file.tmp <- tempfile(tmpdir = dir_out, fileext = ".zip")
        out(paste0("Downloading '", prod.id[i], "' to '", file.ds[i], "'..."), msg = T)
        gSD.get(url.ds[i], cred[1], cred[2], dir.file = file.tmp, prog = T) #file.ds[i]

        if(as.character(md5sum(file.tmp)) == tolower(md5[i])){ #file.ds[i]
          out("Successfull download, MD5 check sums match.", msg = T)
          file.rename(file.tmp, file.ds[i])
          down.status[i] <- T
        } else{
          out(paste0("Downloading of '", prod.id[i],"' failed, MD5 check sums do not match."), type = 2)
          file.remove(file.ds[i])
        }
      } else{
        out(paste0("Skipping '", prod.id[i], "', since '", file.ds[i], "' already exists..."), msg = T)
      }
    }
    down.retry <- down.retry-1 #limit retries
  }

  if(any(down.status) == F){out(paste0("All downloads failed due to unknown reason (", toString(max.retry), " attempts)."), type=3)}
  if(all(down.status) == F){out(paste0("Downloads failed for datasets '", paste0(prod.id[down.status == F], collapse = "', '"),  "' (", toString(max.retry), " attempts)."), type = 2)}

  out(paste0("All downloads have been succesfull (", toString(max.retry-down.retry), " attempts)."), msg = T)
  return(file.ds[down.status])
}
