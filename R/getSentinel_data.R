#' Download Sentinel-1, Sentinel-2, Sentinel-3, Sentinel-5P or Sentinel GNSS data
#'
#' \code{getSentinel_data} downloads Sentinel data from the Copernicus Open Access Hubs. The datasets are identified per records as returned by \link{getSentinel_query}.
#'
#' @inheritParams getSentinel_query
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{getSentinel_query}.
#' @param dir_out character, full path to download target directory. Optional. If not set, \code{getSentinel_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData archive folder.
#' @param force logical. If \code{TRUE}, download is forced even if file already exisits in the download directory. Default is \code{FALSE}.
#' @param n.retry numeric, maximum number of download (re-)attempts. If the downloads of datasets fail (e.g. MD5 checksums do not match), these downloads will be reattampted.
#'
#' @return Character vector of paths to the downloaded files.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom tools md5sum
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
#' ## Filter the records
#' records_filtered <- records[which(records$processinglevel == "Level-1C"),] #filter by Level
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
#' @seealso \link{getSentinel_query}
#' @export

getSentinel_data <- function(records, dir_out = NULL, force = FALSE, username = NULL, password = NULL,
                             hub = "auto", n.retry = 3, verbose = TRUE){
  
  ## Global Copernicus Hub login
  if(is.TRUE(getOption("gSD.dhus_set"))){
    if(is.null(username)) username <- getOption("gSD.dhus_user")
    if(is.null(password)) password <- getOption("gSD.dhus_pass")
  }
  if(!is.character(username)) out("Argument 'username' needs to be of type 'character'. You can use 'login_CopHub()' to define your login credentials globally.", type=3)
  if(!is.null(password)) password = password else password = getPass()
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  
  if(length(unique(records$platformname)) > 1) out("Platform name differs. Platform name needs to be unique per call, e.g. 'Sentinel-1' only, as returned by getSentinel_query().", type=3)
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)) dir_out <- paste0(getOption("gSD.archive_get"), "/", unique(records$platformname), "/") else dir_out <- path.expand(dir_out)
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  
  ## Intercept false inputs and get inputs
  char_args <- list("records$uuid" = records$uuid, "records$url" = records$url, "records$identifier" = records$identifier, dir_out = dir_out)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("'", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(!dir.exists(dir_out)) out("The defined output directory does not exist.", type=3)
  
  ## collect record column names
  records.names <- colnames(records)
  
  ## Manage API access
  platform <- unique(records$platformname)
  gnss <- unique(records$gnss)
  if(length(platform) > 1){out(paste0("Argument 'records' contains multiple platforms: ", paste0(platform,collapse = ", "), ". Please use only a single platform per call."), type = 3)}
  if(length(gnss) > 1) out("Some records are GNSS records, while others are not. Please do not join non-GNNS and GNNS records and call getSentinel_data separately for both.", type = 3)
  cred <- .CopHub_select(x = hub, p = if(isTRUE(gnss)) "GNSS" else platform, user = username, pw = password)
  
  ## check availability
  if(is.null(records$available)) records$available <- as.logical(toupper(unlist(.get_odata(records$uuid, cred, field = "Online/$value"))))
  if(any(!records$available)){
    out(paste0("Datasets '", paste0(records$identifier[!records$available], collapse = "', '"), "' are not available on-demand, since they have been archived."), type = if(all(!records$available)) 3 else 2 )
    records <- records[records$available,]
  }
  
  ## create urls and md5 checksums
  records$gSD.md5.url <- sapply(records$url.alt, function(x) paste0(x, "Checksum/Value/$value"), USE.NAMES = F)
  records$gSD.md5 <- sapply(records$gSD.md5.url, function(x) content(gSD.get(x, cred[1], cred[2])), USE.NAMES = F)
  records$gSD.url.ds <- records$url
  
  ## create file names
  file_ext <-  ".zip"
  if(isTRUE(gnss)){
    file_ext <- ".TGZ"
  } else{
    if(isTRUE(grepl("Sentinel-5 Precursor", platform))) file_ext <-  ".nc" 
  }
  records$gSD.file <- sapply(records$identifier, function(x){
    paste0(dir_out, "/", x, file_ext)
  }, USE.NAMES = F) #download to file
  
  ## create console items
  records$gSD.name <- records$identifier
  records$gSD.item <- 1:nrow(records)
  records$gSD.head <- sapply(records$gSD.item, function(i, n = nrow(records)) paste0("[Dataset ", toString(i), "/", toString(n), "] "))
  
  ## download per record
  records <- gSD.retry(records, gSD.download, names = colnames(records), prog = getOption("gSD.verbose"), force = force, n.retry = n.retry)
  
  ## message the user
  if(any(!records$gSD.downloaded)){
    out(paste0("Some downloads have not been succesfull after ", max(records$gSD.attempts), " attempt(s) (see column 'gSD.downloaded'). Please retry later."), type = 2)
  } else{
    out(paste0("All downloads have been succesfull after ", max(records$gSD.attempts), " attempt(s)."), msg = T)
  }
  
  ## remove internal fields
  records <- records[,-sapply(c("gSD.url.ds", "gSD.name", "gSD.item", "gSD.head"), function(x, names = colnames(records)) which(names == x), USE.NAMES = F)]
  
  out(paste0("Columns added to records: '", paste0(setdiff(colnames(records), records.names), collapse = "', '"), "'"))
  return(records)
}
