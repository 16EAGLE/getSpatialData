#' Download MODIS data
#'
#' \code{getMODIS_data} downloads MODIS data queried using \link{getMODIS_records} from different sources.
#'
#' @inheritParams getMODIS_records
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{getMODIS_records}.
#' @param dir_out character, full path to download target directory. Optional. If not set, \code{getMODIS_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData archive folder.
#' @param force logical. If \code{TRUE}, download is forced even if file already exisits in the download directory. Default is \code{FALSE}.
#' @param n.retry numeric, maximum number of download (re-)attempts. If downloads of datasets fail (e.g. MD5 checksums do not match), these downloads will be reattampted.
#'
#' @return Character vector of paths to the downloaded files.
#'
#' @details \code{getMODIS_data} downloads MODIS data from the Level-1 and Atmosphere Archive & Distribution System (LAADS) of NASA's Distributed Active Archive Center (DAAC) at the Goddard Space Flight Center in Greenbelt, Maryland (\url{https://ladsweb.modaps.eosdis.nasa.gov/}).
#'
#' @author Jakob Schwalb-Willmann
#'
#' @examples
#' ## Load packages
#' library(getSpatialData)
#' library(sf)
#'
#' ## set aoi and time range for the query
#' set_aoi(aoi_data[[1]])
#' time_range <-  c("2017-08-01", "2017-08-30")
#'
#' ## Login to USGS ERS
#' \dontrun{
#' login_USGS("username")
#'
#' ## set archive directory
#' set_archive("/path/to/archive/")
#'
#' ## get available products and select one
#' product_names <- getMODIS_names()
#' product <- grep("MOD13Q1", product_names, value = T)
#'
#' ## query for records for your AOI, time range and product
#' query <- getMODIS_records(time_range = time_range, name = product)
#'
#' ## preview a record
#' getMODIS_preview(query[1,])
#'
#' ## download records 1 and 2
#' files <- getMODIS_data(query[1:2,])
#' }
#'
#' @importFrom getPass getPass
#' @importFrom httr http_error
#'
#' @seealso \link{getMODIS_names} \link{getMODIS_records} \link{getMODIS_preview}
#' @keywords internal
#'
getMODIS_data <- function(records, dir_out = NULL, force = FALSE, n.retry = 3, verbose = TRUE){

  ## Argument checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)

  ## Check output directory
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)) dir_out <- paste0(getOption("gSD.archive_get"), "/MODIS/") else dir_out <- path.expand(dir_out)
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  if(!is.character(dir_out)) out(paste0("Argument 'dir_out' needs to be of type 'character'."), type = 3)
  if(!dir.exists(dir_out)) out("The defined archive directory does not exist.", type=3)

  ## collect record column names
  records.names <- colnames(records)
  
  ## Assemple request
  out("Checking download URLs for requested records...")
  records$dataset_url <- apply(records, MARGIN = 1, function(x, root = getOption("gSD.api")$laads){
    y <- rbind.data.frame(x, stringsAsFactors = F)
    colnames(y) <- names(x)
    fn <- gsub("Entity ID: ", "", strsplit(y$summary, ", ")[[1]][1]) #positional
    ydoy <- gsub("A", "", strsplit(fn, "[.]")[[1]][2]) #positional
    
    paste0(root, toString(as.numeric(strsplit(fn, "[.]")[[1]][4])), "/", strsplit(fn, "[.]")[[1]][1], "/", substr(ydoy, 1, 4),
           "/", substr(ydoy, 5, nchar(ydoy)), "/", fn)
    
  })

  # change URLs if erroring
  records$dataset_url <- sapply(records$dataset_url, function(x){
    if(http_error(x)){
      
      fn.names <- gSD.get(paste0(paste0(head(strsplit(x, "/")[[1]], n = -1), collapse = "/"), ".csv"))
      fn.names <- sapply(gsub("\r", "", strsplit(content(fn.names), "\n")[[1]][-1]), function(x) strsplit(x, ",")[[1]][1], USE.NAMES = F)
      
      # assign correct fn
      fn <- grep(paste0(strsplit(tail(strsplit(x, "/")[[1]], n=1), "[.]")[[1]][1:4], collapse = "."), fn.names, value = T)
      
      # redefine URL and file
      return(paste0(paste0(head(strsplit(x, "/")[[1]], n=-1), collapse = "/"), "/", fn))
    } else return(x)
  })
  

  records$dataset_dir <- sapply(records$dataset_url, function(x, d = dir_out) paste0(d, "/", gsub(".hdf", "", tail(strsplit(x, "/")[[1]], n=1)[1])), USE.NAMES = F)
  catch <- sapply(records$dataset_dir , function(x) dir.create(x, showWarnings = F))
  
  ## create file names
  records$dataset_file <- unlist(mapply(url = records$dataset_url, dir = records$dataset_dir, FUN = function(url, dir){
    paste0(dir, "/", tail(strsplit(url, "/")[[1]], n=1))
  }, SIMPLIFY = F, USE.NAMES = F))
  
  ## create console items
  records$dataset_name <- unlist(sapply(records$dataset_url, function(u) sapply(u, function(x) tail(strsplit(x, "/")[[1]], n=1), USE.NAMES = F), USE.NAMES = F, simplify = F))
  records$gSD.item <- 1:nrow(records)
  records$gSD.head <- sapply(records$gSD.item, function(i, n = nrow(records)) paste0("[Dataset ", toString(i), "/", toString(n), "] "))
  
  ## download per record
  records <- gSD.retry(records, gSD.download, names = colnames(records), prog = getOption("gSD.verbose"), force = force, n.retry = n.retry)
  
  return(.column_summary(records, records.names, download_success = T))
}
