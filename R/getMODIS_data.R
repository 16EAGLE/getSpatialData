#' Download MODIS data
#'
#' \code{getMODIS_data} downloads MODIS data queried using \link{getMODIS_query} from different sources.
#'
#' @inheritParams getMODIS_query
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{getMODIS_query}.
#' @param dir_out character, full path to download target directory. Optional. If not set, \code{getMODIS_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData archive folder.
#' @param force logical. If \code{TRUE}, download is forced even if file already exisits in the download directory. Default is \code{FALSE}.
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
#' query <- getMODIS_query(time_range = time_range, name = product)
#'
#' ## preview a record
#' getMODIS_preview(query[1,])
#'
#' ## download records 1 and 2
#' files <- getMODIS_data(query[1:2,])
#' }
#'
#' @importFrom getPass getPass
#'
#' @seealso \link{getMODIS_names} \link{getMODIS_query} \link{getMODIS_preview}
#' @export
#'
getMODIS_data <- function(records, dir_out = NULL, force = FALSE, verbose = TRUE){

  ## Argument checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)

  ## Check output directory
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)) dir_out <- paste0(getOption("gSD.archive_get"), "/MODIS/") else dir_out <- path.expand(dir_out)
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  if(!is.character(dir_out)) out(paste0("Argument 'dir_out' needs to be of type 'character'."), type = 3)
  if(!dir.exists(dir_out)) out("The defined archive directory does not exist.", type=3)

  ## Assemple request
  out(paste0("Starting download of product(s) '", paste0(records$displayId, collapse = "', "), "'."), msg = T)
  url.files <- apply(records, MARGIN = 1, function(x, root = getOption("gSD.api")$laads){
    y <- rbind.data.frame(x, stringsAsFactors = F)
    colnames(y) <- names(x)
    fn <- gsub("Entity ID: ", "", strsplit(y$summary, ", ")[[1]][1]) #positiona
    ydoy <- gsub("A", "", strsplit(fn, "[.]")[[1]][2]) #positional
    paste0(root, toString(as.numeric(strsplit(fn, "[.]")[[1]][4])), "/", strsplit(fn, "[.]")[[1]][1], "/", substr(ydoy, 1, 4),
           "/", substr(ydoy, 5, nchar(ydoy)), "/", fn)
  })

  dir.ds <- sapply(url.files, function(x, d = dir_out) paste0(d, "/", gsub(".hdf", "", tail(strsplit(x, "/")[[1]], n=1)[1])), USE.NAMES = F)
  catch <- sapply(dir.ds, function(x) dir.create(x, showWarnings = F))
  file.ds <- unlist(mapply(url = url.files, dir = dir.ds, FUN = function(url, dir){
    file <- paste0(dir, "/", tail(strsplit(url, "/")[[1]], n=1))
    gSD.download(name = tail(strsplit(url, "/")[[1]], n=1), url.file = url, file = file)
    return(file)
  }, SIMPLIFY = F))
  names(file.ds) <- NULL

  out(paste0("Requested records are stored in '", dir_out, "'."))
  return(file.ds)
}
