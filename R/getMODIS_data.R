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
#' @details \code{getMODIS_data} downloads MODIS data from the Level-1 and Atmosphere Archive & Distribution System (LAADS) of NASA's Distributed Active Archive Center (DAAC) at the Goddard Space Flight Center in Greenbelt, Maryland (\url{https://ladsweb.modaps.eosdis.nasa.gov/}). The function uses the LAADC MODIS R client being part of the \strong{MODIS} R package written by Florian Detsch & Matteo Mattiuzzi (see \url{ https://github.com/MatMatt/MODIS}).
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
#' @importFrom MODIS MODISoptions getHdf
#'
#' @seealso \link{getMODIS_names} \link{getMODIS_query} \link{getMODIS_preview}
#' @export
#'
getMODIS_data <- function(records, dir_out = NULL, force = FALSE, verbose = TRUE){

  ## Argument checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)

  ## Check output directory
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)){dir_out <- paste0(getOption("gSD.archive_get"))}
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  if(!is.character(dir_out)) out(paste0("Argument 'dir_out' needs to be of type 'character'."), type = 3)
  if(!dir.exists(dir_out)) out("The defined archive directory does not exist.", type=3)

  ## Define MODIS options & check source (not implemted yet, authentification needed for LP DAAC)
  source <- "auto"
  MODIS.opt <- paste0("MODISoptions(localArcPath = dir_out, outDirPath = getOption('gSD.archive_prep')")
  if(source != "auto"){
    if(source != "LAADS" & source != "LPDAAC") out("Argument 'source' must be either 'auto', 'LAADS' or 'LPDAAC'", type = 3)
    MODIS.opt <- paste0(MODIS.opt, ", MODISserverOrder = '", source, "'")
  }
  sink(file = tempfile(fileext = ".txt"))
  tmp <- suppressMessages(eval(parse(text = paste0(MODIS.opt, ", quiet = F)"))))
  sink(file = NULL)

  ## Assemple getHdf
  records$product <- .convMODIS_names(records$product)
  out(paste0("Starting download of product(s) '", paste0(records$displayId, collapse = "', "), "'."), msg = T)
  file.ds <- unlist(apply(records[1:2,], MARGIN = 1, function(x, d = paste0(dir_out, "/MODIS")){
    y <- rbind.data.frame(x, stringsAsFactors = F)
    colnames(y) <- names(x)
    out(paste0("Attempting to download '", y$displayId, "' into '", d, "'..."), msg = T)
    getHdf(product = y$product, begin = y$acquisitionDate, end = y$acquisitionDate, tileH = y$HorizontalTileNumber,
           tileV = y$VerticalTileNumber, collection = "006")
  }))
  names(file.ds) <- NULL

  out(paste0("Requested records are stored in '", dir_out, "'."))
  return(file.ds)
}
