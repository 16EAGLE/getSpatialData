#' Crop file to a spatial extent (beta)
#'
#' \code{cropFAST} crops a raster file to a spatial extent using GDAL. It is useful when working with large-scale, memory-intensive datasets.
#'
#' @inheritParams getSentinel_data
#' @param file Path to raster file.
#' @param ext Extent object or such from which an extent can be extracted.
#' @param filename Filename of output, if it should be written to disk.
#' @param ... additional arguments passed to \code{writeRaster}. Ignored, if \code{filename} ends with ".vrt".
#'
#' @details GDAL must be installed. If no GDAL installation is found, \code{cropFAST} is masking \code{raster::crop}.
#' The input file on disk will not be changed.
#'
#' @return Raster* object.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom gdalUtils gdal_setInstallation gdalbuildvrt
#' @importFrom raster raster extent intersect writeRaster crop nlayers
#'
#' @examples
#' \dontrun{
#' library(raster)
#'
#' ## load a file
#' path_tiff <- "/path/to/file.tiff"
#' r_tiff <- stack(path_tiff)
#'
#' ## define an AOI
#' data("aoi_data")
#' aoi <- aoi_data[[2]] # example AOI
#'
#' ## if necessary, reproject AOI to file CRS
#' aoi <- spTransform(aoi, crs(r))
#'
#' ## crop (file on disk will not be changed)
#' r_crop <- cropFAST(path_tiff, ext = aoi)
#'
#' ## crop to a new file
#' r_crop <- cropFAST(path_tiff, ext = aoi, filename = "/path/to/file_cropped.tiff")
#' }
#'
#' @seealso \link{getSentinel_data}
#' @export

cropFAST <- function(file, ext, filename, verbose = TRUE, ...){

  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  if(!inherits(ext, "Extent")){
    ext <- try(extent(ext), silent = T)
    if(!inherits(ext, "Extent")) out("No object of class 'Extent' could be derived from argument 'ext'.", type = 3)
  }
  if(!file.exists(file)) out(paste0("'", file, "' does not exist."), type = 3) else{
    if(is.null(intersect(extent(raster(file)), ext))) out("Extents of raster from 'file' and 'ext' do not overlap.", type = 3)
  }

  gdal.path <- getOption("gdalUtils_gdalPath")[[1]]$path
  if(!all(is.character(gdal.path) & nchar(gdal.path) > 0)){

    # do it natively
    out("GDAL could not be found. Using 'raster::crop' instead.", type = 2)
    x <- stack(file)
    crop(x, ext, ...)
  } else{

    # do it in gdal
    temp.env <- tempfile(fileext = ".vrt")
    catch <- gdalbuildvrt(file, temp.env, te = c(ext@xmin, ext@ymin, ext@xmax, ext@ymax))
    x <- stack(temp.env)
    if(!missing("filename")) if(!is.na(grep(".vrt", tolower(temp.env))[1])) file.rename(temp.env, filename) else writeRaster(x, ...)
    if(nlayers(x) == 1) raster(x) else x
  }
}
