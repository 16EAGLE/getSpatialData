#' Calculate the cloud cover of Sentinel-2 within an aoi
#' 
#' \code{calcSentinel_aoi_cloudcov} estimates the cloud cover of Sentinel-2 data based on preview images using the haze-optimal transformation (HOT)
#' 
#' @details The estimation of the cloud cover is done on the red and blue information of the preview images provided by the respective data distributor. Haze-optimal transformation (HOT) procedure is applied based on 
#' Zhu & Helmer (2018), https://data.fs.usda.gov/research/pubs/iitf/ja_iitf_2018_Zhu.pdf. Orignally, the algorithm was introduced by Zhang et al. (2002)
#' "An image transform to characterize and compensate for spatial variations in thin cloud contamination of Landsat images", Remote Sensing of Environment 82, 2-3.
#' HOT seperates clear-sky pixels first from a threshold, calculates a least alternate deviation (LAD) regression from these pixels and exposes cloud pixels by the deviation of all pixels from this clear-sky line.
#' 
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{getSentinel_query}.
#' @param dir_previews 
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param save_masks lgoical, if \code{TRUE}, the cloud masks are saved in \code{dir_out} directory. Default is TRUE.
#' @param dir_out character, full path to target directory where to save the cloud masks. Optional, only used if \code{save_masks = TRUE}. If not set, \code{getLandsat_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData  archive folder.
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#' 
#' @return A data.frame as the records input with one additional column holding the estimated cloud cover within the aoi
#'
#' @author Henrik Fisser
#' 
#' @importFrom stringi stri_split_fixed
#' 
#' @export

calcSentinel_aoi_cloudcov <- function(records, aoi = NULL, save_masks = TRUE, dir_out = NULL, verbose = TRUE) {
  
  ## Global AOI
  if(is.null(aoi)) {
    if(isTRUE(getOption("gSD.aoi_set"))) {
      aoi <- getOption("gSD.aoi")
    } else {
      out("Argument 'aoi' is undefined and no session AOI could be obtained. Define aoi or use set_aoi() to define a session AOI.", type = 3)
    }
  }
  
  ## Define Sentinel-2-specific parameters
  urlCol <- "url.icon" # the column of the preview icon url
  tileID <- "tileid" # for later use the column name holding the tile ID
  sceneCloudCoverColumn <- "cloudcoverpercentage" # for later use the column name holding the scene cloud cover
  # some data.frames have no tile id column or NA values in tile id column
  # thus, in all cases the tile id is extracted from the title of the record
  records <- lapply(records,function(x) {
    titleSplit <- stringi::stri_split_fixed(x[["title"]],"_")
    tileid <- sapply(1:length(titleSplit),function(i) {
      id <- titleSplit[[i]][6] # get tileid from filename column
    })
    x[[tileID]] <- tileid
    return <- x
  })
  
  ## Calculate HOT
  records <- calc_hot_cloudcov(records = records, aoi = aoi, save_masks = save_masks, dir_out = dir_out, verbose = verbose)
}




