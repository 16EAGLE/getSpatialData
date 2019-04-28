#' Calculate the cloud cover of Sentinel-2 within an aoi
#' 
#' \code{calcSentinel_aoi_cloudcov} estimates the cloud cover of Sentinel-2 data based on preview images using the haze-optimal transformation (HOT)
#' 
#' @details The estimation of the cloud cover is done on the red and blue information of the preview images provided by the respective data dissiminator.
#'  
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{getSentinel_query}.
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param maxDeviation numeric, the maximum allowed deviation of calculated scene cloud cover from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover % given by the data distributor. Default is \code{maxDeviation = 20}.
#' @param sceneCloudCoverCol string, the clear name of the cloud cover column in the \code{record} data.frame provided by the data distributor.
#' @param cloudPrbThreshold numeric, the threshold of the HOT cloud probability layer (0-100 %) below which pixels are considered as clear sky. Default is \code{cloudPrbThreshold = 40}. It will be dynamically adjusted according to the input in \code{maxDeviation}
#' @param slopeDefault numeric, value taken as slope ONLY if least-alternate deviation regression fails.  Default is 1.4, proven to work well for common land surfaces.f default values. In this case cloud cover will be set to 9999 % for the given record.
#' @param interceptDefault, value taken as intercept ONLY if least-alternate deviation regression fails. Default is -10, proven to work well for common land surfaces.
#' @param dir_out character, optional. Full path to target directory where to save the cloud masks. If \code{NULL}, cloud masks are not saved.
#' @param username character, a valid user name to the ESA Copernicus Open Access Hub. If \code{NULL} (default), the session-wide login credentials are used (see \link{login_CopHub} for details on registration).
#' @param password character, the password to the specified user account. If \code{NULL} (default) and no seesion-wide password is defined, it is asked interactively ((see \link{login_CopHub} for details on registration).
#' @param verbose logical, if \code{TRUE}, details on the function's progress will be visibile on the console. Default is TRUE.
#' 
#' @return A data.frame as the records input with one additional column holding the estimated cloud cover within the aoi
#'
#' @author Henrik Fisser
#' 
#' @importFrom stringi stri_split_fixed
#' @importFrom utils object.size
#' 
#' @export

calcSentinel_aoi_cloudcov <- function(records, aoi = NULL,  maxDeviation = 20, sceneCloudCoverCol, cloudPrbThreshold = 40, slopeDefault = 1.4, interceptDefault = -10, dir_out = NULL, username = NULL, password = NULL, verbose = TRUE) {
  
  ## Define Sentinel-2-specific parameters
  tileID <- "tileid" # for later use the column name holding the tile ID
  sceneCloudCoverCol <- "cloudcoverpercentage" # for later use the column name holding the scene cloud cover
  numRecords <- NROW(records)
  
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
  
  numRecords <- NROW(records)
  quarterNumRecords <- round(numRecords/4)
  processingTime <- c()
  previewSize <- c()
  ## Do HOT cloud cover assessment consecutively
  records <- do.call(rbind,lapply(1:numRecords,function(i) {
    startTime <- Sys.time()
    # get preview of current record
    currRecord <- records[i,]
    preview <- getSentinel_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                        username=username,password=password,verbose=verbose)
    previewSize <- c(previewSize,object.size(preview))
    # pass preview to HOT function
    currRecCloudCover <- calc_hot_cloudcov(records=records,preview=preview,aoi=aoi,maxDeviation=maxDeviation,sceneCloudCoverCol=sceneCloudCoverCol,
                                           slopeDefault=slopeDefault,interceptDefault=interceptDefault,dir_out=dir_out,verbose=verbose)
    endTime <- Sys.time()
    if (i <= 5) {elapsed <- round(as.numeric(difftime(endTime,startTime,units="mins")))}
    processingTime <- c(processingTime,elapsed)
    if (numRecords >= 10 && i == 5) {
      .calcProcTime(numRecords=numRecords,quarterNumRecords=quarterNumRecords,i=i,processingTime=processingTime,previewSize=previewSize)
    }
  }))
  
  return(records)
}




