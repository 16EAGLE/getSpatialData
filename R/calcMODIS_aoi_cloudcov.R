#' Calculate the cloud cover of MODIS data within an aoi
#' 
#' \code{calcMODIS_aoi_cloudcov} estimates the cloud cover of Landsat data based on preview images using the haze-optimal transformation (HOT)
#' 
#' @details The estimation of the cloud cover is done on the red and blue information of the preview images provided by the respective data dissiminator.
#'  
#' @inheritParams calcSentinel_aoi_cloudcov
#' 
#' @return A data.frame as the records input with one additional column holding the estimated cloud cover within the aoi
#'
#' @author Henrik Fisser
#' 
#' @seealso \link{getMODIS_query} \link{getMODIS_preview} \link{getMODIS_data}
#'  
#' @export

calcMODIS_aoi_cloudcov <- function(records, aoi = NULL,  maxDeviation = 20, cloudPrbThreshold = 40, 
                                     slopeDefault = 1.4, interceptDefault = -10, dir_out = NULL, 
                                     username = NULL, password = NULL, verbose = TRUE) {
  
  sceneCloudCoverCol <- "SceneCloudCover" # for later use the column name holding the scene cloud cover
  sensor <- "MODIS"
  
  records <- .hotBridge(sensor=sensor,sceneCloudCoverCol=sceneCloudCoverCol,records=records,aoi=aoi,maxDeviation=maxDeviation,
                        cloudPrbThreshold=cloudPrbThreshold,slopeDefault=slopeDefault,
                        interceptDefault=interceptDefault,dir_out=dir_out,username=username,password=password,verbose=verbose)
  
  return(records)
}