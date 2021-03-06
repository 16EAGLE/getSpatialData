% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_cloudcov.R
\name{calc_cloudcov}
\alias{calc_cloudcov}
\title{Calculate the cloud cover of optical Sentinel, Landsat or MODIS data in an aoi based on small previews}
\usage{
calc_cloudcov(
  records,
  max_deviation = 2,
  aoi = NULL,
  write_records = TRUE,
  write_cloud_masks = TRUE,
  dir_out = NULL,
  username = NULL,
  password = NULL,
  as_sf = TRUE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{records}{data.frame, one or multiple records (each represented by one row), as it is 
returned by \link{get_records}.}

\item{max_deviation}{numeric, the maximum allowed deviation of calculated scene cloud cover 
from the provided scene cloud cover. Use 100 if you do not like to consider the cloud cover 
\% given by the data distributor. Default is 2.}

\item{aoi}{sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point 
(at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to 
have two columns (longitude and latitude) and at least three rows (each row representing one 
#corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, 
it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally 
for all queries within the running session. If \code{aoi} is undefined, the AOI that has been 
set using \link{set_aoi} is used.}

\item{write_records}{logical specifies if the records (row by row) shall be written.}

\item{write_cloud_masks}{logical specifies if the cloud mask tifs shall be written.}

\item{dir_out}{character. If \code{dir_out} is not NULL the given cloud mask rasters and 
a record file for each record will be saved in \code{dir_out}. If it is NULL, the session 
\code{dir_out} is used.
If no session \code{dir_out} is set through \link{set_archive} an error is thrown.}

\item{username}{character, a valid user name to the ESA Copernicus Open Access Hub. If \code{NULL} 
(default), the session-wide login credentials are used (see \link{login_CopHub} for details on 
registration).}

\item{password}{character, the password to the specified user account. If \code{NULL} (default) 
and no seesion-wide password is defined, it is asked interactively ((see \link{login_CopHub} for 
details on registration).}

\item{as_sf}{logical, whether records should be returned as \code{sf} \code{data.frame} or a simple \code{data.frame}. In both cases, spatial geometries are stored in column \code{footprint}.}

\item{verbose}{logical, if \code{TRUE}, details on the function's progress will be visibile 
on the console. Default is TRUE.}

\item{...}{further arguments that can be passed to \link{write_records} for writing record files.
Can be: driver, append.}
}
\value{
\code{records} data.frame with three added columns:
\enumerate{
\item cloud_mask_file: character path to the cloud mask file of the record
\item aoi_HOT_cloudcov_percent: numeric percentage of the calculated aoi cloud cover.
\item scene_HOT_cloudcov_percent: numeric percentage of the calculated scene cloud cover.
}
}
\description{
\code{calc_cloudcov} calculates the aoi cloud cover and optionally saves raster cloud
masks, all based on preview images. The previews are requested through \link{get_previews}. 
You may call \link{get_previews} before \code{calc_cloudcov}. In this case the previews will be reloaded.
If one or more records have been processed in \code{calc_cloudcov} and in the same \code{dir_out} before 
they will be reloaded.
}
\details{
Using the Haze-optimal transformation (HOT), the cloud cover estimation is done on the 
red and blue information of the input RGB. HOT procedure is applied to the red and blue bands [1-3]. 
Originally, the base computation was introduced by Zhang et al. (2002) [2]. 
The computation done in \code{calc_cloudcov} includes the following steps:
\enumerate{
\item Binning: extract low red values and their highest blue values
\item Regression: calculate linear regression of these values
\item HOT layer: compute haze-optimal transformation cloud likelihood layer
\item Iterative thresholding: Find a HOT threshold by iterative comparison with
the provider scene cloud cover.
\item Aoi cloud cover calculation: Calculate the aoi cloud cover from the binary
cloud mask.
}

HOT separates clear-sky 
pixels first from a threshold, calculates a linear regression from these pixels and exposes 
cloud pixels by the deviation of all pixels from this clear-sky line.
}
\references{
[1] Chen, S, Chen, X., Chen, J., Jia, P., 2015. An Iterative Haze Optimized Transformation for 
Automatic Cloud/Haze
Detection of Landsat Imagery. IEEE Transactions on Geoscience and Remote Sensing 54 (5), 2682-2694.

[2] Zhang, Y., Guindon, B., Cihlar, J., 2002. An image transform to characterize and compensate 
for spatial variations in thin cloud contamination of Landsat images.
Remote Sensing of Environment 82 (2-3), 173-187.
  
[3] Zhu, X., Helmer, E.H., 2018. An automatic method for screening clouds and cloud shadows in 
opticalsatellite image time series in cloudy regions.
Remote Sensing of Environment 214 (2018), 135-153.
}
\author{
Henrik Fisser
}
