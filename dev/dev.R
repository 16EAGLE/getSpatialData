setwd("~/Documents/dev/getSpatialData/")
library(getPass)
library(httr)
library(xml2)
library(raster)
library(sf)
library(lwgeom)
library(gdalUtils)
library(mapview)
library(mapedit)
library(cli)
library(RStoolbox)
library(ggplot2)
library(patchwork)
library(pbapply)

source("R/checks.R")
source("R/clients.R")
source("R/internal_clients.R")
source("R/internal_names.R")
source("R/internal.R")
source("R/out_communication.R")

# check modis download
# check login_earthdata and login_codede
# check SRTM


library(sf)
library(getSpatialData)
set_archive("/media/Data/data/env/testing")
login_CopHub(username = "16eagle")
login_USGS(username = "16eagle")
# login_earthdata(username = "16eagle")
# login_codede(username = "16eagle")
# services()

# set aoi
aoi <- st_as_sfc(st_bbox(
  c(xmin =  7.542358, ymin = 47.604593, xmax = 7.654205, ymax = 47.708532), crs = st_crs(4326)))
set_aoi(aoi)
view_aoi()

#aoi <- st_read("~/Documents/test.gpkg")
#set_aoi(st_geometry(aoi))

# get data
records <- get_records(time_range = c("2019-08-12", "2019-08-24"),
                       products = c("sentinel-1"))
records <- get_records(time_range = c("2019-08-12", "2019-08-24"),
                       products = c("sentinel-1"))


aoi = NULL
as_sf = TRUE
rename_cols = TRUE
check_products = TRUE
simplify_cols = TRUE
verbose = TRUE
extras <- list()

# get previews
records <- records[records$level == "Level-2A",]
records <- get_previews(records)

# fitler by AOI cloud cover
records <- calc_cloudcov(records) 
records$aoi_HOT_cloudcov_percent
records <- records[records$aoi_HOT_cloudcov_percent < 25,]

# check availability
records <- check_availability(records)
records <- order_data(records, wait_to_complete = T)
records <- get_data(records)



records <- get_records(time_range = c("2021-06-16", "2021-08-02"),
                       products = c("landsat_8_c1"))
records <- records[records$level == "sr",]
records <- get_previews(records)
records <- calc_cloudcov(records) 


records <- check_availability(records)
records <- order_data(records)
records <- get_data(records)

