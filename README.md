# getSpatialData <a href="http://jxsw.de/getSpatialData"><img align="right" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_hex.png" /></a>

[![CRAN version](https://www.r-pkg.org/badges/version/getSpatialData)](https://CRAN.R-project.org/package=getSpatialData)
[![Build Status](https://travis-ci.org/16EAGLE/getSpatialData.svg?branch=master)](https://travis-ci.org/16EAGLE/getSpatialData) 
[![Coverage](https://codecov.io/gh/16eagle/getSpatialData/branch/master/graph/badge.svg)](https://codecov.io/gh/16EAGLE/getSpatialData)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

## Introduction

`getSpatialData` is an R package in an early development stage that ultimately aims to provide homogeneous function bundles to query, download, prepare and transform various kinds of spatial datasets from open sources, e.g. Satellite sensor data, higher-level environmental data products etc. It supports both `sf` and `sp` classes as AOI inputs (see `set_aoi` in  [available functions](http://jxsw.de/getSpatialData/#available-functions)). Due to the early development stage, the included functions and their concepts could be removed or changed in some cases.

For all public functions [documentation is available](http://jxsw.de/getSpatialData). See also the [list of data sources](http://jxsw.de/getSpatialData/#products) that are or will be implemented.

<b><font color="red">Please note:</font> Due to the temporary shutdown of the United States federal government, the on-demand processing through ESPA is currently not available. Thus, the `getLandsat_data()` function can currently download Level 1 data only. See more here: https://github.com/16EAGLE/getSpatialData/issues/15</b>

## Installation

To install the current beta version, use `devtools`.

```s
devtools::install_github("16EAGLE/getSpatialData")
```

## Available Functions

The following functions are publicly available and tested on Linux (Ubuntu 16.04 LTS, 17.10, 18.04 LTS) and Windows 10.

#### Sentinel

* `getSentinel_query()` – querys the Copernicus Open Access Hubs for Sentinel-1, Sentinel-2 and Sentinel-3 data and returns a data frame containing the found records (rows) and their attributes (columns).
* `getSentinel_preview()` – uses the output of `getSentinel_query()` to preview (quick-look) a user-selected record even before downloading it. By default, the preview is displayed corner-georeferenced in a map viewer in relation to the session AOI.
* `getSentinel_data()` – uses the output of `getSentinel_query()` to download Sentinel data.

#### Landsat

* `getLandsat_names()` – obtains available Landsat product names from USGS Earth Explorer, which can be optionally used with getLandsat_query() to narrow the search.
* `getLandsat_query()` – querys USGS Earth Explorer for Landsat data and returns a data frame containing the found records (rows) and their attributes (columns).
* `getLandsat_preview()` – uses the output of `getLandsat_query()` to preview (quick-look) a user-selected record. By default, the preview is displayed corner-georeferenced in a map viewer in relation to the session AOI.
* `getLandsat_data()` – uses the output of getLandsat_query() to order and download Landsat data.
    - supports order (on-demand processing) and download of higher-level products (all Landsat products), e.g. top-of-atmosphere (TOA), surface reflectance (SR) or different indices, from USGS-EROS ESPA.
    - supports direct download of Level-1 products (Landsat-8 only) via Amazon Web Services (AWS).
    - will support direct download of Level-1 products (all Landsat products) via USGS EarthExplorer (requires a USGS user profile with machine-to-machine download permission)


#### MODIS
* `getMODIS_names()` – obtains available MODIS product names from USGS Earth Explorer, which can be optionally used with getMODIS_query() to narrow the search.
* `getMODIS_query()` – querys USGS Earth Explorer for MODIS data and returns a data frame containing the found records (rows) and their attributes (columns).
* `getMODIS_preview()` – uses the output of `getMODIS_query()` to preview (quick-look) a user-selected record. By default, the preview is displayed corner-georeferenced in a map viewer in relation to the session AOI.
* `getMODIS_data()` – uses the output of getMODIS_query() to order and download MODIS data from LAADS.


#### Preprocessing
* `prepSentinel()` **beta** – makes downloaded Sentinel datasets ready-to-use by automatically inspecting, extracting, sorting and converting the relevant contents of the datasets to a user-defined format.
* `cropFAST()` **beta** – crops a raster file to a spatial extent using GDAL. It is useful when working with large-scale, memory-intensive datasets.


#### Session Login

* `login_CopHub()` – define your Copernicus Open Access login credentials once for the present R session to be able to call each `getSentinel*` function without defining login arguments each time you use them.
* `login_USGS()` – define your USGS login credentials once for the present R session to be able to call each `get*` function that connects to a USGS service without defining login arguments each time you use them.


#### Session Settings

* `set_archive()` – define a `getSpatialData` archive directory to which all `*_data` functions will download data.
* `set_aoi()` - draw or define an AOI as sf, sp or matrix object for the running session that can be used by all query functions.
* `view_aoi()` - display the session AOI in an interactive `mapview`/`leaflet` map viewer.
* `get_aoi()` - get the session AOI you have defined or drawn before as `sf`, `sp` or `matrix` object.


## Semantics

The following universal semantics on data are used by `getSpatialData` (from smallest to biggest entity):
* `image`: An image of a specific time and spatial extent.
* `record`: A set of meta fields identifying and describing a specific `image`, being part of multiple records in a `query`.
* `dataset`: Smallest entity that is delivered by a service. Might consist of multiple files, including meta data and bandwise imagery. Covers a specific time and spatial extent.
* `product`: A data product offered by a specific service, consisting of multiple datasets over a period of time and a wide spatial extent. Might be differentiated by:
    - `platform`: A general platform design (e.g. "Landsat" or "Sentinel").
    - `sensor`: Type of sensor which acquired the data from which the product originates (e.g. "MODIS", "MSI" or "OLI").
    - `collection`: A product version.
    - `level`: Processing level of the product (e.g. "Level 2A" or "Surface Reflectance").
    - `source`: The service acquiring, processing or distributing the product (e.g. "ESA Copernicus" or "USGS").

The following universal semantics on computational steps are used by `getSpatialData`:
* `get`: Recieve data from different sources, named either by `sensor` or `platform` (whichever is used by the scientific community to referr to the derived products)
    - `names`: Result of searching available products (differs by `source` and `platform`), which might be differentiated further later on (e.g. by `level`).
    - `query`: Result of searching a `source` for data `records` of a specific or multiple `products`.
    - `preview`: Preview a `record`.
    - `data`: Result of recieving one or multiple `dataset` from a `source`.
* `prep`: Prepare/preprocess data obtained with `get`

## Get Started

The following code represents a working chain for querying, filtering, previewing and downloading Sentinel-2 data within R. The procedure can be done for Sentinel-1, Sentinel-2 or Sentinel-3.

```R
## Load packages
library(getSpatialData)
library(raster)
library(sf)
library(sp)

## Define an AOI (either matrix, sf or sp object)
data("aoi_data") # example aoi

aoi <- aoi_data[[3]] # AOI as matrix object, or better:
aoi <- aoi_data[[2]] # AOI as sp object, or:
aoi <- aoi_data[[1]] # AOI as sf object
#instead, you could define an AOI yourself, e.g. as simple matrix

## set AOI for this session
set_aoi(aoi)
view_aoi() #view AOI in viewer, which will look like this:
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_view_aoi.png"></p>
<p align="center"><sub>Figure 1: Screenshot of the RStudio Viewer, displaying the previously defined session AOI using view_aoi()</sub></p>
<br>


```R
#instead of using an existing AOI, you can simply draw one:
set_aoi() #call set_aoi() without argument, which opens a mapedit editor:
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_create_aoi.png"></p>
<p align="center"><sub>Figure 2: Screenshot of the RStudio Viewer, displaying the mapedit editor allowing the user to draw a session AOI</sub></p>
<br>


```R
## After defining a session AOI, define time range and platform
time_range <-  c("2017-08-01", "2017-08-30")
platform <- "Sentinel-2" #or "Sentinel-1" or "Sentinel-3"

## set login credentials and archive directory
login_CopHub(username = "username") #asks for password or define 'password'
set_archive("/path/to/archive/")

## Use getSentinel_query to search for data (using the session AOI)
records <- getSentinel_query(time_range = time_range, platform = platform)

## Filter the records
colnames(records) #see all available filter attributes
unique(records$processinglevel) #use one of the, e.g. to see available processing levels

records_filtered <- records[which(records$processinglevel == "Level-1C"),] #filter by Level
records_filtered <- records_filtered[as.numeric(records_filtered$cloudcoverpercentage) <= 30, ] #filter by clouds

## View records table
View(records)
View(records_filtered)
#browser records or your filtered records
```

<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_query_table.png"></p>
<p align="center"><sub>Figure 3: Screenshot of the View() display in RStudio, displaying a filtered records table produced by getSentinel_query()</sub></p>
<br>


```R
## Preview a single record on a mapview map with session AOI
getSentinel_preview(record = records_filtered[9,])
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_preview.png"></p>
<p align="center"><sub>Figure 4: Screenshot of the RStudio viewer, displaying a corner-georeferenced Sentinel-2 preview and the session AOI using getSentinel_preview()</sub></p>
<br>


```R
## Preview a single record on a mapview map without session AOI
getSentinel_preview(record = records_filtered[9,], show_aoi = FALSE)
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_preview_no_aoi.png"></p>
<p align="center"><sub>Figure 5: Screenshot of the RStudio viewer, displaying a corner-georeferenced Sentinel-2 preview using getSentinel_preview()</sub></p>
<br>


```R
## Preview a single record as RGB plot
getSentinel_preview(record = records_filtered[9,], on_map = FALSE)
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_preview_plotRGB.png"></p>
<p align="center"><sub>Figure 6: Screenshot of the RStudio viewer, displaying a simple Sentinel-2 RGB plot preview using getSentinel_preview()</sub></p>
<br>


```R
## Download some datasets to your archive directory
datasets <- getSentinel_data(records = records_filtered[c(4,7,9), ])

## Finally, define an output format and make them ready-to-use
datasets_prep <- prepSentinel(datasets, format = "tiff")
# or use VRT to not store duplicates of different formats
datasets_prep <- prepSentinel(datasets, format = "vrt")

## View the files
datasets_prep[[1]][[1]][1] #first dataset, first tile, 10 m resolution
datasets_prep[[1]][[1]][2] #first dataset, first tile, 20 m resolution
datasets_prep[[1]][[1]][3] #first dataset, first tile, 60 m resolution

## Load them directly into R
r <- stack(datasets_prep[[1]][[1]][1])

```

## Products

The following products are being evaluated to be implemented within the package. This also includes sources which can be already accessed through existing packages that could be wrapped behind an standardized R function interface. Please feel free to contribute to the list, e. g. through a pull request:

| Product(s) | Source | Access | Status | Client(s) | 
| ---------- | --------------- | --- | -------| ----------- |
| Sentinel (-1/-2/-3) | ESA Copernicus | <a target="_blank" href="https://scihub.copernicus.eu/userguide/5APIsAndBatchScripting">Copernicus Open Access Hub API</a>  | implemented | native |
| MODIS | NASA/USGS | <a target="_blank" href="https://modis.ornl.gov/data/modis_webservice.html">ORNL DAAC SOAP MODIS web service</a>, <a target="_blank" href="https://ladsweb.modaps.eosdis.nasa.gov/tools-and-services/lws-classic/api.php"> LAADS DAAC SOAP/REST web service</a> | implemented | native |
| Landsat | USGS | <a target="_blank" href="https://earthexplorer.usgs.gov/inventory/documentation/json-api">USGS EarthExplorer json API</a>, <a target="_blank" href="https://landsat.usgs.gov/landsat-data-access">USGS-EROS ESPA</a>, <a target="_blank" href="https://registry.opendata.aws/landsat-8/">AWS</a> | implemented | native |
| Global Forest Change | Hansen et al. | http://azvoleff.com/articles/analyzing-forest-change-with-gfcanalysis | evaluated | R: `gfcanalysis`? |
| CMIP5/PMIP3 Global Climate | ecoClimate | http://ecoclimate.org/about/ | evaluated | R: `ecoClimate`? |
| Copernicus Global Land Products | ESA Copernicus | http://land.copernicus.eu/ | evaluated | |
| CHELSA Global Land Climate | Karger et al. | http://chelsa-climate.org/ | evaluated | |
| Global Forest Cover | EU-JRC | http://remote-sensing-biodiversity.org/forest-cover-and-forest-cover-pattern-data-by-jrc/ | evaluated | |
| Global Surface Dynamics | EU-JRC | http://remote-sensing-biodiversity.org/global-water-dynamics-data/ | evaluated | |
| Global Soil Grids | Hengl et al. | http://remote-sensing-biodiversity.org/global-soil-data-soilgrids/ | evaluated | |
| Global Urban Footprint | Esch et al. | https://urban-tep.eo.esa.int/geobrowser/?id=portfolio#!&context=GUF%2FGUF2012-12m | evaluated | |
| UK Urban Areas LiDAR | UK Environment Agency | http://remote-sensing-biodiversity.org/free-lidar-data-for-some-uk-cities/ | evaluated | |
| Global Human Built-up And Settlement Extent (HBASE)| Wang et al. | http://sedac.ciesin.columbia.edu/data/set/ulandsat-hbase-v1 | evaluated | |
| GIMMS NDVI3g | NASA | https://nex.nasa.gov/nex/projects/1349/ | evaluated | R: `GIMMS`? |


## Contribution

Contribute! I'm happy about any kind of contribution, from feature ideas, ideas on possible data sources, technical ideas or other to bug fixes, code suggestions or larger code contributions! Open an issue to start a discussion: <https://github.com/16eagle/getSpatialData/issues> 


## Mentioned

`getSpatialData` has been mentioned here:

Kwok, R., 2018. Ecology’s remote-sensing revolution. Nature 556, 137. https://doi.org/10.1038/d41586-018-03924-9



