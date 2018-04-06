# getSpatialData

## Introduction

`getSpatialData` is an R package in an early development stage that ultimately aims to provide homogeneous function bundles to query, download, prepare and transform various kinds of spatial datasets from open sources, e.g. Satellite sensor data, higher-level environmental data products etc. The current version is a beta version, meant to be used for functionality tests. The included functions and their concepts are exploratory and could be removed or changed.

`getSpatialData` supports both `sf` and `sp` classes as AOI inputs.

## State of development

Lately, an R-native getSentinel function bundle has been deployed, which enables the user to easily query, preview and download Sentinel-1, -2 and -3 data directly from R. The client is coded in R and works independently from external libraries. It is currently tested for minor bugs (please report bugs if you find some).

A universal AOI defintion function has been implemented (see `set_aoi`). It understands different spatial objects representing the user's AOI and translates them depending on the client function that the user wants to use. It supports multi-point polygon shape objects (sp and sf) or matrix objects.

Currently, a NASA EE and USGS ESPA R client is being implemented, which will allow the user to query, preview, process-on-demand and download imagery from Landsat, MODIS etc. directly within R. The client is wirtten in R and will be published as soon as its base functionalities can be tested.


### Available functions

The following functions are publicly available and have been tested on Linux (Ubuntu 16.04 LTS, 17.10) and Windows 10:

#### main functions

* `getSentinel_query()` – querys the Copernicus Open Access Hubs for Sentinel-1, -2 and -3 data and returns a data frame containing the found datasets (rows) and their attributes (columns).
* `getSentinel_preview()` – uses the output of `getSentinel_query()` to preview an user-selected, individual dataset within in an R plotting device without the necessity to download the dataset first.
* `getSentinel_data()` – uses the output of `getSentinel_query()` to download the specified datasets to a local directory as .zip files. A transform function bundle helping to deal with the downloaded files within R without expert knowledge will follow soon.


#### helper functions

* `set_login_CopHub` – define your Copernicus Open Access login credentials once for the present R session to be able to call each `getSentinel*` function without defining login arguments each time you use them.
* `set_archive` – define a `getSpatialData` archive directory to which all `*_data` functions will download data.
* `set_aoi` - define an AOI as sf, sp or matrix object for the running session that can be used by all query functions.
* `view_aoi` - display the session AOI in an interactive `mapview`/`leaflet` map viewer.


### Manuals

For all current functions publicly available, documentation is available, containing information on the expected arguments, the return and examples. The files can be accessed executing a command like `?getSentinel_query`.


## Installation

To install the current beta version, use `devtools`.

```s
devtools::install_github("16EAGLE/getSpatialData")
```


## Example

The following code represents a working chain for querying, filtering, previewing and downloading Sentinel-2 data within R. This can be also done for Sentinel-1 or -3.

```
## Load packages
library(getSpatialData)
library(sf)
library(sp)

## Define an AOI (either matrix, sf or sp object)
data("aoi_data") # example aoi

aoi <- aoi_data[[3]] # AOI as matrix object, or better:
aoi <- aoi_data[[2]] # AOI as sp object, or:
aoi <- aoi_data[[1]] # AOI as sf object

## set AOI for this session
set_aoi(aoi)
view_aoi() #view AOI in viewer

## Define time range and platform
time_range <-  c("2017-08-01", "2017-08-30")
platform <- "Sentinel-2"

## set login credentials and archive directory
set_login_CopHub(hub_user = "16eagle") #asks for your password, if argument 'hub_pass' is not defined
set_archive("/home/jakob/Dokumente/wd/tmp/gSD/")

## Use getSentinel_query to search for data (using the session AOI)
products <- getSentinel_query(time_range = time_range, platform = platform)

## Get an overview of the products
View(products) #get an overview about the search products
## displays similar to this in RStudio:
```

<p align="center"><img width="80%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/view.png"></p>
<p align="center"><sub>Figure 1: Screenshot of the RStudio View() browser, displaying the products data.frame returned by getSentinel_query</sub></p>
<br>


```
## Filter the products
colnames(products) #see all available filter attributes
unique(products$processinglevel) #use one of the, e.g. to see available processing levels

products_filtered <- products[which(products$processinglevel == "Level-1C"),] #filter by Level

## Preview a single product
getSentinel_preview(product = products_filtered[5,])
# This will plot a preview to the active plotting device, is plotted like this:
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/preview.png"></p>
<p align="center"><sub>Figure 2: Screenshot of the RStudio plotting window, displaying a product preview returned by getSentinel_preview</sub></p>
<br>

```
## Download some datasets to your archive directory
files <- getSentinel_data(products = products_filtered[c(4,5,6), ])

```

## Ideas

Ideas on possible data sources to be included, technical ideas or other are welcome! Open an issue to start a discussion: <https://github.com/16eagle/getSpatialData/issues> 

### Datasets

The following data sources are being evaluated to be implemented within the package. This also includes sources which can be already accessed through existing packages that could be wrapped behind an standardized R function interface. Please feel free to contribute to the list, e. g. through a pull request:

| Product(s) | Source | API/URL | Status | Contributor | Remark | 
| ---------- | --------------- | --- | -------| ----------- | ------ |
| Sentinel (-1/-2/-3) | ESA Copernicus | Copernicus Open Access Hub, https://scihub.copernicus.eu/ | implemented | @16eagle | included: `getSentinel*` |
| MODIS | NASA/USGS | DAAC API, https://modis.ornl.gov/data/modis_webservice.html | ongoing | @16eagle | wrapper to `MODIS` |
| Landsat | NASA | ESPA API, https://landsat.usgs.gov/landsat-data-access | ongoing | | |
| Global Forest Change | Hansen et al. | http://azvoleff.com/articles/analyzing-forest-change-with-gfcanalysis | planned | | wrapper to `gfcanalysis` |
| CMIP5/PMIP3 Global Climate | ecoClimate | http://ecoclimate.org/about/ | planned | | wrapper to `ecoClimate` |
| Copernicus Global Land Products | ESA Copernicus | http://land.copernicus.eu/ | evaluated | | |
| CHELSA Global Land Climate | Karger et al. | http://chelsa-climate.org/ | evaluated | | |
| Global Forest Cover | EU-JRC | http://remote-sensing-biodiversity.org/forest-cover-and-forest-cover-pattern-data-by-jrc/ | evaluated | | |
| Global Surface Dynamics | EU-JRC | http://remote-sensing-biodiversity.org/global-water-dynamics-data/ | evaluated | | |
| Global Soil Grids | Hengl et al. | http://remote-sensing-biodiversity.org/global-soil-data-soilgrids/ | evaluated | | |
| Global Urban Footprint | Esch et al. | https://urban-tep.eo.esa.int/geobrowser/?id=portfolio#!&context=GUF%2FGUF2012-12m | evaluated | | |
| UK Urban Areas LiDAR | UK Environment Agency | http://remote-sensing-biodiversity.org/free-lidar-data-for-some-uk-cities/ | evaluated | | |
| Global Human Built-up And Settlement Extent (HBASE)| Wang et al. | http://sedac.ciesin.columbia.edu/data/set/ulandsat-hbase-v1 | evaluated | | |
| GIMMS NDVI3g | NASA | https://nex.nasa.gov/nex/projects/1349/ | evaluated | | wrapper to `GIMMS` |

