# getSpatialData

## Introduction

getSpatialData is an R package in an early development stage that ultimatively aims to provide homgenious function bundles to query, download, prepare and transform various kinds of spatial datasetes from open sources, e.g. Satellite sensor data, environmental data products etc. The current version is a pre-beta version, meant to be used for some early functionality tests. The included functions and their concepts are explorative and could be removed or changed fundamentaly and do not necessarily represent the latest state of development.

## State of development

At the moment, a getSentinel function bundle is developed, which should enable the user to easily query, download and transform Sentinel-1, -2 and -3 data directly within R. At this stage of development, the python library `sentinelsat` is used to connect to the Coperinucs Open Access Hub(s). Currently, a python-independent, simple query assembler to use the API directly is developed, making only limited use of query-side filtering.

### Available functions

The following functions are publicly available and have been tested on Linux (Ubunut 16.04) and Windows 10. On both, Python 3.6.* callable from the command line and the python library `sentinelsat` were pre-installed, before installing `getSpatialData`.

#### main functions

* `getSentinel_query()` – querys the Copernicus Open Access Hubs for Sentinel-1, -2 and -3 data and returns a data frame containing the found datasetes (rows) and their attributes (columns).
* `getSentinel_preview()` – uses the output of `getSentinel_query()` to preview an user-selected, individual dataset within in an R plotting device without the necessity to download the dataset first.
* `getSentinel_data()` – uses the output of `getSentinel_query()` to download the specified datasets to a local directory as .zip files. Functions to deal with the files form within R wihtout expert knowledge will follow.


#### helper functions

* `set_python` – manually define the python installation that sould be used
* `set_cophub_login` – define your Copernicus Open Access login credentials once for the present R session to be able to call each `getSentinel*` function without defining login arguments


### Manuals

For all current functions publicly available, documentation is available, containing information on the expected arguments, the return and examples. The files can be accesed executing a command like `?getSentinel_query`.

### Known bugs

At the moment, the `getSentinel*` function bundle seems to fail using an Anaconda Python installation.

## Installation

An operational use of this pre-beta version of getSpatialData is not recommended and not possible. Functions could be removed or fundamentaly changed. Documentation could be wrong or incomplete.

To install the current pre-beta version for playing around with the concept, use `devtools`.

```s
devtools::install_github("16EAGLE/getSpatialData")
```

Currently, a Python installation (Python interpreter for Python 2.7.* or 3.*) is necessary to use the package. Anaconda is not supported at the moment.


## Example

The following code represents a working chain for querrying, filtering, previewing and downloading Sentinel-2 data wihtin R. This can be also done for Sentinel-1 or -3.

```
## Requirements: A Python installation (not Anaconda). If not recognized properly, use set_python().
## First run of a getSentinel* functions will take longer, since python dependencies will be installed


## Load packages
library(getSpatialData)
library(raster)


## Define an extent, a time range and a platform
ext <- extent(10.29048, 11.75558, 45.93350, 46.94617)
time_range <-  c("20170801", "20170830")
platform <- "Sentinel-2"


## Prior to calling getSentinel* functions,
## define your Copernicus Open Access Hub credentials :
set_cophub_login(hub_user = "16eagle") #asks for your password, if argument 'hub_pass' is not defined


## Use getSentinel_query to search for data
products <- getSentinel_query(ext = ext, time_range = time_range, platform = platform)


## Get an overview of the products
colnames(products) #see all available filter attributes
unique(products$processinglevel) #use one of the, e.g. to see available processing levels

View(products) #get an overview about the search products. You can navigate
#through the available datasets and their attributes (in RStudio, this looks like this):
```

<p align="center"><img width="80%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/view.png"></p>
<p align="center"><sub>Figure 1: Screenshot of the RStudio View() browser, displaying the products data.frame returned by getSentinel_query</sub></p>
<br>


```
## Filter the products
products_filtered <- products[which(products$processinglevel == "Level-1C"),] #filter by Level
## add more lines, if you want to further filter your filtered products


## Preview a single product in your plot window, e.g. to see cloud coverage
getSentinel_preview(product = products_filtered[10,])
# This will plot a preview to the active plotting device:
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/preview.png"></p>
<p align="center"><sub>Figure 2: Screenshot of the RStudio plotting window, displaying a product preview returned by getSentinel_preview</sub></p>
<br>

```
## Download datasets
#dir_out <- "your/output/directory"
dir_out <- tempdir() #for this example, we use a temporary directory
files <- getSentinel_data(products = products_filtered, dir_out = dir_out)

```

## Ideas

Ideas on possible data sources to be included, technical ideas or other are welcome! Open an issue to start a discussion: <https://github.com/16eagle/getSpatialData/issues> 

### Datasets

The following data sources are being evaluated to be implemented within the package. This also includes sources which can be already accessed through existing packages that could be wrapped behind an standardized R function interface. Please feel free to contribute to the list, e. g. through a pull request:

| Product(s) | Source | API/URL | Status | Contributer | Remark | 
| ---------- | --------------- | --- | -------| ----------- | ------ |
| Sentinel (-1/-2/-3) | Copernicus | ESA Copernicus Open Access Hub, https://scihub.copernicus.eu/ | ongoing | @16eagle | see beta: `getSentinel*` |
| MODIS | NASA/USGS | DAAC API, https://modis.ornl.gov/data/modis_webservice.html | ongoing | @16eagle | wrapper |
| Landsat | NASA | ESPA API, https://landsat.usgs.gov/landsat-data-access | planned | | |
| Global Forest Change | Hansen et al. | http://azvoleff.com/articles/analyzing-forest-change-with-gfcanalysis | planned | | wrapper to gfcanalysis |
| Global Forest Cover | EU-JRC | http://remote-sensing-biodiversity.org/forest-cover-and-forest-cover-pattern-data-by-jrc/ | evaluated | | |
| Global Surface Dynamics | EU-JRC | http://remote-sensing-biodiversity.org/global-water-dynamics-data/ | evaluated | | |
| Global Soil Grids | Hengl et al. | http://remote-sensing-biodiversity.org/global-soil-data-soilgrids/ | evaluated | | |
| Global Urban Footprint | Esch et al. | https://urban-tep.eo.esa.int/geobrowser/?id=portfolio#!&context=GUF%2FGUF2012-12m | evaluated | | |
| UK Urban Areas LiDAR | UK Environment Agency | http://remote-sensing-biodiversity.org/free-lidar-data-for-some-uk-cities/ | evaluated | | |
| CMIP5/PMIP3 Global Climate | ecoClimate | http://ecoclimate.org/about/ | planned | | wrapper to ecoClimate |
| Global Human Built-up And Settlement Extent (HBASE)| Wang et al. | http://sedac.ciesin.columbia.edu/data/set/ulandsat-hbase-v1 | evaluated | | |

