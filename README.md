# getSpatialData

## Introduction

`getSpatialData` is an R package in an early development stage that ultimately aims to provide homogeneous function bundles to query, download, prepare and transform various kinds of spatial datasets from open sources, e.g. Satellite sensor data, higher-level environmental data products etc. The current version is a beta version, meant to be used for functionality tests. The included functions and their concepts are exploratory and could be removed or changed.

`getSpatialData` supports both `sf` and `sp` classes as AOI inputs.

## State of development

Currently, `getSpatialData` can be used to download Sentinel and Landsat data. An R-native getSentinel function bundle allows the user to easily query, preview and download Sentinel-1, -2 and -3 data directly from R. The client is coded in R and works independently from external libraries. It is currently tested for minor bugs (please report bugs if you find some). An R-native getLandsat function bundle connecting to the USGS Earth Explorer and the USGS ESPA APIs can be used to query, preview and on-demand download Landsat data of different product levels.

A universal AOI defintion function has been implemented (see `set_aoi`). It understands different spatial objects representing the user's AOI and translates them depending on the client function that the user wants to use. It supports multi-point polygon shape objects (sp and sf) or matrix objects. Alternatively, it lets the user draw an AOI via `mapedit`.

Currently, a MODIS get function bundle is being developed. 

### Available functions

The following functions are publicly available and have been tested on Linux (Ubuntu 16.04 LTS, 17.10) and Windows 10:

#### main functions

* `getSentinel_query()` – querys the Copernicus Open Access Hubs for Sentinel-1, -2 and -3 data and returns a data frame containing the found datasets (rows) and their attributes (columns).
* `getSentinel_preview()` – uses the output of `getSentinel_query()` to preview an user-selected, individual dataset without the necessity to download the dataset first. By default, the preview is displayed corner-georeferenced in a map viewer in relation to the session AOI. Alternatively, an RGB plot can be displayed.
* `getSentinel_data()` – uses the output of `getSentinel_query()` to download the specified datasets to a local directory as .zip files. A transform function bundle helping to deal with the downloaded files within R without expert knowledge will follow soon.


* `getLandsat_names()` – obtains available Landsat dataset names from the USGS Earth Explorer, which can be used with getLandsat_query().
* `getLandsat_query()` – querys the USGS Earth Explorer for Landsat products.
* `getLandsat_preview()` – previews the query results obtained with getLandsat_query().
* `getLandsat_data()` – uses the output of getLandsat_query() to order and download Landsat data products from USGS ESPA, including top-of-atmosphere or surface reflectance products as well as different indices, processed on-demand.


#### helper functions

* `login_CopHub` – define your Copernicus Open Access login credentials once for the present R session to be able to call each `getSentinel*` function without defining login arguments each time you use them.
* `login_USGS` – define your USGS login credentials once for the present R session to be able to call each `get*` function that connects to a USGS service without defining login arguments each time you use them.


* `set_archive` – define a `getSpatialData` archive directory to which all `*_data` functions will download data.
* `set_aoi` - draw or define an AOI as sf, sp or matrix object for the running session that can be used by all query functions.
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

```R
## Load packages
library(getSpatialData)
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
platform <- "Sentinel-2"

## set login credentials and archive directory
login_CopHub(username = "username") #asks for password or define 'password'
set_archive("/path/to/archive/")

## Use getSentinel_query to search for data (using the session AOI)
products <- getSentinel_query(time_range = time_range, platform = platform)

## Filter the products
colnames(products) #see all available filter attributes
unique(products$processinglevel) #use one of the, e.g. to see available processing levels

products_filtered <- products[which(products$processinglevel == "Level-1C"),] #filter by Level
products_filtered <- products_filtered[products_filtered$cloudcoverpercentage <= 30, ] #filter by clouds

## View products table
View(products)
View(products_filtered)
#browser products or your filtered products
```

<p align="center"><img width="100%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_query_table.png"></p>
<p align="center"><sub>Figure 3: Screenshot of the View() display in RStudio, displaying a filtered products table produced by getSentinel_query()</sub></p>
<br>


```R
## Preview a single product on a mapview map with session AOI
getSentinel_preview(product = products_filtered[9,])
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_preview.png"></p>
<p align="center"><sub>Figure 4: Screenshot of the RStudio viewer, displaying a corner-georeferenced Sentinel-2 preview and the session AOI using getSentinel_preview()</sub></p>
<br>


```R
## Preview a single product on a mapview map without session AOI
getSentinel_preview(product = products_filtered[9,], show_aoi = FALSE)
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_preview_no_aoi.png"></p>
<p align="center"><sub>Figure 5: Screenshot of the RStudio viewer, displaying a corner-georeferenced Sentinel-2 preview using getSentinel_preview()</sub></p>
<br>


```R
## Preview a single product as RGB plot
getSentinel_preview(product = products_filtered[9,], on_map = FALSE)
```

<p align="center"><img width="60%" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_preview_plotRGB.png"></p>
<p align="center"><sub>Figure 6: Screenshot of the RStudio viewer, displaying a simple Sentinel-2 RGB plot preview using getSentinel_preview()</sub></p>
<br>


```R
## Finally, download some datasets to your archive directory
files <- getSentinel_data(products = products_filtered[c(4,7,9), ])

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

