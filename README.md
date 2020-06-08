# getSpatialData <a href="http://jxsw.de/getSpatialData"><img align="right" src="https://raw.githubusercontent.com/16EAGLE/AUX_data/master/data/gSD_hex.png" /></a>

[![CRAN version](https://www.r-pkg.org/badges/version/getSpatialData)](https://CRAN.R-project.org/package=getSpatialData)
[![Build Status](https://travis-ci.org/16EAGLE/getSpatialData.svg?branch=master)](https://travis-ci.org/16EAGLE/getSpatialData) 
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/16EAGLE/getSpatialData?branch=master&svg=true)](https://ci.appveyor.com/project/16EAGLE/getSpatialData)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

## Introduction

`getSpatialData` is an R package in an early development stage that ultimately aims to enable homogeneous and reproducible workflows to query, preview, analyze, select, order and download various kinds of spatial datasets from open sources.

The package enables generic access to multiple data distributors with a common syntax for **159** products.

Among others `getSpatialData` supports these products: *Sentinel-1*, *Sentinel-2*, *Sentinel-3*, *Sentinel-5P*, *Landsat 8 OLI*, *Landsat ETM*, *Landsat TM*, *Landsat MSS*, *MODIS (Terra & Aqua)* and *SRTM DEMs*. For this, `getSpatialData` facilitates access to multiple services implementing clients to public APIs of *ESA Copernicus Open Access Hub*, *USGS EarthExplorer*, *USGS EROS ESPA*, *Amazon Web Services (AWS)*, *NASA DAAC LAADS* and *NASA CMR search*. A full list of all supported products can be found below.

`getSpatialData` offers to quickly overview the data catalogues for a custom place and time period.
For an efficient handling of available earth observation data, it specifically calculates the cloud coverage of records
in an area of interest based on light preview images. Furthermore, `getSpatialData` is able
to automatically select records based on cloud cover and temporal user requirements.

## Installation

To install the current beta version, use `devtools`:

```R
devtools::install_github("16EAGLE/getSpatialData")
```

## Workflow

The `getSpatialData` workflow is designed to be as reproducible as possible and is made of six steps: 

1. **querying** products of interest (see `get_products()`) for available records by an area of interest (AOI) and time (see `get_records()`),
2. **previewing** geometries and previews of the obtained records (see `get_previews()`, `view_records()` and `view_previews()`),
3. **analysing** records by deriving scene and AOI cloud distribution and coverage directly from preview imagery (see `calc_cloudcov()`),
4. **selecting** records based on user-defined measures (see `select_unitemporal()`, `select_bitemporal()` and `select_timeseries()`),
5. **ordering** datasets that are not available for immediate download (on-demand) but need to be ordered or restored before download (see `check_availability()` and `order_data()`), and lastly
6. **downloading** the full datasets for those records that have been selected in the process, solely based on meta and preview-dervied data.

For all steps, `getSpatialData` supports local chaching to reduce bandwith usage and uneccasary downloads.

This approach is implemented by the following functions (sorted by the order in which they would be typically used):

#### Defining session settings

* `set_aoi()`, `view_aoi()` and `get_aoi()` set, view and get a session-wide area of interest (AOI) that can be used by all `getSpatialData` functions.
* `set_archive()` and `get_archive()` set and get a session-wide archive directory that can be used by all `getSpatialData` functions.

#### Logging in

* `login_CopHub()` logs you in at the ESA Copernicus Open Access Hub using your credentials (register once at https://scihub.copernicus.eu/).
* `login_USGS()` logs you in at the USGS EROS Registration System (ERS) using your credentials (register once at https://ers.cr.usgs.gov/register/).
* `login_earthdata()` logs you in at the NASA Earth Data User Registration System (URS) using your credentials (register once at https://urs.earthdata.nasa.gov/users/new)
* `services()` displays the status of all online services used by `getSpatialData`. 

#### Retrieving and visualizing records

* `get_products()` obtains the names of all products available using `getSpatialData`. Currently, `getSpatialData` supports **159** products, including *Sentinel*, *Landsat*, *MODIS* and *SRTM* products.
* `get_records()` queries a service for available records using basic input search parameters such as product name, AOI and time range and returns an `sf data.frame` containing meta data and the geometries of each record.
* `view_records()` and `plot_records()` display the footprint geometries of each record on a map.

#### Analysing previews

* `get_previews()` downloads and georeferences preview images for visual inspection or automatic analysis and saves them to disk.
* `view_previews()` and `plot_previews()` load and display georeferenced previews acquired using `get_previews()`.
* `get_cloudcov_supported()` tells you for which products preview-based cloud coverages can be calculated using `calc_cloudcov()`.
* `calc_cloudcov()` calculates the AOI cloud cover and optionally saves raster cloud masks, all based on preview images.

#### Selecting records

Automatic remote sensing records selection is possible both for optical and SAR products.
`select_*` functionalities also support fusion of multiple optical products.
The selection is based on aoi cloud cover of optical records and temporal characterstics.
For optical records `select_*` uses preview cloud masks from `calc_cloudcov()` to create timestamp-wise mosaics.
It aims at cloud-free mosaics while ensuring user-defined temporal and product constraints.
* `get_select_supported()` tells you for which products automatic record selection is supported.
* `select_unitemporal()` selects remote sensing records *uni-temporally*
* `select_bitemporal()` selects remote sensing records *bi-temporally*
* `select_timeseries()` selects remote sensing records for a *time series*
* `is.*()`, such as `is.sentinel()`, `is.landsat()`, `is.modis()` and more to simplify filtering of records.

#### Checking, ordering and downloading records

* `check_availability()` checks for each record whether it is available for direct download (can be downloaded instantly) or not (and thus must be ordered before download).
* `order_data()` oders datasets that are not available for immediate download (on-demand) but need to be ordered or restored before download.
* `get_data()` downloads the full datasets per records.

#### Writing and reading

* `get_records_drivers()` provides the driver names that can be used in `write_records()`.
* `write_records()` writes records, e.g. as `GeoJSON`, for later use.
* `read_records()` reads records that have been written through `write_records()`.
* `read_previews()` reads georeferences preview images downloaded using `get_previews()`.

## Get started

The following example demonstrates a workflow for querying, previewing, analysing, selecting, ordering and downloading optical data from multiple sources at once (in this case Sentinel-2 L2A and Landsat 8 OLI Surface Reflectances).

``` r
library(getSpatialData)
data("aoi_data")

# Define an archive directory:
set_archive("/path/to/your/archive/directory/")

# Define an area of interest (AOI):
# Use the example AOI or draw an AOI by calling set_aoi():
set_aoi(aoi_data[[1]])
# View the AOI:
view_aoi()
```

![gsd_view_aoi](https://user-images.githubusercontent.com/23257860/84061935-11297a00-a9bf-11ea-9626-a69c6bde6061.png)


```R
# There are three services to login at:
login_CopHub(username = "yourusername")
#> Login successfull. ESA Copernicus credentials have been saved for the current session.
login_USGS(username = "yourusername")
#> Login successfull. USGS ERS credentials have been saved for the current session.
login_earthdata(username = "yourusername")
#> Login successfull. NASA URS EarthData credentials have been saved for the current session.

# Print the status of all services:
services()
#> ● ESA Copernicus Open Hub:  'available'     'Connection successfully established.'
#> ● ESA Copernicus S5P Hub:   'available'     'Connection successfully established.'
#> ● ESA Copernicus GNSS Hub:  'available'     'Connection successfully established.'
#> ● USGS-EROS ESPA:           'available'     'Connection successfully established.'
#> ● USGS EarthExplorer:       'available'     'Connection successfully established.'
#> ● AWS Landsat 8:            'available'     'Connection successfully established.'
#> ● NASA DAAC LAADS:          'available'     'Connection successfully established.'

# First, print all available products:
get_products()
#>   [1] "Sentinel-1"            "Sentinel-2"            "Sentinel-3"           
#>   [4] "Sentinel-5P"           "Sentinel-1_GNSS"       "Sentinel-2_GNSS"      
#>   [7] "Sentinel-3_GNSS"       "LANDSAT_ETM_C1"        "LANDSAT_MSS_C1"       
#>  [10] "LANDSAT_TM_C1"         "LANDSAT_8_C1"          "MODIS_MCD43D28_V6"    
#>  [13] "MODIS_MYD21A1N_V6"     "MODIS_MCD43A1_V6"      "MODIS_MCD43A2_V6"     
#>  [16] "MODIS_MCD43A3_V6"      "MODIS_MCD43A4_V6"      "MODIS_MCD43C1_V6"     
#>  [19] "MODIS_MCD43C2_V6"      "MODIS_MCD43C3_V6"      "MODIS_MCD43C4_V6"     
#>  [22] "MODIS_MCD43D01_V6"     "MODIS_MCD43D02_V6"     "MODIS_MCD43D03_V6"    
#>  [25] "MODIS_MCD43D04_V6"     "MODIS_MCD43D05_V6"     "MODIS_MCD43D06_V6"    
#>  [28] "MODIS_MCD43D07_V6"     "MODIS_MCD43D08_V6"     "MODIS_MCD43D09_V6"    
#>  [31] "MODIS_MCD43D10_V6"     "MODIS_MCD43D11_V6"     "MODIS_MCD43D12_V6"    
#>  [34] "MODIS_MCD43D13_V6"     "MODIS_MCD43D14_V6"     "MODIS_MCD43D15_V6"    
#>  [37] "MODIS_MCD43D16_V6"     "MODIS_MCD43D17_V6"     "MODIS_MCD43D18_V6"    
#>  [40] "MODIS_MCD43D19_V6"     "MODIS_MCD43D20_V6"     "MODIS_MCD43D21_V6"    
#>  [43] "MODIS_MCD43D22_V6"     "MODIS_MCD43D23_V6"     "MODIS_MCD43D24_V6"    
#>  [46] "MODIS_MCD43D25_V6"     "MODIS_MCD43D26_V6"     "MODIS_MCD43D27_V6"    
#>  [49] "MODIS_MCD43D29_V6"     "MODIS_MCD43D30_V6"     "MODIS_MCD43D31_V6"    
#>  [52] "MODIS_MCD43D32_V6"     "MODIS_MCD43D33_V6"     "MODIS_MCD43D34_V6"    
#>  [55] "MODIS_MCD43D35_V6"     "MODIS_MCD43D36_V6"     "MODIS_MCD43D37_V6"    
#>  [58] "MODIS_MCD43D38_V6"     "MODIS_MCD43D39_V6"     "MODIS_MCD43D40_V6"    
#>  [61] "MODIS_MCD43D41_V6"     "MODIS_MCD43D42_V6"     "MODIS_MCD43D43_V6"    
#>  [64] "MODIS_MCD43D44_V6"     "MODIS_MCD43D45_V6"     "MODIS_MCD43D46_V6"    
#>  [67] "MODIS_MCD43D47_V6"     "MODIS_MCD43D48_V6"     "MODIS_MCD43D49_V6"    
#>  [70] "MODIS_MCD43D50_V6"     "MODIS_MCD43D51_V6"     "MODIS_MCD43D52_V6"    
#>  [73] "MODIS_MCD43D53_V6"     "MODIS_MCD43D54_V6"     "MODIS_MCD43D55_V6"    
#>  [76] "MODIS_MCD43D56_V6"     "MODIS_MCD43D57_V6"     "MODIS_MCD43D58_V6"    
#>  [79] "MODIS_MCD43D59_V6"     "MODIS_MCD43D60_V6"     "MODIS_MCD43D61_V6"    
#>  [82] "MODIS_MCD43D62_V6"     "MODIS_MCD43D63_V6"     "MODIS_MCD43D64_V6"    
#>  [85] "MODIS_MCD43D65_V6"     "MODIS_MCD43D66_V6"     "MODIS_MCD43D67_V6"    
#>  [88] "MODIS_MCD43D68_V6"     "MODIS_MYD09A1_V6"      "MODIS_MYD09CMG_V6"    
#>  [91] "MODIS_MYD09GA_V6"      "MODIS_MYD09GQ_V6"      "MODIS_MYD09Q1_V6"     
#>  [94] "MODIS_MYD13A1_V6"      "MODIS_MYD13A2_V6"      "MODIS_MYD13A3_V6"     
#>  [97] "MODIS_MYD13C1_V6"      "MODIS_MYD13C2_V6"      "MODIS_MYD13Q1_V6"     
#> [100] "MODIS_MYD14_V6"        "MODIS_MYD14A1_V6"      "MODIS_MYD14A2_V6"     
#> [103] "MODIS_MYD15A2H_V6"     "MODIS_MYD17A2H_V6"     "MODIS_MYD21A2_V6"     
#> [106] "MODIS_MYD21_V6"        "MODIS_MYD21A1D_V6"     "MODIS_MYD11A1_V6"     
#> [109] "MODIS_MYD11A2_V6"      "MODIS_MYD11B1_V6"      "MODIS_MYD11C1_V6"     
#> [112] "MODIS_MYD11C2_V6"      "MODIS_MYD11C3_V6"      "MODIS_MYD11_L2_V6"    
#> [115] "MODIS_MYD16A2_V6"      "MODIS_MCD15A2H_V6"     "MODIS_MCD15A3H_V6"    
#> [118] "MODIS_MOD11A1_V6"      "MODIS_MOD09A1_V6"      "MODIS_MOD09CMG_V6"    
#> [121] "MODIS_MOD09GA_V6"      "MODIS_MOD09GQ_V6"      "MODIS_MOD09Q1_V6"     
#> [124] "MODIS_MOD11A2_V6"      "MODIS_MOD11B1_V6"      "MODIS_MOD11B2_V6"     
#> [127] "MODIS_MOD11B3_V6"      "MODIS_MOD11C1_V6"      "MODIS_MOD11C2_V6"     
#> [130] "MODIS_MOD11C3_V6"      "MODIS_MOD11_L2_V6"     "MODIS_MOD13A1_V6"     
#> [133] "MODIS_MOD13A2_V6"      "MODIS_MOD13A3_V6"      "MODIS_MOD13C1_V6"     
#> [136] "MODIS_MOD13C2_V6"      "MODIS_MOD13Q1_V6"      "MODIS_MOD14_V6"       
#> [139] "MODIS_MOD14A1_V6"      "MODIS_MOD14A2_V6"      "MODIS_MOD15A2H_V6"    
#> [142] "MODIS_MOD17A2H_V6"     "MODIS_MYD11B2_V6"      "MODIS_MYD11B3_V6"     
#> [145] "MODIS_MOD44W_V6"       "MODIS_MOD16A2_V6"      "MODIS_MOD44B_V6"      
#> [148] "MODIS_MCD12C1_V6"      "MODIS_MCD12Q1_V6"      "MODIS_MCD19A3_V6"     
#> [151] "MODIS_MCD19A2_V6"      "MODIS_MCD19A1_V6"      "MODIS_MCD64A1_V6"     
#> [154] "MODIS_MODOCGA_V6"      "MODIS_MODTBGA_V6"      "MODIS_MYDOCGA_V6"     
#> [157] "MODIS_MYDTBGA_V6"      "EMODIS_GLOBAL_LST_V6"  "EMODIS_NDVI_V6"       
#> [160] "EMODIS_PHEN_METRICS"   "SRTM_global_3arc_V003" "SRTM_global_1arc_V001"

# Query all available records for multiple products and a given time range at once,
# for example for Sentinel-2 and Landsat 8:
records <- get_records(time_range = c("2020-05-15", "2020-05-30"),
                       products = c("Sentinel-2", "LANDSAT_8_C1"))
#> Searching records for product name 'Sentinel-2'...
#> Searching records for product name 'LANDSAT_8_C1'...
#> Reading meta data of search results from USGS EarthExplorer...
#> Recieving available product levels from USGS-EROS ESPA...

# Have a look at the returned records table:
View(records)
```

![gSD_records](https://user-images.githubusercontent.com/23257860/84061417-2a7df680-a9be-11ea-982b-8d954f85370c.png)

```R
# Filter records, e.g. to contain only Level 2A/surface reflectance records:
records <- records[records$level == "Level-2A" | records$level == "sr",]

# Display the records footprints interactively:
view_records(records)
#> Composing records map...
```

![gsd_view_records](https://user-images.githubusercontent.com/23257860/84061407-27830600-a9be-11ea-8249-7914d77ded0e.png)

```R
# ...or plot them:
plot_records(records)
#> Composing records plot...
```

![gsd_plot_records](https://user-images.githubusercontent.com/23257860/84061404-24881580-a9be-11ea-9d43-a60e9ad93f58.png)


```R
# Download and georeference the previews for all records:
records <- get_previews(records) 

# Display the previews interactively (all or just a selection):
view_previews(records[21:24,])
#> Composing preview map...
```

![gsd_view_previews](https://user-images.githubusercontent.com/23257860/84061411-294cc980-a9be-11ea-9457-60461469e70b.png)

```R
# ...or plot them:
plot_previews(records[21:24,])
#> Composing preview plot...
```

![gsd_plot_previews](https://user-images.githubusercontent.com/23257860/84061405-2651d900-a9be-11ea-8038-075efe9a3f36.png)

```R
# Use the previews to calculate the cloud coverage in your AOI for all records:
records <- calc_cloudcov(records) 


# With the result, getSpatiaData can automatically select the most usable records,
# for a single timestamp:
records <- select_unitemporal(records)
# for a two timestamps:
records <- select_bitemporal(records)
# or for a series of timestamps:
records <- select_timeseries(records)


# Once, you came to a selection (manually or automatically), check for availability:
records <- check_availability(records)
#> Checking instant availability for Sentinel records...
#> Checking availability for Landsat records...
#> Investigating matching ESPA orders in the past...
#> --> Found matching ESPA orders available for download.
#> 23/30 records are currently available for download (this includes past completed orders that are still available for download).
#> Columns added to records: 'download_available', 'order_id', 'ordered'

# Data sets that are not instantly available for download, e.g. because the have been
# archived, can be ordered:
records <- order_data(records)
#> Warning: Please note: The Copernicus LTA quota currently permits users to request a maximum of one LTA dataset per 30 minutes!
#> Assembling dataset URLs...
#> Attempting orders...
#> [Dataset 1/7] Requesting order of 'LC08_L1TP_194025_20200516_20200527_01_T1' at ESPA...
#> Ordering requested items from ESPA...
#> Collecting from 1 collection(s) [olitirs8_collection], resulting in 1 order(s)...
#> Products 'LC08_L1TP_194025_20200516_20200527_01_T1' have been ordered successfully:
#> [level = 'toa', format = 'gtiff', order ID(s) 'espa-xxx@xxx.de-06082020-113626-xxx'].
#> ...

# Finally, download records available for download:
records <- get_data(records)
```

## Supported products

*Table to be added*

## Contribution

We are happy about any kind of contribution, from feature ideas, ideas on possible data sources, technical ideas or other to bug fixes, code suggestions or larger code contributions! Open an issue to start a discussion: <https://github.com/16eagle/getSpatialData/issues> 

## Mentioned

`getSpatialData` has been mentioned here:

Kwok, R., 2018. Ecology’s remote-sensing revolution. Nature 556, 137. https://doi.org/10.1038/d41586-018-03924-9



