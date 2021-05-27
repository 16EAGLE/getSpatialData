## getSpatialData 0.1.1 (ongoing development)
Bug fixes, Landsat Collection 2 support

#### Bug fixes:

* affects `login_USGS()`, `get_products()`, `get_records()`, `get_previews()`, `get_data()`: Fixed a bug that caused all request to the EROS Inventory Service API to fail, including login. The client has been updated to support the new EROS API 1.5.0 to query Earth Explorer.

#### Products:

* Landsat Collection 2 products are now supported, call `get_products()` for a list.

#### Changes:

* All product names and product group names, as returned by `get_products()` and expected as inputs to several functions, are now case-**insensitive**. It should not matter whether you query `"SENTINEL-2"` or `"sentinel-2"`. Since `get_products()` now returns only lower-case outputs, this change can be breaking for code that searches patterns in these outputs. Legacy inputs should still work.


## getSpatialData 0.1.0
An all-new interface, largely compatible to older versions, and new core functionalites shaping the package's capabilties.

This major update introduces a new function interface that allows to query, preview, analyse, select, order and download records of multiple products from different sources at once. Records are represented as spatial `sf data.frames`. A new collection of functions is introduced that calculate AOI cloud cover from preview imagery and facilitate the automatic, reproducible selection of best-quality records in advance of downloading the datasets. Some functions are deprecated, but mostly, the new interface should be compatible with older versions. Please note that records column names are renamed by default, which will break older codes relying on column names. This can be turned off, if needed, which however is not recommended since multi-source queries are then impossible (see argument `rename_cols` of `get_records`).

Please report issues with this version so that they can be rapidly resolved.

#### New functions:

**Session:**

* `login_earthdata()` logs you in at the NASA Earth Data User Registration System (URS) using your credentials (register once at https://urs.earthdata.nasa.gov/users/new)
* `services()` displays the status of all online services used by `getSpatialData`. 
* `set_archive()` and `get_archive()` set and get a session-wide archive directory that can be used by all `getSpatialData` functions.


**Retrieving products and records:**
* `get_products()` obtains the names of all products available using `getSpatialData`. Currently, `getSpatialData` supports **159** products, including *Sentinel*, *Landsat*, *MODIS* and *SRTM* products.
* `get_records()` queries a service for available records using basic input search parameters such as product name, AOI and time range and returns an `sf data.frame` containing meta data and the geometries of each record.
* `view_records()` and `plot_records()` display the footprint geometries of each record on a map.

**Analysing previews:**

* `get_previews()` downloads and georeferences preview images for visual inspection or automatic analysis and saves them to disk.
* `view_previews()` and `plot_previews()` load and display georeferenced previews acquired using `get_previews()`.
* `get_cloudcov_supported()` tells you for which products preview-based cloud coverages can be calculated using `calc_cloudcov()`.
* `calc_cloudcov()` calculates the AOI cloud cover and optionally saves raster cloud masks, all based on preview images.

**Selecting records:**

* `get_select_supported()` tells you for which products automatic record selection is supported.
* `select_unitemporal()` selects remote sensing records (both optical and SAR across different products) *uni-temporally* according to AOI cloud cover (in case of optical data) and temporal characteristics.
* `select_bitemporal()` selects remote sensing records (both optical and SAR across different products) *bi-temporally* according to AOI cloud cover (in case of optical data) and temporal characteristics.
* `select_timeseries()` selects remote sensing records (both optical and SAR across different products) for a *time series* according to AOI cloud cover (in case of optical data) and temporal characteristics.
* `is.*()`, such as `is.sentinel()`, `is.landsat()`, `is.modis()` and more to simplify filtering of records.

**Checking, ordering and downloading records:**

* `check_availability()` checks for each record whether it is available for direct download (can be downloaded instantly) or not (and thus must be ordered before download).
* `order_data()` oders datasets that are not available for immediate download (on-demand) but need to be ordered or restored before download.
* `get_data()` downloads the full datasets per records.

**Writing and reading**

* `get_records_drivers()` provides the driver names that can be used in `write_records()`.
* `write_records()` writes records, e.g. as `GeoJSON`, for later use.
* `read_records()` reads records that have been written through `write_records()`.
* `read_previews()` reads georeferences preview images downloaded using `get_previews()`.

#### Deprecated and replaced functions: ####

* `getSentinel_names()`, `getLandsat_names()`, `getMODIS_names()`: These functions still work, but are deprecated. Please switch to using the generic replacement `get_products()` and its platform-specific aliases.
* `getSentinel_query()`, `getLandsat_query()`, `getMODIS_query()`: These functions still work, but are deprecated. Please switch to using the generic replacement `get_records()` and its platform-specific aliases.
* `getSentinel_restore()`: This function is deprecated. Please switch to using the generic replacement `order_data()`.
* `services_avail()`: This function is deprecated. Please switch to using `services()`.
* `getSentinel_preview()`, `getLandsat_preview()`, `getMODIS_preview()`: These functions still work, but are deprecated. Please switch to using the generic replacements `get_previews()` for downloading previews and `view_previews()` for displaying previews.

## getSpatialData 0.0.4
Introduction of functions for data preparation, feature enhancements, bug fixes


#### New functions:

* `services_avail()`: checks and returns the status of all online services used by getSpatialData. Can be used to check if a service is undergoing maintenance or is unavailable due to unknown reasons.
* `getSentinel_restore()`: restore Sentinel datasets that have been archived to Copernocus LTA
* `prepSentinel()`: automatically prepare Sentinel data to be ready-to-use
* `cropFAST()`: fastly crop large-scale datasets to a spatial extent


#### New features:

* `getSentinel_query()`: argument "check_avail" added to check on-demand availability of Sentinel datasets or if they had been moved to the Copernicus Long-Term Archive (LTA). Deactivated by default since check increases query request time.
* `getSentinel_data()`: checks for on-demand availability of requested datasets first before attempting download.
* `getSentinel_query()` and `getSentinel_data()`: added Sentinel-5 precursor and Sentinel GNSS on-board GPS data support
* `set_archive()`: added argument `create` to control whether archive directory should be created or not.
* `getLandsat_query()`: added quick filters for filtering records by level or cloud cover (contribution by @SteveMHill)
* `getLandsat_data()`: ESPA order placement of records with diverging products level in one single call now possible (contribution by  @SteveMHill)
* `_query()` functions: Fields known to be of type numeric (e.g. cloud cover percentages) are now converted into numeric
* `_data()` functions: added number of downloaded item vs total number of downloads to console output


#### Major bug fixes:

* `getMODIS_data()` now native, deprecated FTP LAADS DAAC services replaced by https requests
* `getLandsat_data()`: bug that caused the function to download only the first product of an order when using order IDs with espa_order as argument
* `getSentinel_query()`: AOIs are not directly used for the query anymore, instead a bounding box is used to prevent exceeding the maximum of 200 polygon points that DHUS allows (issue #6)
* `getLandsat_query()`: bug in translating the AOI geometry into query, causing eastwards shifted query results (issue #17)
* all `_data` functions and `set_archive()` now expand paths shortend with swung dashed to avoid problems with tools::md5sum
* `getLandsat_data()`: bug causing false file name subsetting (issue 19)
* `getLandsat_data()`: ESPA orders are not double-placed anymore (contribution by @SteveMHill) (issue #20)
* `getMODIS_data()`: bug preventing successfull downloads of some MODIS products due to outdated EE URLs which are now checked and, if necessary, replaced with recent ones (issue #22)
* `getMODIS_preview()`: bug causing previews to be stretched to the wrong extents (issue #23)
* `getSentinel_preview()`: bug causing previews to be stretched to the wrong extents
* `getSentinel_data()`: bug causing Sentinel downloads to fail due to the selection of the wrong LTA availability column (issue #27)


#### Contributions:

* Code contributions by Steven Hill (@SteveMHill)
* Bug indication by @bleutner, @vanto1994, @mtreg, @MatthiasSiewert, @RemoteSensingR
* Feature ideas by @kadyb, @ptaconet


***

## getSpatialData 0.0.3
getLandsat* and getMODIS* function bundles for USGS EE client


#### New functions:

* `getLandsat_names()`: obtain Landsat product names
* `getLandsat_query()`: query USGS Earth Explorer for Landsat data
* `getLandsat_preview()`: preview (quick look) a queried Landsat record
* `getLandsat_data()`: download Landsat data on-demand from USGS ESPA
* `getMODIS_names()`: obtain MODIS product names
* `getMODIS_query()`: query USGS Earth Explorer for MODIS data
* `getMODIS_preview()`:  preview (quick look) a queried MODIS record
* `getMODIS_data()`: download MODIS data from LAADS
* `login_USGS()`: session-wide login to USGS services


#### New features:

* `getLandsat_query()` and `getMODIS_query()`: argument "name" is now optional and set to "all" by default. In this case, all available Landsat datasets are searched and added to the output. The originating dataset name is always indicated by the column "dataset_name".


#### Bug fixes:

* RGB clouring of preview functions is now correct
* `getLandsat_query()` and `getMODIS_query()` output now more than 10 products per query (maximum is 50 000)
* `getSentinel_query()` paging error (always maxiumu of 200 records) solved


#### Minor Changes:

* clearer login functions naming (removed 'set_' prefix)
* all request are now internally channeld through gSD.get/gSD.post
* clearer semantics, changed "products" argument to "records"


#### Contributions:

* Bug fix by Francesco Pirotti (@fpirotti, #2)


***

## getSpatialData 0.0.2
initial session-wide tool functions


#### New functions:

* `set_archive()`: defines getSpatialData archive folder
* `set_aoi()`: defines session AOI from sp, sf or matrix object or mapedit GUI input
* `view_aoi()`: displays AOI in a mapview viewer


#### New features:

* `getSentinel_preview()` displays previews as corner-referenced RGB images on a mapview map in relation to session AOI by default. With show_aoi = F the session AOI is not displayed. With on_map = F a simple RGB plot is displayed instead.


#### Contributions:

* Feature idea by Mike Treglia (@mtreg, #1)


***

## getSpatialData 0.0.1
getSentinel* function bundle for ESA Copernicus R client


#### New functions:

* `getSentinel_query()`: query function for Sentinel-1, -2, -3
* `getSentinel_preview()`: preview (quick look) function for Sentinel-1, -2, -3
* `getSentinel_data()`: download function for Sentinel-1, -2, -3
* `login_CopHub()`: session login for getSentinel* functions


#### Removed features:

* early-dev. python bindings, getSentinel* is fully R-native now


***
This document should provide a broad overview on changes that are applied to the getSpatialData R package. There is no warranty for completeness, since minor changes might not be included. All improvement and feature descriptions are bundled per release version. The document is currently maintained by Jakob Schwalb-Willmann.
