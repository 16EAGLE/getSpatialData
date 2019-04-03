## getSpatialData 0.0.4 (under development)
Introduction of functions for data preparation, feature enhancements, bug fixes

<br>
**New functions:**

* `services_avail()`: checks and returns the status of all online services used by getSpatialData. Can be used to check if a service is undergoing maintenance or is unavailable due to unknown reasons.
* `getSentinel_restore()`: restore Sentinel datasets that have been archived to Copernocus LTA
* `prepSentinel()`: automatically prepare Sentinel data to be ready-to-use
* `cropFAST()`: fastly crop large-scale datasets to a spatial extent

<br>
**New features:**

* `getSentinel_query()`: argument "check_avail" added to check on-demand availability of Sentinel datasets or if they had been moved to the Copernicus Long-Term Archive (LTA). Deactivated by default since check increases query request time.
* `getSentinel_data()`: checks for on-demand availability of requested datasets first before attempting download.
* `getSentinel_query()` and `getSentinel_data()`: added Sentinel-5 precursor and Sentinel GNSS on-board GPS data support
* `set_archive()`: added argument `create` to control whether archive directory should be created or not.
* `getLandsat_query()`: added quick filters for filtering records by level or cloud cover (contribution by @SteveMHill)
* `getLandsat_data()`: ESPA order placement of records with diverging products level in one single call now possible (contribution by  @SteveMHill)
* `_query()` functions: Fields known to be of type numeric (e.g. cloud cover percentages) are now converted into numeric
* `_data()` functions: added number of downloaded item vs total number of downloads to console output

<br>
**Major bug fixes:**

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


<br>
**Contributions:**

* Code contributions by Steven Hill (@SteveMHill)
* Bug indication by @bleutner, @vanto1994, @mtreg, @MatthiasSiewert, @RemoteSensingR
* Feature ideas by @kadyb, @ptaconet

<br>

***

## getSpatialData 0.0.3
getLandsat* and getMODIS* function bundles for USGS EE client

<br>
**New functions:**

* `getLandsat_names()`: obtain Landsat product names
* `getLandsat_query()`: query USGS Earth Explorer for Landsat data
* `getLandsat_preview()`: preview (quick look) a queried Landsat record
* `getLandsat_data()`: download Landsat data on-demand from USGS ESPA
* `getMODIS_names()`: obtain MODIS product names
* `getMODIS_query()`: query USGS Earth Explorer for MODIS data
* `getMODIS_preview()`:  preview (quick look) a queried MODIS record
* `getMODIS_data()`: download MODIS data from LAADS
* `login_USGS()`: session-wide login to USGS services

<br>
**New features:**

* `getLandsat_query()` and `getMODIS_query()`: argument "name" is now optional and set to "all" by default. In this case, all available Landsat datasets are searched and added to the output. The originating dataset name is always indicated by the column "dataset_name".

<br>
**Bug fixes:**

* RGB clouring of preview functions is now correct
* `getLandsat_query()` and `getMODIS_query()` output now more than 10 products per query (maximum is 50 000)
* `getSentinel_query()` paging error (always maxiumu of 200 records) solved

<br>
**Minor Changes:**

* clearer login functions naming (removed 'set_' prefix)
* all request are now internally channeld through gSD.get/gSD.post
* clearer semantics, changed "products" argument to "records"

<br>
**Contributions:**

* Bug fix by Francesco Pirotti (@fpirotti, #2)

<br>

***

## getSpatialData 0.0.2
initial session-wide tool functions

<br>
**New functions:**

* `set_archive()`: defines getSpatialData archive folder
* `set_aoi()`: defines session AOI from sp, sf or matrix object or mapedit GUI input
* `view_aoi()`: displays AOI in a mapview viewer

<br>
**New features:**

* `getSentinel_preview()` displays previews as corner-referenced RGB images on a mapview map in relation to session AOI by default. With show_aoi = F the session AOI is not displayed. With on_map = F a simple RGB plot is displayed instead.

<br>
**Contributions:**

* Feature idea by Mike Treglia (@mtreg, #1)

<br>

***

## getSpatialData 0.0.1
getSentinel* function bundle for ESA Copernicus R client

<br>
**New functions:**

* `getSentinel_query()`: query function for Sentinel-1, -2, -3
* `getSentinel_preview()`: preview (quick look) function for Sentinel-1, -2, -3
* `getSentinel_data()`: download function for Sentinel-1, -2, -3
* `login_CopHub()`: session login for getSentinel* functions

<br>
**Removed features:**

* early-dev. python bindings, getSentinel* is fully R-native now


<br>

***
This document should provide a broad overview on changes that are applied to the getSpatialData R package. There is no warranty for completeness, since minor changes might not be included. All improvement and feature descriptions are bundled per release version. The document is currently maintained by Jakob Schwalb-Willmann.
