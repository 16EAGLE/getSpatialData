# getSpatialData

## Introduction

getSpatialData is an R package in an early development stage that ultimatively aims to provide function bundles to query, download, prepare and transform various kinds of spatial datasetes from open sources, e.g. Satellite sensor data, environmental data products etc. The current version is a pre-beta version, meant to be used for some early functionality tests. The included functions and their concepts are explorative and could be removed or changed fundamentaly and do not necessarily represent the earliest state of development.

## Current implementations

At the moment, a getSentinel function bundle is developed, which should enable the user to easily query, download and transform Sentinel-1, -2 and -3 data directly within R. At this stage of development, the python library `sentinelsat` is used to connect to the Coperinucs Open Access Hub(s). Currently, the development of aa python-independent, simple query assembler to use the API directly is evaluated.

The following functions are publicly available and have been tested on Linux (Ubunut 16.04) and Windows 10. On both, Python 3.6.* callable from the command line and the python library `sentinelsat` were pre-installed, before installing `getSpatialData`.

* `getSentinel_query()` – querys the Copernicus Open Access Hubs for Sentinel-1, -2 and -3 data and returns a data frame containing the found datasetes (rows) and their attributes (columns).
* `getSentinel_preview()` – uses the output of `getSentinel_query()` to preview an user-selected, individual dataset within in an R plotting device without the necessity to download the dataset first.
* `getSentinel_data()` – uses the output of `getSentinel_query()` to download the specified datasets to a local directory as .zip files. Functions to deal with the files form within R wihtout expert knowledge will follow.

## Installation

An operational use of this pre-beta version of getSpatialData is not recommended and not possible. Functions could be removed or fundamentaly changed. Documentation could be wrong or incomplete.

To install the current pre-beta version for playing around with the concept, use `devtools`.

```s
devtools::install_github("16EAGLE/getSpatialData")
```

Currently, a Python installation (Python interpreter for Python 2.7.* or 3.*) is necessary to use the package.

unction manuals. Further examples and explanations are provided within the function manuals.

## Ideas

Ideas on possible data sources to be included, technical ideas or other are welcome! Open an issue to start a discussion: <https://github.com/16eagle/getSpatialData/issues> 
