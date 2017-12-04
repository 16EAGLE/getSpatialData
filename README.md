# getSpatialData

## Introduction

getSpatialData is an R package in an early development stage that ultimatively aims to provide function bundles to query, download, prepare and transform various kinds of spatial datasetes from open sources, e.g. Satellite sensor data, environmental data products etc. The current version is a pre-beta version, meant to be used for some early functionality tests. The included functions and their concepts are explorative and could be removed or changed fundamentaly and do not necessarily represent the earliest state of development.

## Current implementations

At the moment, a getSentinel function bundle is developed, which should enable the user to easily query, download and transform Sentinel-1, -2 and -3 data directly within R. At this stage of development, the python library `sentinelsat` is used to connect to the Coperinucs Open Access Hub(s). Currently, a python-independent, simple query assembling implementation to use the API directly is evaluated.

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
