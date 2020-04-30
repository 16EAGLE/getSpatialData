#' ---------------------------------------------------------------------
#' @name is_product_checker
#' @description These are check utils for product and product group and
#' work in same manner as is.na(). Apart from product and product group
#' some methods target more specific types that are identified from 
#' the record ids. All methods return logical vectors of the same length as NROW(records).
#' @keywords internal
#' @author Henrik Fisser, 2020
#' ---------------------------------------------------------------------

#' Returns TRUE for records that are of the specified product group
#' @description \code{is_product_group_} checks which records are of product group \code{product_group}
#' @param records sf data.frame
#' @param product_group character specifies the product group to be checked on.
#' @return logical vector
#' @export
is_product_group_ <- function(records, product_group) {
  records <- .check_records(records, col.names = c(name_product_group()))
  product_groups <- records[[name_product_group()]]
  return(which(product_groups == product_group))
}

#' Returns TRUE for records that are of the specified product
#' @description \code{is_product_} checks which records are of product \code{product}.
#' @details Check \link{get_names} for available product names.
#' @inheritParams is_product_group_
#' @param product character specifies the product to be checked on.
#' @inherit is_product_group_ return author
#' @export
is_product_ <- function(records, product) {
  records <- .check_records(records, col.names = c(name_product()))
  products <- records[[name_product()]]
  return(which(products == product))
}

#' Returns TRUE for records that are of product group 'Landsat'
#' @description \code{is_landsat} checks which records are Landsat records.
#' @inheritParams calc_cloudcov
#' @return logical vector, same length as number of rows in \code{records}.
#' @author Henrik Fisser, 2020
#' @export
is_landsat <- function(records) {
  return(is_product_group_(records, name_product_group_landsat()))
}

#' Returns TRUE for records that are of product group 'MODIS'
#' @description \code{is_modis} checks which records are MODIS records.
#' @inheritParams calc_cloudcov
#' @inherit is_landsat return author inheritParams
#' @export
is_modis <- function(records) {
  return(is_product_group_(records, name_product_group_modis()))
}

#' Returns TRUE for records that are of product group 'MODIS'

#' Returns TRUE for records that are of product 'LANDSAT_MSS_C1'
#' @description \code{is_landsatMSS} checks which records are Landsat MSS records.
#' @inheritParams calc_cloudcov
#' @inherit is_landsat return author inheritParams
#' @export
is_landsatMSS <- function(records) {
  return(is_product_(records, name_product_landsatmss()))
}

#' Returns TRUE for records that are of product 'LANDSAT_TM_C1' (Landsat-5)
#' @description \code{is_landsat5} checks which records are Landsat-5 records.
#' @inherit is_landsat return author inheritParams
#' @export
is_landsat5 <- function(records) {
  return(is_product_(records, name_product_landsat5()))
}

#' Returns TRUE for records that are of product 'LANDSAT_ETM_C1' (Landsat-7)
#' @description \code{is_landsat7} checks which records are Landsat-7 records.
#' @inherit is_landsat return author inheritParams
#' @export
is_landsat7 <- function(records) {
  return(is_product_(records, name_product_landsat7()))
}

#' Returns TRUE for records that are of product 'LANDSAT_8_C1' (Landsat-8)
#' @description \code{is_landsat8} checks which records are Landsat-8 records.
#' @inherit is_landsat return author inheritParams
#' @export
is_landsat8 <- function(records) {
  return(is_product_(records, name_product_landsat8()))
}

#' Returns TRUE for records that are of product group 'Sentinel'
#' @description \code{is_sentinel} checks which records are Sentinel records.
#' @inherit is_landsat return author inheritParams
#' @export
is_sentinel <- function(records) {
  return(is_product_group_(records, name_product_group_sentinel()))
}

#' Returns TRUE for records that are of product 'Sentinel-1'
#' @description \code{is_sentinel1} checks which records are Sentinel-1 records.
#' @inherit is_landsat return author inheritParams
#' @export
is_sentinel1 <- function(records) {
  return(is_product_(records, name_product_sentinel1()))
}

#' Returns TRUE for records that are of product 'Sentinel-2'
#' @description \code{is_sentinel2} checks which records are Sentinel-2 records.
#' @inherit is_landsat return author inheritParams
#' @export
is_sentinel2 <- function(records) {
  return(is_product_(records, name_product_sentinel2()))
}

#' Returns TRUE for records that are of product 'Sentinel-3'
#' @description \code{is_sentinel3} checks which records are Sentinel-3 records.
#' @inherit is_landsat return author inheritParams
#' @export
is_sentinel3 <- function(records) {
  return(is_product_(records, name_product_sentinel3()))
}

#' Returns TRUE for records that are of product 'Sentinel-3' SYNERGY
#' @description \code{is_sentinel3_synergy} checks which records are Sentinel-3 SYNERGY records.
#' @inherit is_landsat return author inheritParams
#' @export
is_sentinel3_synergy <- function(records) {
  return(which(sapply(records[[name_record_id()]], .record_is_syn)))
}

#' Returns TRUE for records that are of product 'Sentinel-3' SLSTR
#' @description \code{is_sentinel3_slstr} checks which records are Sentinel-3 SLSTR records.
#' @inherit is_landsat return author inheritParams
#' @export
is_sentinel3_slstr <- function(records) {
  return(which(sapply(records[[name_record_id()]], .record_is_slstr)))
}

#' Returns TRUE for records that are of product 'Sentinel-3' SRAL
#' @description \code{is_sentinel3_sral} checks which records are Sentinel-3 SRAL records.
#' @inherit is_landsat return author inheritParams
#' @export
is_sentinel3_sral <- function(records) {
  return(which(sapply(records[[name_record_id()]], .record_is_sral)))
}

#' Returns TRUE for records that are of product 'Sentinel-3' OLCI
#' @description \code{is_sentinel3_olci} checks which records are Sentinel-3 OLCI records.
#' @inherit is_landsat return author inheritParams
#' @export
is_sentinel3_olci <- function(records) {
  return(which(sapply(records[[name_record_id()]], .record_is_olci)))
}

#' checks if a record is a Sentinel-3 OLCI record
#' @param record sf data.frame one line
#' @return logical
#' @keywords internal
#' @noRd
.record_is_olci <- function(record) {
  is_olci <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "OL"
  return(ifelse(is.na(is_olci), FALSE, is_olci))
}

#' checks if a record is a Sentinel-3 SLSTR record
#' @param record sf data.frame one line
#' @inherit .record_is_olci return keywords
#' @noRd
.record_is_slstr <- function(record) {
  is_slstr <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "SL"
  return(ifelse(is.na(is_slstr), FALSE, is_slstr))
}

#' checks if a record is a Sentinel-3 SYN record
#' @param record sf data.frame one line
#' @inherit .record_is_olci return keywords
#' @noRd
.record_is_syn <- function(record) {
  is_syn <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "SY"
  return(ifelse(is.na(is_syn), FALSE, is_syn))
}

#' checks if a record is a Sentinel-3 SRAL record
#' @param record sf data.frame one line
#' @inherit .record_is_olci return keywords
#' @noRd
.record_is_sral <- function(record) {
  is_sral <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "SR"
  return(ifelse(is.na(is_sral), FALSE, is_sral))
}

#' checks if a record is a Sentinel-3 continental or global tile
#' @param record sf data.frame one line
#' @inherit .record_is_olci return keywords
#' @noRd
.record_is_s3_continental <- function(record) {
  tile_id <- record[[name_tile_id()]]
  tile_id <- ifelse(is.na(tile_id) || is.null(tile_id), "", tile_id)
  if (record[[name_product()]] == name_product_sentinel3()) {
    return(tolower(tile_id) %in% tolower(names_continental_s3()))
  } else {
    return(FALSE)
  }
}

#' checks if a record is a MODIS reflectance/radiance product
#' @param record data.frame one line
#' @inherit .record_is_olci return keywords
#' @noRd
.record_is_refl_modis <- function(record) {
  # e.g. 'MODIS_MCD19A1'
  return(any(startsWith(.cloudcov_products(), substr(record[[name_product()]], 1, 13)))) 
}
