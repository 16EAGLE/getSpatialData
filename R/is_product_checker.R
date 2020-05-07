#' ---------------------------------------------------------------------
#' name: is_product_checker
#' description: These are check utils for product and product group and
#' work in same manner as is.na(). Apart from product and product group
#' some methods target more specific types that are identified from 
#' the record ids. All methods return logical vectors of the same length as NROW(records).
#' author: Henrik Fisser, 2020
#' ---------------------------------------------------------------------

#' Returns TRUE for records that are of the specified product group
#' @description \code{is.product_group_} checks which records are of product group \code{product_group}
#' @param records sf data.frame
#' @param product_group character specifies the product group to be checked on.
#' @return logical vector
#' @author Henrik Fisser, 2020
#' @export
is.product_group_ <- function(records, product_group) {
  records <- .check_records(records, col.names = c(name_product_group()), as_df = TRUE)
  product_groups <- records[[name_product_group()]]
  return(product_groups == product_group)
}

#' Returns TRUE for records that are of the specified product
#' @description \code{is.product_} checks which records are of product \code{product}.
#' @details Check \link{get_products} for available product names.
#' @param product character specifies the product to be checked on.
#' @param records sf data.frame
#' @inherit is.product_group_ return
#' @author Henrik Fisser, 2020
#' @export
is.product_ <- function(records, product) {
  records <- .check_records(records, col.names = c(name_product()), as_df = TRUE)
  products <- records[[name_product()]]
  return(products == product)
}

#' Returns TRUE for records that are of product group 'Landsat'
#' @description \code{is.landsat} checks which records are Landsat records.
#' @inheritParams calc_cloudcov
#' @return logical vector, same length as number of rows in \code{records}.
#' @author Henrik Fisser, 2020
#' @export
is.landsat <- function(records) {
  return(is.product_group_(records, name_product_group_landsat()))
}

#' Returns TRUE for records that are of product group 'MODIS'
#' @description \code{is.modis} checks which records are MODIS records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.modis <- function(records) {
  return(is.product_group_(records, name_product_group_modis()))
}

#' Returns TRUE for records that are of product group 'MODIS'

#' Returns TRUE for records that are of product 'LANDSAT_MSS_C1'
#' @description \code{is.landsatMSS} checks which records are Landsat MSS records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.landsatMSS <- function(records) {
  return(is.product_(records, name_product_landsatmss()))
}

#' Returns TRUE for records that are of product 'LANDSAT_TM_C1' (Landsat-5)
#' @description \code{is.landsat5} checks which records are Landsat-5 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.landsat5 <- function(records) {
  return(is.product_(records, name_product_landsat5()))
}

#' Returns TRUE for records that are of product 'LANDSAT_ETM_C1' (Landsat-7)
#' @description \code{is.landsat7} checks which records are Landsat-7 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.landsat7 <- function(records) {
  return(is.product_(records, name_product_landsat7()))
}

#' Returns TRUE for records that are of product 'LANDSAT_8_C1' (Landsat-8)
#' @description \code{is.landsat8} checks which records are Landsat-8 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.landsat8 <- function(records) {
  return(is.product_(records, name_product_landsat8()))
}

#' Returns TRUE for records that are of product group 'Sentinel'
#' @description \code{is.sentinel} checks which records are Sentinel records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel <- function(records) {
  return(is.product_group_(records, name_product_group_sentinel()))
}

#' Returns TRUE for records that are of product 'Sentinel-1'
#' @description \code{is.sentinel1} checks which records are Sentinel-1 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel1 <- function(records) {
  return(is.product_(records, name_product_sentinel1()))
}

#' Returns TRUE for records that are of product 'Sentinel-2'
#' @description \code{is.sentinel2} checks which records are Sentinel-2 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel2 <- function(records) {
  return(is.product_(records, name_product_sentinel2()))
}

#' Returns TRUE for records that are of product 'Sentinel-2' L2A
#' @description \code{is.sentinel2_L2A} checks which records are Sentinel-2 L2A records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel2_L2A <- function(records) {
  return(sapply(1:NROW(records), function(i) {
    strsplit(records[i,][[name_record_id()]], "_")[[1]][2] == name_sentinel2_L2A()
  }))
}

#' Returns TRUE for records that are of product 'Sentinel-2' L1C
#' @description \code{is.sentinel2_L1C} checks which records are Sentinel-2 L1C records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel2_L1C <- function(records) {
  return(sapply(1:NROW(records), function(i) {
    strsplit(records[i,][[name_record_id()]], "_")[[1]][2] == name_sentinel2_L1C()
  }))
}

#' Returns TRUE for records that are of product 'Sentinel-3'
#' @description \code{is.sentinel3} checks which records are Sentinel-3 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3 <- function(records) {
  return(is.product_(records, name_product_sentinel3()))
}

#' Returns TRUE for records that are of product 'Sentinel-3' SYNERGY
#' @description \code{is.sentinel3_synergy} checks which records are Sentinel-3 SYNERGY records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_synergy <- function(records) {
  return(which(sapply(1:NROW(records), function(i) {return(.record_is_syn(records[i,]))})))
}

#' Returns TRUE for records that are of product 'Sentinel-3' SLSTR
#' @description \code{is.sentinel3_slstr} checks which records are Sentinel-3 SLSTR records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_slstr <- function(records) {
  return(which(sapply(1:NROW(records), function(i) {return(.record_is_slstr(records[i,]))})))
}

#' Returns TRUE for records that are of product 'Sentinel-3' SRAL
#' @description \code{is.sentinel3_sral} checks which records are Sentinel-3 SRAL records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_sral <- function(records) {
  return(which(sapply(1:NROW(records), function(i) {return(.record_is_sral(records[i,]))})))
}

#' Returns TRUE for records that are of product 'Sentinel-3' OLCI
#' @description \code{is.sentinel3_olci} checks which records are Sentinel-3 OLCI records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_olci <- function(records) {
  return(which(sapply(1:NROW(records), function(i) {return(.record_is_olci(records[i,]))})))
}

# internal product check utils

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
#' @inherit .record_is_olci return
#' @keywords internal
#' @noRd
.record_is_slstr <- function(record) {
  is_slstr <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "SL"
  return(ifelse(is.na(is_slstr), FALSE, is_slstr))
}

#' checks if a record is a Sentinel-3 SYN record
#' @param record sf data.frame one line
#' @inherit .record_is_olci return
#' @keywords internal
#' @noRd
.record_is_syn <- function(record) {
  is_syn <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "SY"
  return(ifelse(is.na(is_syn), FALSE, is_syn))
}

#' checks if a record is a Sentinel-3 SRAL record
#' @param record sf data.frame one line
#' @inherit .record_is_olci return
#' @keywords internal
#' @noRd
.record_is_sral <- function(record) {
  is_sral <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "SR"
  return(ifelse(is.na(is_sral), FALSE, is_sral))
}

#' checks if a record is a Sentinel-3 continental or global tile
#' @param record sf data.frame one line
#' @inherit .record_is_olci return
#' @keywords internal
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

#' Returns TRUE for records that are of product 'Sentinel-5p' or 'Sentinel-5'
#' @description \code{is.sentinel5} checks which records are Sentinel-5 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel5 <- function(records) {
  s5 <- is.product_(records, name_product_sentinel5())
  s5p <- is.product_(records, name_product_sentinel5p())
  s5[which(s5p == TRUE)] <- TRUE
  return()
}

#' Returns TRUE for records that are of product 'Sentinel-5p'
#' @description \code{is.sentinel5} checks which records are Sentinel-5 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel5p <- function(records) {
  return(is.product_(records, name_product_sentinel5p()))
}

#' checks if a record is a MODIS reflectance/radiance product
#' @param record data.frame one line
#' @inherit .record_is_olci return
#' @keywords internal
#' @noRd
.record_is_refl_modis <- function(record) {
  # e.g. 'MODIS_MCD19A1'
  return(any(startsWith(.cloudcov_products(), substr(record[[name_product()]], 1, 13)))) 
}
