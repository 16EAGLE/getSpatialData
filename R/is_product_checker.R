# ---------------------------------------------------------------------
# name: is_product_checker
# description: These are check utils for product and product group and
# work in same manner as is.na(). Apart from product and product group
# some methods target more specific types that are identified from 
# the record ids. All methods return logical vectors of the same length as NROW(records).
# author: Henrik Fisser, 2020
# ---------------------------------------------------------------------

#' Returns TRUE for records that are of the specified product group
#' @description \code{is.product_group_} checks which records are of product group \code{product_group}
#' @param records sf data.frame
#' @param product_group character specifies the product group to be checked on.
#' @return logical vector
#' @author Henrik Fisser, 2020
#' @export
is.product_group_ <- function(records, product_group) {
  records <- .check_records(records, col.names = c(name_product_group()), as_sf = FALSE)
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
  records <- .check_records(records, col.names = c(name_product()), as_sf = FALSE)
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

#' Returns TRUE for records that are of product group 'MODIS' and sensor Terra
#' @description \code{is.modis_terra} checks which records are MODIS Terra records ('MOD')
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.modis_terra <- function(records) {
  records <- .check_records(records, as_sf = FALSE)
  return(startsWith(tolower(records[[name_record_id()]]), "mod"))
}

#' Returns TRUE for records that are of product group 'MODIS' and sensor Aqua
#' @description \code{is.modis_aqua} checks which records are MODIS Aqua records ('MYD')
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.modis_aqua <- function(records) {
  records <- .check_records(records, as_sf = FALSE)
  return(startsWith(tolower(records[[name_record_id()]]), "myd"))
}

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

#' Returns TRUE for records that are of product 'Sentinel-1' IW SLC
#' @description \code{is.sentinel1_iw_slc} checks which records are Sentinel-1 Interferometric Wideswath (IW)
#' Single Look Complex (SLC) records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel1_iw_slc <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_substrings(records, "iw", "slc"))
}

#' Returns TRUE for records that are of product 'Sentinel-1' IW GRDH
#' @description \code{is.sentinel1_iw_grdh} checks which records are Sentinel-1 Interferometric Wideswath (IW)
#' Ground Range Detected (GRDH) records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel1_iw_grdh <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_substrings(records, "iw", "grdh"))
}

#' Returns TRUE for records that are of product 'Sentinel-1' IW RAW
#' @description \code{is.sentinel1_iw_raw} checks which records are Sentinel-1 Interferometric Wideswath (IW)
#' RAW records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel1_iw_raw <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_substrings(records, "iw", "raw"))
}

#' Returns TRUE for records that are of product 'Sentinel-1' IW OCN
#' @description \code{is.sentinel1_iw_ocn} checks which records are Sentinel-1 Interferometric Wideswath (IW)
#' Ocean (OCN) records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel1_iw_ocn <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_substrings(records, "iw", "ocn"))
}

#' Returns TRUE for records that are of product 'Sentinel-1' Level 0
#' @description \code{is.sentinel1_level0} checks which records are Sentinel-1 level 0 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel1_level0 <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_level(records, level = "0"))
}

#' Returns TRUE for records that are of product 'Sentinel-1' Level 1
#' @description \code{is.sentinel1_level1} checks which records are Sentinel-1 level 1 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel1_level1 <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_level(records, level = "1"))
}

#' Returns TRUE for records that are of product 'Sentinel-1' Level 2
#' @description \code{is.sentinel1_level2} checks which records are Sentinel-1 level 2 records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel1_level2 <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_level(records, level = "2"))
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
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(sapply(1:NROW(records), function(i) {
    record <- records[i,]
    if (is.sentinel2(record)) {
      return(strsplit(records[i,][[name_record_id()]], "_")[[1]][2] == name_sentinel2_L2A())
    } else {
      return(FALSE)
    }
  }))
}

#' Returns TRUE for records that are of product 'Sentinel-2' and sensor S2A
#' @description \code{is.sentinel2_S2A} checks which records are Sentinel-2A records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel2_S2A <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(sapply(1:NROW(records), function(i) {
    record <- records[i,]
    if (is.sentinel2(record)) {
      return(startsWith(strsplit(record[[name_record_id()]], "_")[[1]][1], "S2A"))
    } else {
      return(FALSE)
    }
  }))
}

#' Returns TRUE for records that are of product 'Sentinel-2' and sensor S2B
#' @description \code{is.sentinel2_S2B} checks which records are Sentinel-2B records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel2_S2B <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(sapply(1:NROW(records), function(i) {
    record <- records[i,]
    if (is.sentinel2(record)) {
      return(startsWith(strsplit(record[[name_record_id()]], "_")[[1]][1], "S2B"))
    } else {
      return(FALSE)
    }
  }))
}

#' Returns TRUE for records that are of product 'Sentinel-2' L1C
#' @description \code{is.sentinel2_L1C} checks which records are Sentinel-2 L1C records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel2_L1C <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(sapply(1:NROW(records), function(i) {
    record <- records[i,]
    if (is.sentinel2(record)) {
      return(strsplit(record[[name_record_id()]], "_")[[1]][2] == name_sentinel2_L1C())
    } else {
      return(FALSE)
    }
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

#' Returns TRUE for records that are of product 'Sentinel-3' and sensor S3A
#' @description \code{is.sentinel3_S3A} checks which records are Sentinel-3A records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_S3A <- function(records) {
  return(.identify_sentinel3_sensor(records, "S3A"))
}

#' Returns TRUE for records that are of product 'Sentinel-3' and sensor S3B
#' @description \code{is.sentinel3_S3B} checks which records are Sentinel-3B records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_S3B <- function(records) {
  return(.identify_sentinel3_sensor(records, "S3B"))
}

#' Returns TRUE for records that are of product 'Sentinel-3' and sensor S3C
#' @description \code{is.sentinel3_S3C} checks which records are Sentinel-3C records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_S3C <- function(records) {
  return(.identify_sentinel3_sensor(records, "S3C"))
}

#' Returns TRUE for records that are of product 'Sentinel-3' and sensor S3D
#' @description \code{is.sentinel3_S3D} checks which records are Sentinel-3D records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_S3D <- function(records) {
  return(.identify_sentinel3_sensor(records, "S3D"))
}

#' Returns TRUE for records that are of product 'Sentinel-3' SYNERGY
#' @description \code{is.sentinel3_synergy} checks which records are Sentinel-3 SYNERGY records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_synergy <- function(records) {
  return(sapply(1:NROW(records), function(i) {return(.record_is_syn(records[i,]))}))
}

#' Returns TRUE for records that are of product 'Sentinel-3' SLSTR
#' @description \code{is.sentinel3_slstr} checks which records are Sentinel-3 SLSTR records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_slstr <- function(records) {
  return(sapply(1:NROW(records), function(i) {return(.record_is_slstr(records[i,]))}))
}

#' Returns TRUE for records that are of product 'Sentinel-3' SRAL
#' @description \code{is.sentinel3_sral} checks which records are Sentinel-3 SRAL records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_sral <- function(records) {
  return(sapply(1:NROW(records), function(i) {return(.record_is_sral(records[i,]))}))
}

#' Returns TRUE for records that are of product 'Sentinel-3' OLCI
#' @description \code{is.sentinel3_olci} checks which records are Sentinel-3 OLCI records.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @export
is.sentinel3_olci <- function(records) {
  return(sapply(1:NROW(records), function(i) {return(.record_is_olci(records[i,]))}))
}

########################################
# internal product check utils

#' helper for identifying the Sentinel-1 processing level
#' @param records sf data.frame
#' @param level character '0', '1' or '2'
#' @inherit is.landsat return
#' @keywords internal
#' @noRd
.identify_sentinel1_level <- function(records, level) {
  return(sapply(1:NROW(records), function(i) {
    record <- records[i,]
    if (is.sentinel1(record)) {
      split <- strsplit(record[[name_record_id()]], "_")[[1]]
      return(ifelse(nchar(split[4]) == 0, startsWith(split[5], level), startsWith(split[4], level)))
    } else {
      return(FALSE)
    }
  }))
}

#' helper for identifying the first two substrings of Sentinel-1 record ids
#' @param records sf data.frame
#' @param sub1 character 
#' @param sub2 character
#' @inherit is.landsat return
#' @keywords internal
#' @keywords internal
#' @noRd
.identify_sentinel1_substrings <- function(records, sub2, sub3 = NULL) {
  return(sapply(1:NROW(records), function(i) {
    record <- records[i,]
    split <- strsplit(tolower(record[[name_record_id()]]), "_")[[1]]
    split2_match <- split[2] == sub2
    match <- ifelse(is.null(sub3), split2_match, split2_match && split[3] == sub3)
    return(match)
  }))
}

#' identifies the Sentinel-3 sensor (e.g. Sentinel-3A)
#' @param sensor character e.g. "S3A"
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @keywords internal
#' @noRd
.identify_sentinel3_sensor <- function(records, sensor) {
  return(sapply(1:NROW(records), function(i) {
    record <- records[i,]
    if (is.sentinel3(record)) {
      return(startsWith(record[[name_record_id()]], sensor))
    } else {
      return(FALSE)
    }
  }))
}

#' checks if a record is a Sentinel-3 OLCI record
#' @param record sf data.frame one line
#' @return logical
#' @keywords internal
#' @noRd
.record_is_olci <- function(record) {
  if (is.sentinel3(record)) {
    is_olci <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "OL"
    return(ifelse(is.na(is_olci), FALSE, is_olci))
  } else {
    return(FALSE)
  }
}

#' checks if a record is a Sentinel-3 SLSTR record
#' @param record sf data.frame one line
#' @inherit .record_is_olci return
#' @keywords internal
#' @noRd
.record_is_slstr <- function(record) {
  if (is.sentinel3(record)) {
    is_slstr <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "SL"
    return(ifelse(is.na(is_slstr), FALSE, is_slstr))
  } else {
    return(FALSE)
  }
}

#' checks if a record is a Sentinel-3 SYN record
#' @param record sf data.frame one line
#' @inherit .record_is_olci return
#' @keywords internal
#' @noRd
.record_is_syn <- function(record) {
  if (is.sentinel3(record)) {
    is_syn <- strsplit(record[[name_record_id()]], "_")[[1]][2] == "SY"
    return(ifelse(is.na(is_syn), FALSE, is_syn))
  } else {
    return(FALSE)
  }
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
  if (is.sentinel3(record)) {
    tile_id <- record[[name_tile_id()]]
    tile_id <- ifelse(is.na(tile_id) || is.null(tile_id), "", tile_id)
    if (record[[name_product()]] == name_product_sentinel3()) {
      return(tolower(tile_id) %in% tolower(names_continental_s3()))
    } else {
      return(FALSE)
    }
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
