# ---------------------------------------------------------------------
# name: is_product_checker
# description: These are check utils for product and product group and
# work in same manner as is.na(). Apart from product and product group
# some methods target more specific types that are identified from 
# the record ids. All methods return logical vectors of the same length as NROW(records).
# author: Henrik Fisser, 2020
# ---------------------------------------------------------------------

#' Returns TRUE for records that are of product group 'Landsat' or the referred sub-selection
#' @description These functions check which records are records of the referred product group, product or sub-selection.
#' @inheritParams calc_cloudcov
#' @return logical vector, same length as number of rows in \code{records}.
#' @author Henrik Fisser, 2020
#' @name is.landsat
#' @details
#' 
#' \code{is.landsat} returns TRUE for records that are of product group 'Landsat'.
#' 
#' \code{is.landsatMSS} returns TRUE for records that are of product 'LANDSAT_MSS_C1'.
#' 
#' \code{is.landsat5} returns TRUE for records that are of product 'LANDSAT_TM_C1' (Landsat-5).
#' 
#' \code{is.landsat7} returns TRUE for records that are of product 'LANDSAT_ETM_C1' (Landsat-7).
#' 
#' \code{is.landsat8} returns TRUE for records that are of product 'LANDSAT_8_C1' (Landsat-8).
#' 
#' @export
is.landsat <- function(records) {
  return(is.product_group_(records, name_product_group_landsat()))
}

#' @rdname is.landsat
#' @export
is.landsatMSS <- function(records) {
  return(is.product_(records, name_product_landsatmss()))
}

#' @rdname is.landsat
#' @export
is.landsat5 <- function(records) {
  return(is.product_(records, name_product_landsat5()))
}

#' @rdname is.landsat
#' @export
is.landsat7 <- function(records) {
  return(is.product_(records, name_product_landsat7()))
}

#' @rdname is.landsat
#' @export
is.landsat8 <- function(records) {
  return(is.product_(records, name_product_landsat8()))
}


#' Returns TRUE for records that are of product group 'MODIS' or the referred sub-selection
#' @description These functions check which records are records of the referred product group, product or sub-selection.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @return logical vector
#' @author Henrik Fisser, 2020
#' @name is.modis
#' @export
is.modis <- function(records) {
  return(is.product_group_(records, name_product_group_modis()))
}

#' @rdname is.modis
#' @export
is.modis_terra <- function(records) {
  records <- .check_records(records, as_sf = FALSE)
  return(startsWith(tolower(records[[name_record_id()]]), "mod"))
}

#' @rdname is.modis
#' @export
is.modis_aqua <- function(records) {
  records <- .check_records(records, as_sf = FALSE)
  return(startsWith(tolower(records[[name_record_id()]]), "myd"))
}


#' Returns TRUE for records that are of product group 'Sentinel' or the referred sub-selection
#' @description These functions check which records are records of the referred product group, product or sub-selection.
#' @inheritParams is.landsat
#' @inherit is.landsat return
#' @author Henrik Fisser, 2020
#' @details 
#' 
#' \code{is.sentinel} returns TRUE for records that are of product group 'Sentinel'.
#' 
#' \code{is.sentinel1} returns TRUE for records that are of product 'Sentinel-1'.
#' 
#' \code{is.sentinel1_iw_slc} returns TRUE for records that are of product 'Sentinel-1' IW SLC (Sentinel-1 Interferometric Wideswath Single Look Complex).
#' 
#' \code{is.sentinel1_iw_grdh} returns TRUE for records that are of product 'Sentinel-1' IW GRDH (Interferometric Wideswath Ground Range Detected).
#' 
#' \code{is.sentinel1_iw_raw} returns TRUE for records that are of product 'Sentinel-1' IW RAW (Interferometric Wideswath RAW).
#' 
#' \code{is.sentinel1_iw_ocn} returns TRUE for records that are of product 'Sentinel-1' IW OCN (Interferometric Wideswath OCN).
#' 
#' \code{is.sentinel1_level0} returns TRUE for records that are of product 'Sentinel-1' level 0.
#' 
#' \code{is.sentinel1_level1} returns TRUE for records that are of product 'Sentinel-1' level 1.
#' 
#' \code{is.sentinel1_level2} returns TRUE for records that are of product 'Sentinel-1' level 2.
#' 
#' \code{is.sentinel2} returns TRUE for records that are of product 'Sentinel-2'.
#' 
#' \code{is.sentinel2_L1C} returns TRUE for records that are 'Sentinel-2' Level-1C.
#' 
#' \code{is.sentinel2_L2A} returns TRUE for records that are 'Sentinel-2' Level-2A.
#' 
#' \code{is.sentinel2_S2A} returns TRUE for records that are of product 'Sentinel-2' and platform S2A.
#' 
#' \code{is.sentinel2_S2B} returns TRUE for records that are of product 'Sentinel-2' and platform S2B.
#' 
#' \code{is.sentinel3} returns TRUE for records that are of product 'Sentinel-3'.
#' 
#' \code{is.sentinel3_S3A} returns TRUE for records that are of product 'Sentinel-3' and platform S3A.
#' 
#' \code{is.sentinel3_S3B} returns TRUE for records that are of product 'Sentinel-3' and platform S3B.
#' 
#' \code{is.sentinel3_S3C} returns TRUE for records that are of product 'Sentinel-3' and platform S3C.
#' 
#' \code{is.sentinel3_S3D} returns TRUE for records that are of product 'Sentinel-3' and platform S3D.
#' 
#' \code{is.sentinel3_synergy} returns TRUE for records that 'Sentinel-3' SYNERGY records.
#' 
#' \code{is.sentinel3_slstr} returns TRUE for records that are 'Sentinel-3' SLSTR records.
#' 
#' \code{is.sentinel3_sral} returns TRUE for records that are 'Sentinel-3' SRAL records.
#' 
#' \code{is.sentinel3_olci} returns TRUE for records that are 'Sentinel-3' OLCI records.
#' 
#' \code{is.sentinel5} and \code{is.sentinel5p} return TRUE for records that are 'Sentinel-5' or 'Sentinel-5P'.
#'
#' 
#' @name is.sentinel
#' @export
is.sentinel <- function(records) {
  return(is.product_group_(records, name_product_group_sentinel()))
}

#' @rdname is.sentinel
#' @export
is.sentinel1 <- function(records) {
  return(is.product_(records, name_product_sentinel1()))
}

#' @rdname is.sentinel
#' @export
is.sentinel1_iw_slc <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_substrings(records, "iw", "slc"))
}

#' @rdname is.sentinel
#' @export
is.sentinel1_iw_grdh <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_substrings(records, "iw", "grdh"))
}

#' @rdname is.sentinel
#' @export
is.sentinel1_iw_raw <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_substrings(records, "iw", "raw"))
}

#' @rdname is.sentinel
#' @export
is.sentinel1_iw_ocn <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_substrings(records, "iw", "ocn"))
}

#' @rdname is.sentinel
#' @export
is.sentinel1_level0 <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_level(records, level = "0"))
}

#' @rdname is.sentinel
#' @export
is.sentinel1_level1 <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_level(records, level = "1"))
}

#' @rdname is.sentinel
#' @export
is.sentinel1_level2 <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(.identify_sentinel1_level(records, level = "2"))
}

#' @rdname is.sentinel
#' @export
is.sentinel2 <- function(records) {
  return(is.product_(records, name_product_sentinel2()))
}

#' @rdname is.sentinel
#' @export
is.sentinel2_L1C <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(sapply(1:NROW(records), function(i) {
    record <- records[i,]
    if (is.sentinel2(record)) {
      return(endsWith(strsplit(record[[name_record_id()]], "_")[[1]][2], "1C"))
    } else {
      return(FALSE)
    }
  }))
}

#' @rdname is.sentinel
#' @export
is.sentinel2_L2A <- function(records) {
  records <- .check_records(records, col.names = c(name_record_id()), as_sf = FALSE)
  return(sapply(1:NROW(records), function(i) {
    record <- records[i,]
    if (is.sentinel2(record)) {
      return(endsWith(strsplit(records[i,][[name_record_id()]], "_")[[1]][2], "2A"))
    } else {
      return(FALSE)
    }
  }))
}

#' @rdname is.sentinel
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

#' @rdname is.sentinel
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


#' @rdname is.sentinel
#' @export
is.sentinel3 <- function(records) {
  return(is.product_(records, name_product_sentinel3()))
}

#' @rdname is.sentinel
#' @export
is.sentinel3_S3A <- function(records) {
  return(.identify_sentinel3_sensor(records, "S3A"))
}

#' @rdname is.sentinel
#' @export
is.sentinel3_S3B <- function(records) {
  return(.identify_sentinel3_sensor(records, "S3B"))
}

#' @rdname is.sentinel
#' @export
is.sentinel3_S3C <- function(records) {
  return(.identify_sentinel3_sensor(records, "S3C"))
}

#' @rdname is.sentinel
#' @export
is.sentinel3_S3D <- function(records) {
  return(.identify_sentinel3_sensor(records, "S3D"))
}

#' @rdname is.sentinel
#' @export
is.sentinel3_synergy <- function(records) {
  return(sapply(1:NROW(records), function(i) {return(.record_is_syn(records[i,]))}))
}

#' @rdname is.sentinel
#' @export
is.sentinel3_slstr <- function(records) {
  return(sapply(1:NROW(records), function(i) {return(.record_is_slstr(records[i,]))}))
}

#' @rdname is.sentinel
#' @export
is.sentinel3_sral <- function(records) {
  return(sapply(1:NROW(records), function(i) {return(.record_is_sral(records[i,]))}))
}

#' @rdname is.sentinel
#' @export
is.sentinel3_olci <- function(records) {
  return(sapply(1:NROW(records), function(i) {return(.record_is_olci(records[i,]))}))
}


#' @rdname is.sentinel
#' @export
is.sentinel5 <- function(records) {
  s5 <- is.product_(records, name_product_sentinel5())
  s5p <- is.product_(records, name_product_sentinel5p())
  s5[which(s5p == TRUE)] <- TRUE
  return()
}

#' @rdname is.sentinel
#' @export
is.sentinel5p <- function(records) {
  return(is.product_(records, name_product_sentinel5p()))
}

########################################
# internal product check utils


#' Returns TRUE for records that are of the specified product group
#' @description \code{is.product_group_} checks which records are of product group \code{product_group}
#' @param records sf data.frame
#' @param product_group character specifies the product group to be checked on.
#' @return logical vector
#' @author Henrik Fisser, 2020
#' @noRd
is.product_group_ <- function(records, product_group) {
  records <- .check_records(records, col.names = c(name_product_group()), as_sf = FALSE)
  product_groups <- records[[name_product_group()]]
  return(product_groups == product_group)
}

#' Returns TRUE for records that are of the specified product
#' @description \code{is.product_} checks which records are of product \code{product}.
#' @details Check \link{get_products} for available product names.
#' @param records sf data.frame
#' @param product character specifies the product to be checked on.
#' @inherit is.product_group_ return
#' @author Henrik Fisser, 2020
#' @noRd
is.product_ <- function(records, product) {
  records <- .check_records(records, col.names = c(name_product()), as_sf = FALSE)
  products <- records[[name_product()]]
  return(products == product)
}

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


#' checks if a record is a MODIS reflectance/radiance product
#' @param record data.frame one line
#' @inherit .record_is_olci return
#' @keywords internal
#' @noRd
.record_is_refl_modis <- function(record) {
  # e.g. 'MODIS_MCD19A1'
  return(any(startsWith(.cloudcov_products(), substr(record[[name_product()]], 1, 13)))) 
}
