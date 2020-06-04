# ---------------------------------------------------------------------
# name: internal_select
# description: This document is the main backend of select functionalities. 
# details: It contains select_ specific methods. Rather generic methods that
# might be useful for other package-internal functionalities are situated
# in internal. Checks are in checks. The frontends of select_ are located in dedicated select_ functions.
# author Henrik Fisser, 2019
# ---------------------------------------------------------------------

#' creates list with selected elements
#' @param record_ids character vector of selected record ids
#' @param base_coverage numeric specifies the percentage coverage of valid pixels in aoi
#' @param records sf data.frame
#' @return selected list
#' @keywords internal
#' @noRd
selected <- function(record_ids, base_coverage, records) {
  cmask_paths <- records[which(records[[name_record_id()]] %in% record_ids), name_cloud_mask_file()]
  selected <- list(ids=record_ids,
                   cMask_paths=cmask_paths,
                   valid_pixels=base_coverage)
  return(selected)
}

#' select main process
#' @param records data.frame.
#' @param aoi aoi.
#' @param has_SAR numeric vector indicating if and how much SAR is in records.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps. 
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param satisfaction_value numeric.
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @param params list holding everything inserted into this parameter list in .select_params().
#' @param dir_out character directory where to save intermediate product.
#' @param cols_initial character vector of records column names as input from user.
#' @return records data.frame ready for return to user.
#' @keywords internal
#' @noRd
#' @author Henrik Fisser
.select_main <- function(records,
                         aoi,
                         has_SAR,
                         num_timestamps,
                         min_distance,
                         min_improvement,
                         max_sub_period,
                         max_cloudcov_tile,
                         satisfaction_value,
                         prio_sensors,
                         save_cmos,
                         save_pmos,
                         dir_out,
                         params,
                         cols_initial) {
  
  # if all are SAR records
  if (has_SAR == 100) {
    records <- .select_all_SAR(records, max_sub_period,
                               min_distance, num_timestamps, params)
    records[[name_sub_period()]] <- NULL
    records <- .column_summary(records,cols_initial)
    return(records)
  }
  
  # start Process of optical data selection
  selected <- list() # list to be filled by all selected 'record_id' ids, the valid coverage percentage per timestamp and the cloud mask paths
  sub_periods <- unique(na.omit(records[[name_sub_period()]]))
  if (!all(1:num_timestamps %in% sub_periods)) sub_periods <- 1:num_timestamps
  
  if (is.null(prio_sensors)) {
    given_products <- records[[name_product()]]
    given_products <- unique(given_products[which(given_products != name_product_sentinel1())])
    if (length(given_products) > 1) {
      out("No 'prio_products' specified, generating random product priorities", msg=T)
      prio_sensors <- .generate_random_prio_prods(records)
    } else {
      prio_sensors <- given_products[1]
    }
    out(paste0("Random priority product order: ", paste(prio_sensors, collapse=", "), "\n", sep()), msg=F)
  }
  
  # select per sub-period (=timestamp) best mosaic. The sub-periods are adjusted dynamically according to min_distance, max_sub_period
  for (t in 1:length(sub_periods)) {
    
    if (t > 1) {
      previous_timestamp <- t - 1
      previous_period <- selected[[previous_timestamp]]$period
    } else {
      previous_period <- NA
    }
    selected_ts <- try(.select_process(records,
                                       aoi,
                                       timestamp=t,
                                       min_distance=min_distance, 
                                       max_sub_period=max_sub_period,
                                       max_cloudcov_tile=max_cloudcov_tile, 
                                       min_improvement=min_improvement,
                                       previous_period=previous_period,
                                       satisfaction_value=satisfaction_value,
                                       prio_products=prio_sensors,
                                       params=params,
                                       dir_out=dir_out))
    
    if (inherits(selected_ts, TRY_ERROR())) {
      out(paste0("\nSelection failed for timestamp: ", t), 2)
    }
    if (is.null(selected_ts[["period"]])) {
      # if no records available/selected at the timestamp get default period based on length of period and number timestamps
      selected_ts[["period"]] <- .calc_default_sub_period(params$period, num_timestamps, t)     
    }
    selected_ts[["timestamp"]] <- t
    selected[[t]] <- selected_ts # insert 'selected_ts' list into selected list
    
  }
  
  # if some are SAR records
  if (has_SAR == 1) {
    selected <- .select_some_SAR(records, selected, max_sub_period,
                                 min_distance, num_timestamps, params)
  }
  
  #A Create and save final cloud mask mosaic
  #B Create and save final RGB preview mosaic
  #C Add 3 columns to records data.frame:
  #1 logical column if a record is selected at all
  #2 path to the RGB mosaic tif where record is included
  #3 the timestamp number for which the record is selected
  sep <- params$sep
  out("Writing mosaics", msg=T)
  out(sep, msg=F)
  # create final mosaics for each timestamp and summary message per timestamp
  records <- try(.select_save_mosaics(records, selected=selected, aoi=aoi,
                                      params=params, dir_out=dir_out, save_cmos, save_pmos))
  if (inherits(records, TRY_ERROR())) {
    out("Selection error", 3)
  }
  # create optional warning(s) and overall summary message
  out(paste0(sep, "\nOverall Summary"), msg=F)
  csw <- .select_overall_summary(selected)
  w <- csw[1:2] # warnings
  w <- w[which(w!="NULL")]
  if (length(w) > 0) to_console <- sapply(w, function(x) .out_vector(x, type=2))
  records[[name_sub_period()]] <- NULL # remove sub-period column
  rm(summary, to_console)
  
  records <- .column_summary(records, cols_initial)
  return(records)
  
}

#' selection process
#' @param records data.frame.
#' @param aoi aoi.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' @param previous_period character vector selected period of previous timestamp.
#' @param satisfaction_value numeric.
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @param params list holding everything inserted into this parameter list in .select_params().
#' @param dir_out character directory where to save intermediate product.
#' @param timestamp numeric of the current timestamp.
#' @return \code{selected} list of selected records with all items returned by .select_process_sub
#' @keywords internal
#' @noRd
.select_process <- function(records, aoi,
                            timestamp,
                            min_distance, max_sub_period, max_cloudcov_tile, 
                            min_improvement, previous_period,
                            satisfaction_value, prio_products = NULL,
                            params, dir_out) {

  name_product <- name_product()
  name_product_group <- name_product_group()
  given_products <- unique(records[[name_product]])
  SAR_given <- name_product_sentinel1() %in% given_products
  period_new <- c() # for selection of multiple sensors
  base_records <- c() # same 
  ids <- c() # same
  valid_pixels <- 0 # same
  selected <- NULL
  
  single_prio_product <- length(prio_products) == 1  # single product given
  
  for (s in prio_products) {
    
    if (s == name_product_sentinel1()) next # Sentinel-1 gets dedicated handling
    # enough records selected, no further need
    if (valid_pixels >= satisfaction_value || round(valid_pixels) == 100) break
    
    selection_failed <- FALSE
    if (single_prio_product) {
      # in case prio_sensors is not given process all sensors together
      s_match <- which(!is.na(records[[name_product]]))
    } else {
      # in case prio_sensors is given process sensors in this order
      s_match <- which(records[[name_product]] == s) # check for the product name
      # the prio product can also be a product group in case of Landsat and MODIS
      if (s == name_product_group_landsat() || s == name_product_group_modis()) {
        s_match <- which(records[[name_product_group()]] == s)
      }
    }
    sensor_match <- intersect(which(records$sub_period == timestamp), s_match)
    if (length(sensor_match) == 0) { # no records for sensor s at timestamp
      .select_catch_empty_records(data.frame(), timestamp, s)
      if (single_prio_product) break else next
    }
    
    tstamp <- list()
    tstamp$records <- records[sensor_match,]
    # in case of Sentinel-3 and MODIS we might have non-supported products
    # since the supported products cannot be identified through the product but the record_id
    tstamp$records <- .select_filter_supported(tstamp$records)
    tstamp$records <- tstamp$records[which(!is.na(tstamp$records[[params$sub_period_col]])),]
    tstamp$records <- tstamp$records[which(!is.na(tstamp$records[[params$preview_col]])),]
    .select_catch_empty_records(tstamp$records, timestamp, s)
    tstamp$period <- .identify_period(tstamp$records[[params$date_col]])

    if (timestamp > 1) {
      # enforce to min_distance from previous timestamp
      if (is.null(previous_period) || is.na(previous_period)) {
        selection_failed <- TRUE
      } else {
        tstamp$first_date <- .select_force_distance(previous_period, min_distance)
        tstamp$period <- .select_handle_next_sub(first_date=tstamp$first_date,
                                                 period_initial=tstamp$period,
                                                 min_distance,
                                                 max_sub_period)
        tstamp$records <- .select_within_period(tstamp$records, tstamp$period, params$date_col) # subset to records in period
      }
    }
    
    delete_files <- ifelse(single_prio_product, FALSE, s == tail(prio_products, 1))
    if (!selection_failed) {
      # run the selection process
      selected <- .select_process_sub(tstamp$records,
                                      aoi,
                                      tstamp$period,
                                      period_new=period_new,
                                      base_records=base_records,
                                      max_sub_period,
                                      max_cloudcov_tile,
                                      min_improvement,
                                      satisfaction_value,
                                      delete_files,
                                      params,
                                      dir_out,
                                      ts=timestamp)
    }
    
    selection_failed <- !inherits(selected, LIST())
    if (!selection_failed) {
      if (single_prio_product && !SAR_given) {
        # if only one optical sensor is given
        .select_completed_statement(timestamp)
        return(selected)
      } else {
        # if combined selection of multiple optical sensors
        has_next <- s != tail(prio_products, n=1)
        if ((selected$valid_pixels < satisfaction_value) && has_next) .select_next_product()
        
        # save values of selected
        base_records <- c(base_records, selected$cMask_paths) # for base mosaic -> selection from next sensor
        ids <- unique(c(ids, selected$ids)) # ids of selected records
        names(base_records) <- ids
        valid_pixels <- selected$valid_pixels # percentage of valid pixels in aoi
        period_new <- .identify_period(c(period_new, selected$period)) # combined period
      }
    }  
  }
  
  if (!SAR_given) {
    .select_completed_statement(timestamp)
  }
  
  # if warning has not been thrown before do it
  if (selection_failed || length(ids) == 0) .select_catch_empty_records(data.frame(), timestamp, s)
  
  selected <- list(ids=ids,
                   cMask_paths=base_records,
                   valid_pixels=valid_pixels,
                   period=period_new)
  
  return(selected)
  
}

#' checks if a record is supported by select
#' @param record data.frame single record line
#' @return logical
#' @keywords internal
#' @noRd
.is_select_supported <- function(record) {
  product <- record[[name_product()]]
  if (product == name_product_sentinel3()) {
    is_supported <- .record_is_olci(record)
  } else if (startsWith(product, name_product_group_modis())) {
    is_supported <- .record_is_refl_modis(record)
  } else {
    is_supported <- product %in% get_select_supported()
  }
  return(is_supported)
}

#' filters out products unsupported by select_* from a records data.frame
#' according to the record_id in case of Sentinel-3 and through a more specific
#' product name check in case of MODIS
#' @param records data.frame
#' @return records data.frame without unsupported products
#' @keywords internal
#' @noRd
.select_filter_supported <- function(records) {
  for (i in 1:NROW(records)) {
    is_supported <- .is_select_supported(records[i,])
    if (!is_supported) {
      records <- records[-c(i),]
    }
  }
  return(records)
}

#' generates a random order of prio_products for cases
#' where user has not provided it
#' @param records data.frame
#' @return prio_products character vector randomly generated prio_products
#' @keywords internal
#' @noRd
.generate_random_prio_prods <- function(records) {
  LANDSAT_GROUP <- name_product_group_landsat()
  MODIS_GROUP <- name_product_group_modis()
  given_products <- unique(records[[name_product()]])
  clean_products <- c()
  for (product in unique(given_products)) {
    name_product <- name_product()
    is_supported_modis <- .record_is_refl_modis(data.frame(product = product))
    not_SAR <- product != name_product_sentinel1()
    if (not_SAR && (product %in% get_select_supported() || is_supported_modis)) {
      clean_products <- append(clean_products, product)
    }
  }
  prio_products <- sample(clean_products, length(clean_products))
  prio_products[which(grepl(LANDSAT_GROUP, prio_products))] <- LANDSAT_GROUP
  prio_products[which(grepl(MODIS_GROUP, prio_products))] <- MODIS_GROUP
  return(unique(prio_products))
}
