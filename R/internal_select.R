#' ---------------------------------------------------------------------
#' @name internal_select
#' @description This document is the backend of select functionalities. 
#' @details It contains select_ specific methods. Rather generic methods that
#' might be useful for other package-internal functionalities are situated
#' in internal. Checks are in checks. The frontends of select_ are located in dedicated select_ functions.
#' @author Henrik Fisser, 2019
#' @keywords internal
#' ---------------------------------------------------------------------

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
                         dir_out,
                         params,
                         cols_initial) {
  
  # if all are SAR records
  if (has_SAR == 100) {
    records <- .select_all_SAR(records, max_sub_period,
                               min_distance, num_timestamps, params)
    records <- .column_summary(records,cols_initial)
    return(records)
  }
  
  #### Start Process for optical data selection
  selected <- list() # list to be filled by all selected 'record_id' ids, the valid coverage percentage per timestamp and the cloud mask paths
  sub_periods <- unique(na.omit(records$sub_period))
  
  # select per sub-period (=timestamp) best mosaic. The sub-periods are adjusted dynamically according to min_distance, max_sub_period
  for (t in 1:length(sub_periods)) {
    
    previous_period <- ifelse(t > 1,selected[[t-1]]$period,NA)
    
    selected_ts <- try(.select_process(records,
                                       aoi,
                                       timestamp=t,
                                       min_distance=min_distance, 
                                       max_sub_period=max_sub_period,
                                       max_cloudcov_tile=max_cloudcov_tile, 
                                       min_improvement=min_improvement,
                                       previous_period=previous_period,
                                       satisfaction_value=satisfaction_value,
                                       prio_sensors=prio_sensors,
                                       params=params,
                                       dir_out=dir_out))
    
    if (inherits(selected_ts,"try-error")) {
      out("\nSelection failed for timestamp: ",t)
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
  mode_console <- ifelse(length(selected)==1,""," per timestamp")
  sep <- params$sep
  out(paste0(sep,"\nSelection Process Summary",
             mode_console))
  # create final mosaics for each timestamp and summary message per timestamp
  records <- .select_save_mosaics(records,selected=selected,aoi=aoi,
                                  params=params,dir_out=dir_out)
  # create optional warning(s) and overall summary message
  csw <- .select_summary_ts(selected)
  w <- csw[2:3] # warnings
  w <- w[which(w!="NULL")]
  summary <- .out_vector(csw[[1]])
  if (length(w) > 0) to_console <- sapply(w,function(x) .out_vector(x,type=2))
  
  records <- subset(records,select=-sub_period) # remove sub-period column
  
  records <- .column_summary(records,cols_initial)
  
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
                            satisfaction_value, prio_sensors = NULL,
                            params, dir_out) {
  
  completed_info <- paste0("\nCompleted selection process for timestamp: ",timestamp)
  period_new <- c() # for selection from multiple sensors
  base_records <- c() # same 
  ids <- c() # same
  valid_pixels <- 0 # same
  
  if (is.null(prio_sensors) || length(prio_sensors) == 1) {
    le_prio_is_one <- TRUE
    prio_sensors <- "none"
  } else {
    le_prio_is_one <- FALSE
  }
  
  for (s in prio_sensors){
    
    if (le_prio_is_one) {
      # in case prio_sensors is not given process all sensors together
      s_match <- which(!is.na(records$product))  
    } else {
      # in case prio_sensors is given process sensors in this order
      s_match <- which(records$product==s)
      # in case of MODIS as prio_sensor we get 'MODIS' from the user, which does not match any product name
      if (length(s_match) == 0) {
        s_match <- which(startsWith(records$product, s)) # all MODIS products
      }
    }
    sensor_match <- intersect(which(records$sub_period==timestamp),s_match)
    if (length(sensor_match) == 0) { # no records for sensors s at timestamp
      if (le_prio_is_one) .select_catch_empty_records(data.frame(),timestamp) else break
    } 
    
    tstamp <- list()
    tstamp$records <- records[sensor_match,]
    tstamp$records <- records[which(!is.na(records[[params$sub_period_col]])),]
    tstamp$records <- tstamp$records[which(!is.na(tstamp$records[[params$preview_col]])),]
    .select_catch_empty_records(tstamp$records, timestamp)
    tstamp$period <- .identify_period(tstamp$records[[params$date_col]])
    
    if (timestamp > 1) {
      # enforce to min_distance from previous timestamp
      tstamp$first_date <- .select_force_distance(previous_period,min_distance)
      tstamp$period <- .select_handle_next_sub(first_date=tstamp$first_date,
                                               period_initial=tstamp$period,
                                               min_distance,max_sub_period)
      tstamp$records <- .select_within_period(records,tstamp$period,params$date_col) # subset to records in period
    }
    
    delete_files <- ifelse(le_prio_is_one,FALSE,ifelse(s == tail(prio_sensors,1),TRUE,FALSE))
    
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
    
    if (class(selected) != "list") {
      .select_catch_empty_records(data.frame(),timestamp)
    } else {
      if (isFALSE(le_prio_is_one)) {
        # if combined selection of multiple optical sensors
        if (selected$valid_pixels < satisfaction_value) out("\nSelecting records for next sensor in 'prio_sensors'")
        base_records <- c(base_records,selected$cMask_paths) # for base mosaic for selection from next sensor
        ids <- unique(c(ids,selected$ids)) # ids of selected records
        names(base_records) <- ids
        valid_pixels <- selected$valid_pixels # percentage of valid pixels in aoi
        period_new <- .identify_period(c(period_new,selected$period)) # combined period
      } else {
        # if only one optical sensor is given
        out(completed_info)
        return(selected)
      }
    }  
  }
  
  if (length(ids) == 0) .select_catch_empty_records(data.frame(),timestamp)
  
  selected <- list(ids=ids,
                   cMask_paths=base_records,
                   valid_pixels=valid_pixels,
                   period=period_new)
  
  out(completed_info)
  
  return(selected)
  
}

