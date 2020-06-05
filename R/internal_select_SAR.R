# ---------------------------------------------------------------------
# name: internal_select_sub
# description: These functions do the temporal selection of SAR (Sentinel-1) records.
# They create a temporary tile id for each record, according to which they can be
# selected. As they are cloud-free no further spatial selection is necessary. These
# records are thus only selected according to their tile and temporal characteristics.
# This can be done in accordance with previously selected optical records. The sub-periods
# selected for optical records are handed over to the SAR selection. Selection of optical
# records has priority due to cloud cover constraints. SAR selection is hence temporally
# guided by the optical selection in case there is one. Otherwise, SAR selection is
# conducted independently.
# author: Henrik Fisser, 2019
# ---------------------------------------------------------------------

#' select timestamps for SAR data according to num_timestamps, min_distance and max_sub_period.
#' @param records data.frame.
#' @param period_new_all list of character vectors of two dates, one vector for each timestamp.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param max_sub_period numeric maximum length of sub-period.
#' @param params list holding everything inserted into this parameter list in the calling select function.
#' @return \code{SAR_selected} list of [[ids]] character vector of selected ids per timestamp, [[period]] character vector
#' of two dates and [[sub-period]] numeric the sub-period number
#' character vector of two dates (start and end date of SAR sub-period)
#' @keywords internal
#' @noRd
.select_SAR <- function(records, period_new_all = NULL,
                        max_sub_period, min_distance, num_timestamps, params) {
  
  out(sep(), msg=F)
  out("Selecting SAR records", msg = T)
  .select_SAR_start()

  ids <- c()
  period <- c()
  
  sensor_name <- name_product_sentinel1()
  subperiods <- unique(records$sub_period)
  s_match <- which(records[[name_product()]] == sensor_name)
  # if the length of the current sub-period is longer than max_sub_period grade all dates in sub_period
  # according to the number of the given records for each date and calculate the distance of each record
  # from this date. Exclude records consecutively until max_sub_period is reached
  selected_SAR <- list() # to be filled with the selected lists of selected ids and sub-periods
  
  for (timestamp in 1:max(subperiods)) {
    
    # sensor_match are all Sentinel-1 records of this sub-period
    sensor_match <- intersect(s_match, which(records$sub_period == timestamp))
    records_in_s <- records[sensor_match,]

    if (NROW(records_in_s) > 0) {
      if (inherits(records_in_s, TRY_ERROR())) out("Failed to create tile id", 3)
      
      period_new <- period_new_all[[timestamp]]
      previous <- timestamp - 1
      
      if (timestamp > 1) {
        # enforce min_distance
        # combined period of period_new from optical records (if not NULL because has_SAR == 100) and previous SAR period
        previous_period <- .identify_period(c(period_new_all[[previous]], selected_SAR[[previous]]$period))
        period_initial <- .identify_period(records_in_s[[params$date_col]])
        # earliest date of next sub-period adjusted
        first_date <- .select_force_distance(previous_period, min_distance)
        period_s <- .select_handle_next_sub(first_date, period_initial,
                                            min_distance, max_sub_period)
        records_in_s <- .select_within_period(records_in_s, period_s, params$date_col) # subset to records within period_s
      }
      
      dates_s <- sapply(records_in_s[[params$date_col]], as.Date)
      if (!is.null(period_new)) {
        period_new_dates <- sapply(period_new, as.Date)
        dates_combined <- unlist(c(dates_s, period_new_dates))
        dates_s <- min(dates_combined, max(dates_combined))
      }
      
      min_dates_s <- min(dates_s)
      max_dates_s <- max(dates_s)
      sub_period_le <- max_dates_s-min_dates_s
      # grade dates and exclude consecutively
      
      if (sub_period_le > max_sub_period) {
        dates_seq <- min_dates_s:max_dates_s
        date_grade <- sapply(dates_seq, function(date) {
          records_match <- which(dates_s == date)
          # include one record per tile for the grading as for SAR several records on one date = no benefit
          grade <- length(unique(records[records_match, params$tileid_col]))
        })
        
        # calculate best_period based on date_grade, in combination with period_new
        # and while ensuring max_sub_period
        best_period <- .select_best_period(date_grade=date_grade, 
                                           dates_seq=dates_seq, 
                                           min_date=min(dates_seq), 
                                           max_date=max(dates_seq),
                                           period_new=period_new,
                                           max_sub_period=max_sub_period)
        
        if (!is.na(best_period)) {
          incl <- .select_subset_to_best_period(dates_s, best_period)
          ids <- records_in_s[incl,params$identifier] # ids of selected records in updated sub-period
        }
      } else {
        ids <- records_in_s[[params$identifier]] # all ids of records in sub-period
      }
      if (!.is_empty_array(ids)) {
        selected_tile_ids <- records_in_s[which(ids %in% records_in_s[[params$identifier]]), name_tile_id()]
        ids_subset <- sapply(unique(selected_tile_ids), function(tile) {
          return(which(selected_tile_ids == tile)[1]) # take one available record of tile id
        })
        ids <- ids[ids_subset]
      }
      dates_sel <- records_in_s[which(ids %in% records[[params$identifier]]), params$date_col]
      period <- .identify_period(dates_sel)
    }
    
    no_SAR_period <- is.null(period)
    if (no_SAR_period && is.null(period_new_all[[timestamp]])) {
      period <- .calc_default_sub_period(params$period, num_timestamps, timestamp)
    }
    selected_SAR[[timestamp]] <- list("ids"=ids,
                                      "period"=period,
                                      "timestamp"=timestamp)
    .select_SAR_timestamp_status(timestamp, selected_SAR[[timestamp]], length(ids))
  }
  
  return(selected_SAR)
  
}

#' selection process when only SAR given in records
#' @param records data.frame.
#' @param max_sub_period numeric max_sub_period.
#' @param min_distance numeric min_distance.
#' @param num_timestamps numeric num_timestamps.
#' @param params list par.
#' @return records data.frame ready for return to user
#' @keywords internal
#' @noRd
.select_all_SAR <- function(records, max_sub_period,
                            min_distance, num_timestamps,
                            params) {
  
  selected_SAR <- .select_SAR(records, period_new_all = NULL,
                              max_sub_period,min_distance,
                              num_timestamps,params)
  records <- .select_finish_SAR(records, selected_SAR, num_timestamps, params)
  return(records)
  
}

#' selection process for SAR when some SAR given in records
#' @param records data.frame.
#' @param selected list selected.
#' @param max_sub_period numeric max_sub_period.
#' @param min_distance numeric min_distance.
#' @param num_timestamps numeric num_timestamps.
#' @param params list par.
#' @return selected list of lists where each list is a selected list of one timestamp
#' @keywords internal
#' @noRd
.select_some_SAR <- function(records, selected, max_sub_period,
                             min_distance, num_timestamps, params) {
  
  period <- "period"
  ids <- "ids"
  # SAR records shall be searched within max_sub_period combined
  # with the periods selected for optical 
  period_new_all <- lapply(selected,function(x) return(x[[period]]))
  selected_SAR <- .select_SAR(records, period_new_all, max_sub_period, min_distance,
                              num_timestamps,params)
  
  # add selected ids to selected list of optical records
  for (ts in 1:length(selected_SAR)) {
    ts_SAR <- selected_SAR[[ts]]
    optical_ids <- selected[[ts]][[ids]]
    selected[[ts]][[ids]] <- append(optical_ids, ts_SAR[[ids]])
    selected[[ts]][[period]] <- .identify_period(c(selected[[ts]]$period,ts_SAR[[period]]))
    .select_completed_statement(ts)
  }
  return(selected)
}
