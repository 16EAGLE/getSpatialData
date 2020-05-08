#' ---------------------------------------------------------------------
#' name: internal_select_sub
#' description: The selection process is distinguished into a main selection
#' and a sub selection process. These are functions that conduct the process of a
#' sub selection. A sub refers to one timestamp. The sub selection consists
#' of a temporal and a spatial selection. 
#' The process receives a collection of possible records for a specific timestamp and
#' its sub-period. This sub-period can still contradict the user parameter
#' 'max_sub_period' (maximum number of records accepeted to compose one timestamp).
#' In this case the sub selection process choses records that have to be dumped.
#' This results in a new temporal sub-period covering fewer dates and agreeing
#' with 'max_sub_period'. This temporal sub selection process is followed by
#' the spatial sub selection process, which is done through internal_select_mosaicking.
#' author: Henrik Fisser, 2019
#' ---------------------------------------------------------------------

#' calls the steps of a selection for a sub-period
#' this includes enforcement of max_cloudcov_tile and max_sub_period
#' @param records data.frame subsetted to a sub-period.
#' @param aoi aoi.
#' @param period character vector of start and end date.
#' @param period_new character vector an existing period for the timestamp. Default is c().
#' @param base_records character vector of paths to cloud masks that create a base mosaic. Default is NULL.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' The value is the percentage of not yet covered area that shall be covered additionally when adding the record.
#' @param delete_files logical TRUE if all files in tmp_dir shall be deleted at the end of the call.
#' If it is a selection for multiple sensors this should be FALSE as long there is a next sensor following.
#' @param satisfaction_value numeric.
#' @param params list holding everything inserted into this parameter list in the calling select function (8 parameters).
#' @param dir_out character directory where to save intermediate product.
#' @param ts numeric of the current timestamp.
#' @return \code{selected} list of [[ids]] character vector of selected ids, [[cMask_paths]] character vector to cloud masks, 
#' [[valid_pixels]] percentage of valid pixels in mosaic with the given selection, [[period]] charater vector of two dates.
#' @keywords internal
#' @noRd
.select_process_sub <- function(records, aoi,
                                period, period_new = NULL, base_records = NULL,
                                max_sub_period, max_cloudcov_tile, min_improvement, satisfaction_value,
                                delete_files, params, dir_out, ts) {

  # the sub is an ordering of all available records per tile according to aoi cloud cover
  # this is the step where max_cloudcov_tile is ensured
  sub <- .select_sub(records=records,
                     tiles=params$tileids,
                     max_cloudcov_tile=max_cloudcov_tile,
                     aoi_cc_col=params$aoi_cc_col,
                     tileid_col=params$tileid_col,
                     date_col=params$date_col,
                     identifier=params$identifier)
  
  sub <- .check_compact_list(sub)
  if (!inherits(sub, LIST())) return(NA)
  
  # this step enforces max_sub_period. It returns a list of vectors of indices 
  # pointing to records in records. The list is ordererd according to aoi cloud cover
  sub_within <- getSpatialData:::.select_force_period(records,sub,period,max_sub_period,period_new=period_new,
                                     date_col=params$date_col,aoi_cc_col=params$aoi_cc_col)
  
  sub_within <- .check_compact_list(sub_within)
  if (!inherits(sub_within, LIST())) return(NA)
  
  # calculate best mosaic of cloud masks for first timestamp
  if (is.null(base_records)) {
    out(params$sep)
    out(paste0("Calculating mosaic of timestamp: ", ts))
  }
  
  selected <- try(.select_calc_mosaic(records,
                                      base_records=base_records,
                                      aoi,
                                      sub_within,
                                      params$cloud_mask_col,
                                      min_improvement=min_improvement,
                                      satisfaction_value=satisfaction_value,
                                      ts=ts,
                                      dir_out,
                                      params$identifier,
                                      delete_files=delete_files))
  if (inherits(selected, TRY_ERROR())) {
    out(paste0("\nSelection error at timestamp: ",ts),2)
  }
  selected$period <- .identify_period(records[which(records[[params$identifier]] %in% selected$ids),params$date_col])
  return(selected)
  
}

#' selects initial records for a sub-period while ensuring max_cloudcov_tile
#' @param records data.frame subsetted to a sub-period.
#' @param tiles character vector of the tile ids.
#' @param period character vector of start and end date.
#' @param aoi_cc_col character name of aoi cloud cover column.
#' @param tileid_col character name of tile id column.
#' @param date_col character name of the date column.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @return \code{sub} list of numeric vectors, each number is an index to a record in \code{records}
#' @keywords internal
#' @noRd
.select_sub <- function(records, tiles, max_cloudcov_tile,
                        aoi_cc_col, tileid_col, date_col, identifier) {
  
  sub <- lapply(tiles, function(x) {
    rec_tile_sub <- records[which(records[[tileid_col]] == x),]
    i <- as.integer(0)
    lwst_cc <- as.integer(0)
    selected <- c()
    rec_ord <- rec_tile_sub[order(rec_tile_sub[[aoi_cc_col]]),]
    while(!is.na(lwst_cc) && i <= NROW(rec_tile_sub)) {
      i <- i+1
      if (i > NROW(rec_tile_sub)) {# is this needed? hf
        lwst_cc <- NA
      } else {
        lwst_cc <- unlist(rec_ord[i,][identifier]) # id of i lowest
        # ensure max_cloudcov_tile
        cc <- rec_ord[i,aoi_cc_col]
        above_max <- ifelse(is.na(cc) || any(is.null(c(lwst_cc,cc))),TRUE,
                            cc > max_cloudcov_tile)
        if (above_max) {
          lwst_cc <- NA
        } else {
          selected[i] <- which(records[,identifier] == lwst_cc) # get index of record in records
        }
      }
    }
    return(unique(selected))
  })
  names(sub) <- tiles
  return(sub)
}

#' returns a sub list of indices pointing to records within max_sub_period, 
#' returned in orders according to aoi cloud cover 
#' @param records data.frame.
#' @param sub list of numeric vectors each pointing to a record.
#' @param period character vector of start and end date.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param period_new character vector of an existing period for this timestamp.
#' @param date_col character name of the date column.
#' @param aoi_cc_col character name of aoi cloud cover column.
#' @return \code{sub_within} list of numeric vectors, each number is an index to a record in \code{records}
#' @keywords internal
#' @noRd
.select_force_period <- function(records, sub, period, max_sub_period, period_new, 
                                 date_col, aoi_cc_col) {
  
  # check if covered period of timestamp is within max_sub_period
  # and re-calculate period consecutively with record of next-lowest cloud cover
  max_num_sel <- max(sapply(sub, length))
  orders <- sapply(1:max_num_sel,function(i) unlist(sapply(sub,function(x) return(x[i]))))
  orders <- data.frame(orders)
  sub_within <- list()
  covered_tiles <- list()
  
  for (i in 1:NCOL(orders)) {
    order <- orders[,i]
    order <- order[!is.na(order)]
    # first, try to use all records of this order
    dates_x <- records[order,date_col]
    period_tmp <- .select_bridge_period(dates_x,period_new)
    period_tmp_le <- .period_days(period_tmp)
    
    if (period_tmp_le <= max_sub_period) { # the case where all records from current order x are within period_new
      period_new <- period_tmp
      sub_within[[i]] <- order
    } else {
      # for the case where at least one of record of order x is not within period_new
      # try with all values in all possible combinations (not orders). Might be that 
      # from 0 to all records except one are within period
      order_within <- .select_remove_dates(order, records, period_new, max_sub_period,
                                           date_col, aoi_cc_col)
      if (!is.na(order_within[1])) {
        period_new <- c(.select_bridge_period(records[order_within,date_col],period_new))
        
        # check which tile ids were not given in first order and move this record to first order
        covered_tiles[[i]] <- records[order_within,][[name_tile_id()]]
        if (i > 1) {
          tiles_not_in_first <- which(!covered_tiles[[i]] %in% covered_tiles[[1]])
          if (length(tiles_not_in_first) > 0) {
            sub_within[[1]] <- append(sub_within[[1]],order_within[tiles_not_in_first])
            covered_tiles[[1]] <- append(covered_tiles[[1]],covered_tiles[[i]])
            order_within <- order_within[-tiles_not_in_first]
          }
        }
        sub_within[[i]] <- order_within
      }
    }
  }
  
  return(sub_within)
  
}

#' finds optimal dates from a records order within a period of a timestamp and max_sub-period.
#' @param order numeric vector. Values index a record in records.
#' @param records data.frame.
#' @param period_new character vector of two dates.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param date_col date_col.
#' @param aoi_cc_col aoi_cc_col.
#' @return \code{order} numeric vector a subset of x with all records within max_sub_period length
#' @importFrom stats median
#' @keywords internal
#' @noRd
.select_remove_dates <- function(order, records, period_new, max_sub_period, date_col, aoi_cc_col) {
  
  dates <- sapply(records[order, date_col],function(d) {as.Date(d)})
  # for each of the dates count how many records are given, this creates a distribution of the records in time as a basis for reference date selection
  # grade each date according to number of given records and the mean aoi cloud cover of this records
  min_date <- min(dates)
  max_date <- max(dates)
  dates_seq <- min_date:max_date
  date_grade <- list()
  for (d in dates_seq) {
    sel <- order[which(dates == d)]
    # turn low cc values into high ones because high counts of sel are good
    cc <- sapply(records[sel, aoi_cc_col],function(c) {100-as.numeric(c)}) # 100 - cloudcov!!
    value <- ifelse(length(cc)==0,0, mean(length(sel) * cc)) # number of values * the aoi cloudcov
    date_grade[[as.character(d)]] <- value
  }
  
  # select best period with maximum values in sum of grades while ensuring max_sub_period
  # if a period_new is given test all possible periods combined with period_new
  best_period <- try(.select_best_period(date_grade = date_grade, dates_seq = dates_seq, 
                                         min_date = min_date, max_date = max_date,
                                         period_new = period_new,
                                         max_sub_period = max_sub_period))
  if (inherits(best_period, TRY_ERROR())) {
    return(NA)
  } else {
    incl <- .select_subset_to_best_period(dates,best_period)
    order <- if (length(incl) == 0) NA else order[incl]
    return(order)
  }
  
}

#' helper for subsetting records to the best_period
#' @param dates numeric vector of dates as days since 1970-01-01.
#' @param best_period numeric vector of two dates in the same format as dates.
#' @return incl integer vector
#' @keywords internal
#' @noRd
.select_subset_to_best_period <- function(dates, best_period) {
  incl <- intersect(which(dates > best_period[1]), which(dates < best_period[2]))
  return(incl)
}

#' selects best period from graded dates of a timestamp, optionally combined
#' with a period_new while ensuring max_sub_period.
#' @param date_grade numeric vector of grade values.
#' @param dates_seq sequence of characters all dates in given records of adjusted sub-period.
#' @param min_date numeric date as days since 1970-01-01. Minimum date of dates_seq.
#' @param max_date numeric date as days since 1970-01-01. Maximum date of dates_seq.
#' @param period_new character vector of two dates. A period to which given records shall
#' be adjusted. Can be NULL if not existing.
#' @param max_sub_period numeric number of days.
#' @return \code{best_period} numeric vector of two date values as days since 1970-01-01.
#' @keywords internal
#' @noRd
.select_best_period <- function(date_grade, dates_seq, min_date, max_date, 
                                period_new, max_sub_period) {
  
  max_sub_half <- max_sub_period / 2
  air_plus <- as.integer(ifelse(max_sub_half*2+1 <= max_sub_period, max_sub_half+1, max_sub_half))
  air_minus <- as.integer(max_sub_half) - 1
  if (is.null(period_new)) {
    # for each date in dates_seq create the sub_period around it according to max_sub_period
    # calculate the sum grade of all dates within that sub_period
    # check optionally if this sub_period matches max_sub_period together within period_new
    # return the mean grade value or NA
    sum_grade <- sapply(dates_seq, function(d) {
      period_tmp <- c(d - air_minus, d + air_plus)
      if (period_tmp[1] < min_date) period_tmp[1] <- min_date
      if (period_tmp[2] > max_date) period_tmp[2] <- max_date
      first <- which(dates_seq == period_tmp[1])
      last <- which(dates_seq == period_tmp[2])
      sum_grade <- sum(unlist(date_grade[first:last]))
      return(sum_grade)
    })
    best_mid_date <- dates_seq[which(sum_grade == max(sum_grade))][1]
    start <- best_mid_date - air_minus
    dev_from_first <- min_date - start
    shift_up <- dev_from_first > 0
    start <- ifelse(shift_up, min_date, start)
    end <- best_mid_date + air_plus
    dev_from_last <- end - max_date
    shift_low <- dev_from_last > 0
    end <- ifelse(shift_low, max_date, end)
    start <- ifelse(shift_low, start - dev_from_last, start)
    end <- ifelse(shift_up, end + dev_from_first, end)
    best_period <- c(ifelse(start < min_date, min_date, start), ifelse(end > max_date, max_date, end))
    return(best_period)
  } else {
    # find optimal new sub-period from period_new and given grades of dates
    # 1 remove dates from dates_seq and date_grade that cannot be within
    # max_sub_period when combining with period_new
    period_new_date <- sapply(period_new, as.Date)
    period_new_seq <- period_new_date[1]:period_new_date[2]
    # how many new dates can be added before reaching max_sub_period
    air <- max_sub_period - length(period_new_seq)
    if (air <= 0) {
      best_period <- period_new_date
      return(best_period)
    } else {
      # try all possible periods with period_new and air and return the sum of grades within that
      air_minus <- air:0
      air_plus <- rev(air_minus)
      shifted_grades <- sapply(1:length(air_minus),function(i) {
        a <- air_minus[i]
        period_tmp <- as.integer(period_new_date[1]-a):as.integer(period_new_date[2]+air_plus[i])
        sum_grade <- sum(unlist(date_grade[which(period_tmp %in% dates_seq)]))
        return(sum_grade)
      })
      shifted_grades <- shifted_grades[!is.na(shifted_grades)]
      if (length(shifted_grades) == 0) {
        return(NA)
      }
      best_grade <- which(shifted_grades == max(shifted_grades))[1]
      best_period <- c(period_new_date[1] - air_minus[best_grade],
                       period_new_date[2] + air_plus[best_grade])
      return(best_period)
    }
  }
  
}

#' creates initial sub-periods 
#' @param records data.frame.
#' @param period character vector of a start date [1] and an end date [2].
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param params list
#' @return \code{records} with one added numeric column 'sub_period' indicating in which sub-period the record is situated.
#' @keywords internal
#' @noRd
.select_sub_periods <- function(records, period, num_timestamps, params) {
  
  period <- sapply(period,as.Date)
  days <- as.integer(diff(period))
  le_subperiods <- days / num_timestamps
  dates <- sapply(0:num_timestamps,function(i) date <- period[1] + (i * le_subperiods))
  l <- length(dates)
  dates[l] <- dates[l] + 1
  date_col_mirr <- sapply(records[[params$date_col]], as.Date) # mirror of the date column as days since 1970-01-01
  for (i in 1:num_timestamps) {
    within <- intersect(which(date_col_mirr >= dates[i]), which(date_col_mirr < dates[i+1]))
    records[within, params$sub_period_col] <- i
  }
  return(records)
  
}

#' checks first possible date for selection after previous selection according to user-specified min_distance
#' @param period character vector of dates. Last is the end date.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @return \code{min_date} character indicating the minimum possible date for the next sub_period.
#' @keywords internal
#' @noRd
.select_force_distance <- function(period, min_distance) {
  next_date <- as.Date(period[2]) + min_distance
  return(next_date)
}

#' handles the determined earliest start date for next sub-period calculated from min_distance
#' @param first_date character date the earliest possible date of next sub-period.
#' @param period_initial character vector of two dates. Period derived from records of next sub-period.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed. This determines how close together
#' @return \code{period} character vector the actual period for next sub-period.
#' @keywords internal
#' @noRd
.select_handle_next_sub <- function(first_date, period_initial, min_distance, max_sub_period) {
  dfirst_date <- as.Date(first_date)
  dperiod_initial <- as.Date(period_initial)
  
  if (dfirst_date >= dperiod_initial[1] || dfirst_date < dperiod_initial[1]) {
    period <- as.character(c(dfirst_date,dperiod_initial[2]))
    return(period)
  } else if (dfirst_date >= dperiod_initial[2]) {
    # theoretical first date of next sub is larger than the last date of period available in records
    .select_temporally_incosistent_error(min_distance, max_sub_period)
  }
}

#' checks if an new coverage percentage exceeds the min_improvement argument
#' @param min_improvement numeric.
#' @param cov_init numeric.
#' @param cov_aft numeric.
#' @param proportion numeric the proportion of the aoi covered by the current tile.
#' Has to be between 0 and 1.
#' @return exceeds logical.
#' @keywords internal
#' @noRd
.select_exceeds_improvement <- function(min_improvement, cov_init, cov_aft) {
  add_it <- min_improvement < (((cov_aft - cov_init) / cov_init) * 100)
  return(add_it)
}

#' checks which records are within a period of time
#' @param records data.frame.
#' @param period character vector of dates. Last is the end date.
#' @param date_col character name of the date column.
#' @return \code{records} data.frame reduced to matching records.
#' @keywords internal
#' @noRd
.select_within_period <- function(records, period, date_col) {
  dates <- as.Date(records[[date_col]])
  cond <- intersect(which(dates >= period[1]),which(dates <= period[2]))
  records <- records[cond,]
}

#' bridge function to the period identifier \link{.identify_period}. Enables to calculate from
#' a given period with added dates a new period.
#' @param dates_tmp character vector of character dates.
#' @return period_new character holding a period of dates.
#' @keywords internal
#' @noRd
.select_bridge_period <- function(dates_tmp,period_new) {
  period_curr <- .identify_period(dates_tmp)
  period_new <- .identify_period(c(period_new,period_curr))
}