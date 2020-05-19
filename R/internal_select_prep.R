#' get column names needed for running select_*
#' @return character vector needed_cols
#' @keywords internal
#' @noRd
.get_needed_cols_select <- function() {
  needed_cols_cloudcov <- .cloudcov_get_needed_cols()
  needed_cols_cloudcov <- append(needed_cols_cloudcov, .cloudcov_colnames())
  return(append(needed_cols_cloudcov, c(name_preview_file())))
}

#' checks if a record is supported by .select_*() or not
#' @param record with one row
#' @return \code{is_supported} logical
#' @keywords internal
#' @noRd
.select_supported <- function(record) {
  if (record[[name_product()]] == name_product_sentinel1()) {
    return(TRUE)
  } else {
    is_supported <- .cloudcov_supported(record) # nearly the same unless: Sentinel-1 (supported in select_*())
    return(is_supported)    
  }
}

#' returns internal params used in select_*
#' @param records data.frame.
#' @param mode character which mode is used: "TS", "BT" or "UT".
#' @return \code{params} list of characters.
#' @importFrom stats na.omit
#' @keywords internal
#' @noRd
.select_params <- function(records, mode) {
  modes <- list("TS"="timeseries","BT"="bitemporal","UT"="unitemporal")
  params <- list(selected_col=paste0("selected_for_",modes[[mode]]), # logical column if a record is selected at all
                 pmos_col=name_rgb_mosaic_file(), # path to the RGB mosaic tif where record is included
                 cmos_col=name_cmask_mosaic_file(), # path to the cloud mask mosaic tif where record is included
                 timestamp_col=name_selected_for_timestamp(), # for the timestamp number for which the record is selected
                 aoi_cc_col=name_aoi_hot_cloudcov_percent(),
                 tileid_col=name_tile_id(),
                 preview_col=name_preview_file(),
                 cloud_mask_col=name_cloud_mask_file(),
                 date_col=name_date_acquisition(),
                 identifier=name_record_id(),
                 sub_period_col=name_sub_period())
  params$product_group <- unique(na.omit(records$product_group))
  params$product <- unique(na.omit(records$product))
  params$tileids <- unique(na.omit(records[[params$tileid_col]]))
  params$sep <- sep()
  return(params)
}

#' prep process of a selection process
#' @param records data.frame.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param params list holding everything inserted into this parameter list in the calling select function (8 parameters).
#' @return records data.frame 
#' @keywords internal
#' @noRd
.select_prep <- function(records, num_timestamps, params) {
  records[params$date_col] <- sapply(records[[params$date_col]], as.character)
  period <- .identify_period(records[[params$date_col]])
  # calculates the sub_period column
  records <- .select_sub_periods(records, period, num_timestamps, params)
  # check which records in records are supported by select and mark unsupported records with NA in 'sub_period'
  supported <- sapply(1:NROW(records), function(i) .select_supported(records[i,]))
  records[!supported, params$sub_period_col] <- NA
  return(records)
}

#' wrapper of the preparation steps in select
#' @param records data.frame.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param mode character mode of selection.
#' @return \code{prep} list of [["records"]] data.frame records, [["par"]] list of parameters
#' and [["has_SAR"]] numeric vector indicating if records have SAR records
#' @keywords internal
#' @noRd
.select_prep_wrap <- function(records, num_timestamps, mode) {
  
  out("Preparing", msg=T)
  records <- .unlist_df(records)
  records <- .make_tileid(records)
  params <- .select_params(records, mode)
  params$period <- .identify_period(records[[params[["date_col"]]]])
  records <- .select_prep(records, num_timestamps, params)
  has_SAR <- .has_SAR(params$product) # check if SAR records in records (1 for TRUE, 0 for FALSE or 100 for "all"). If 100 selection is done only for SAR
  prep <- list(records=records,
               params=params,
               has_SAR=has_SAR)
  return(prep)
  
}

#' creates new columns for selection completion and fills with NAs or FALSE
#' @param records data.frame.
#' @param cols character vector of the column names.
#' @param selected_col character name of the 'selected' column.
#' @return records data.frame with new columns.
#' @keywords internal
#' @noRd
.select_prep_cols <- function(records, cols, selected_col) {
  
  for (j in 1:length(cols)) {
    col <- cols[j]
    val <- ifelse(col == selected_col, FALSE, NA)
    records[[col]] <- val
  }
  return(records)
  
}