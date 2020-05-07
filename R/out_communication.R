# -------------------------------------------------------------
# GENERAL
# -------------------------------------------------------------

#' Outputs errors, warnings and messages
#'
#' @param input character
#' @param type numeric, 1 = message/cat, 2 = warning, 3 = error and stop
#' @param msg logical. If \code{TRUE}, \code{message} is used instead of \code{cat}. Default is \code{FALSE}.
#' @param sign character. Defines the prefix string.
#'
#' @importFrom utils flush.console
#' @keywords internal
#' @noRd

out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", flush = FALSE, verbose = getOption("gSD.verbose")){
  if(!is.na(type) && !type %in% c(1, 2, 3)) type <- 1
  if(isTRUE(flush)) flush.console()
  if(is.null(ll)) if(isTRUE(verbose)) ll <- 1 else ll <- 2
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input, call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){ cat(paste0(sign,input), sep = ifelse(isTRUE(flush), " ", "\n"))
    } else{message(paste0(sign,input))}}}}
}

#' prints character vectors in console combined into one message in out()
#' @param x list of character vectors.
#' @param type numeric as in out().
#' @param msg logical as in out().
#' @return nothing. Console print
#' @keywords internal
#' @noRd
.out_vector <- function(x,type=1,msg=FALSE) {
  
  shout_out <- sapply(x,function(vec) {
    print_out <- sapply(vec,function(v) out(v,type=type,msg=msg))
  })
  
}

#' seperator for console communication
#' @return character
#' @keywords internal
#' @noRd
sep <- function() {
  sep <- "\n----------------------------------------------------------------"
}

#' run silent
#' @param expr an expression
#' @return nothing. runs expression
#' @keywords internal
#' @noRd
quiet <- function(expr){
  return(suppressWarnings(suppressMessages(expr)))
}

# -------------------------------------------------------------
# calc_cloudcov
# -------------------------------------------------------------

#' calculate and communicate processing time
#' 
#' @param numRecords numRecords
#' @param i record number in the loop
#' @param processingTime processingTime
#' @keywords internal
#' @noRd
.calcHOTProcTime <- function(numRecords,i,processingTime) {
  meanProcessingTime <- mean(processingTime)
  queue <- numRecords - 5
  sumProcessingTime <- meanProcessingTime * queue
  if (sumProcessingTime < 1) {
    sumProcessingTime <- "less than 1 minute"
  } else if (round(sumProcessingTime) == 1) {
    sumProcessingTime <- "1 minute"
  } else {
    sumProcessingTime <- paste0(round(as.numeric(sumProcessingTime))," minutes")
  }
  out(paste0(sep(),"\n10/", numRecords," records processed.\nRemaining time: ",
             sumProcessingTime, sep(), "\n"))
}

# -------------------------------------------------------------
# select_*
# -------------------------------------------------------------

#' communicates a selection process start info
#' @param mode character selection mode.
#' @param sep character aesthetic element for console output.
#' @return nothing. Console output.
#' @keywords internal
#' @noRd
.select_start_info <- function(mode,sep) {
  out(paste0(sep,"\n           Starting ",mode," Selection Process           ",sep))
}

#' Throws error because temporal arguments disable a consistent selection
#' @param min_distance numeric
#' @param max_sub_period numeric
#' @return nothing. Throws error
#' @keywords internal
#' @noRd
.select_temporally_incosistent_error <- function(min_distance, max_sub_period) {
  msg <- paste0("Argument 'min_distance' between acquisitions used for dinstinguished timestamps is: ",
min_distance," days. The 'max_period' of covered acquisitions for one timestamp is: ",max_sub_period,". 
With the given 'num_timestamps' these values disable creating a temporally consistent selection. 
You could modify these values (most likely decrease (some of) them.")
  out(msg, 3)
}

#' constructs a console message to be given at the end of a selection process
#' @param selected_i list 'selected' holding for one timestamp: 'ids', 'cMask_paths', 'valid_pixels', 'timestamp'.
#' @return \code{console_info} character vector holding the message
#' @keywords internal
#' @noRd
.select_final_info <- function(selected_i) {
  
  sep <- "----------------------------------------------------------------"
  ts <- selected_i$timestamp
  n_records <- length(selected_i$cMask_paths)
  header <- paste0("- Timestamp: ",ts)
  coverage_info <- paste0("- Coverage of valid pixels in mosaic of selected records: ",round(selected_i$valid_pixels)," %")
  num_selected_info <- paste0("- Number of selected records: ",n_records)
  console_info <- c(sep,header,coverage_info,num_selected_info)
  
}

#' constructs a summary of the console info of \code{.select_final_info}.
#' In addition: if the minimum coverage amongst the timestamps is below 60 \% return a warning message. 
#' In addition: if the mean coverage amongst the timestamps is below 80 \% return a warning message.
#' These warning messages are returned as NULL if the respective condition is not TRUE.
#' @param selected list of lists 'selected' each holding for a timestamp: 'ids', 'cMask_paths', 'valid_pixels', 'timestamp'
#' @return \code{console_summary_warning} list of character vectors holding the messages:
#' [[1]] Summary info
#' [[2]] Warning if minimum coverage is below 60 \% else character "NULL".
#' [[3]] Warning if mean coverage is below 80 \% else character "NULL".
#' The second [[2]] character vector holds the optional warning(s). 
#' @keywords internal
#' @noRd
.select_summary_ts <- function(selected) {
  
  sep <- sep()
  coverages <- sapply(selected,function(x) {x$valid_pixels})
  num_timestamps <- length(selected)
  min_cov <- round(min(coverages))
  mean_cov <- round(mean(coverages))
  p <- " %"
  header <- paste0("\nSelection Process Overall Summary")
  cov_pixels <- "overage of valid pixels "
  info1 <- paste0("\n- Number of timestamps: ",num_timestamps)
  info2 <- paste0("\n- C",cov_pixels,"in timestamp-wise mosaics of selected records: ")
  info3 <- paste0("\n-      Mean:     ",mean_cov,p)
  info4 <- paste0("\n-      Lowest:   ",min_cov,p)
  info5 <- paste0("\n-      Highest:  ",round(max(coverages)),p)
  console_summary <- paste0(sep,header,sep,info1,info2,info3,info4,info5,sep,"\n")
  
  # optional warnings
  min_thresh <- 60
  mean_thresh <- 80
  check_min <- min_cov < min_thresh # return warning below this
  check_mean <- mean_cov < mean_thresh # return warning below this 
  in_ts <- " in selection "
  warn_str <- "This warning is thrown when "
  warn_help <- ". \nIf you aim at a selection with consistently low cloud cover you could e.g.:\n
  - decrease 'num_timestamps',
  - decrease 'min_distance',
  - increase 'max_period'.\n"
  warn_min <- ifelse(check_min,paste0("The lowest c",cov_pixels,in_ts,"is ",min_cov,warn_help,
                                      "\n",warn_str,"the lowest coverage amongst all timestamps is below: ",min_thresh,p,"\n"),"NULL")
  warn_mean <- ifelse(check_mean,paste0("The mean c",cov_pixels,in_ts,"is ",mean_cov,warn_help,
                                        "\n",warn_str,"the mean coverage is below: ",mean_thresh,p,"\n"),"NULL")
  console_summary_warning <- list(console_summary,warn_min,warn_mean)
  
}

#' creates a selection summary for a SAR selection
#' @param SAR_selected list of [[ids]] character vector of selected ids per timestamp, [[period]] character vector
#' of two dates and [[sub-period]] numeric the sub-period number.
#' @param records data.frame.
#' @param params list holding everything inserted into this parameter list in the calling select function.
#' @return \code{console_summary_warning} list of two character vectors holding the messages:
#' [[1]] Summary info
#' [[2]] Warning if minimum coverage of tiles does not reach number of tiles.
#' @keywords internal
#' @noRd
.select_SAR_summary <- function(records, selected_SAR, num_timestamps, params) {
  
  sep <- sep()
  covered_tiles_ts_wise <- sapply(selected_SAR,function(s) {
    num_tiles <- length(unique(records[match(s[["ids"]],records[[params$identifier]]),params$tileid_col]))
  })
  info <- c()
  for (i in 1:length(covered_tiles_ts_wise)) {
    num_tiles <- covered_tiles_ts_wise[i]
    info[i] <- paste0("Timestamp ",i," covers: ",num_tiles," Sentinel-1 tiles")
  }
  console_summary <- paste0(info,sep)
  # check if for all timestamps all tiles are covered
  check_tile_cov <- which(covered_tiles_ts_wise != length(params$tileids))
  # return warning if check_tile_cov has length > 0
  le <- length(check_tile_cov)
  char <- c()
  for (x in check_tile_cov) char <- ifelse(le == 1,x,paste0(char,x,", "))
  warning <- ifelse(le == 0,NA,paste0("\nFor timestamps: \n   ",char,
                                      "\nnot all tiles could be covered with the given parameters. You could e.g.:\n
                                      - decrease 'num_timestamps',
                                      - decrease 'min_distance',
                                      - increase 'max_period'.")) 
  
  console_summary_warning <- list(console_summary,warning)
}

#' communicates the current coverage of valid pixels in select to user through .out()
#' @param base_coverage numeric.
#' @param i integer.
#' @param le_collection integer length of the records queue to look at.
#' @param sensor character sensor name.
#' @return nothing. Console communication
#' @keywords internal
#' @noRd
.select_out_cov <- function(base_coverage, i, le_collection, sensor) {
  
  cov <- as.character(round(base_coverage,2))
  cov <- ifelse(nchar(cov)==5,cov,paste0(cov,"0"))
  i <- ifelse(i==0,1,i)
  out(paste0("\r", "-      ",cov,"  %      [",i,"/",le_collection,"] checked records of ", sensor, "  "), flush = T)
  
}

#' catches and communicates the case where the records data.frame of a sub-period is empty.
#' @param records data.frame.
#' @param ts numeric which timestamp.
#' @param sensor character name of sensors
#' @return nothing. Optionally console warning.
#' @keywords internal
#' @noRd
.select_catch_empty_records <- function(records, ts, sensor = "unspecified") {
  
  if (NROW(records) == 0) {
    out(paste0("No records at timestamp: ",ts," for product: '", sensor, "'. You could e.g.:\n- decrease 'num_timestamps',
               - decrease 'min_distance',
               - increase 'max_period',
               - add another product.\n"),2)
  }
  
}

#' finishs a SAR selection where only SAR records were given. Fills the records data.frame
#' return columns and creates final console summary + optional warning.
#' @param records data.frame.
#' @param selected_SAR list of [[ids]] character vector of selected ids per timestamp, [[period]] character vector
#' of two dates and [[sub-period]] numeric the sub-period number
#' @param params list holding everything inserted into this parameter list in the calling select function.
#' @return records data.frame with two added columns, ready for return to user.
#' @keywords internal
#' @noRd
.select_finish_SAR <- function(records, selected_SAR, num_timestamps, params) {
  
  sep <- sep()
  csw_SAR <- .select_SAR_summary(records,selected_SAR,num_timestamps,params)
  out(paste0(sep,"\nSelection Process Overall Summary",sep))
  out(paste0("- Number of timestamps: ",num_timestamps,"\n"))
  summary <- .out_vector(csw_SAR[[1]]) # SAR selection summary
  rm(summary)
  w <- csw_SAR[[2]]
  if (!is.na(w)) out(w,type=2) # warning
  ids <- lapply(selected_SAR,function(x) {return(x[["ids"]])})
  # add columns to records
  cols <- c(params$selected_col,params$timestamp_col)
  records <- .select_prep_cols(records,cols,params$selected_col)
  for (ts in 1:length(ids)) {
    ids_match <- match(ids[[ts]],records[[params$identifier]])
    records[ids_match,params$selected_col] <- TRUE # is record selected at all
    records[ids_match,params$timestamp_col] <- ts # timestamp for which record is selected
  }
  return(records)
  
}

