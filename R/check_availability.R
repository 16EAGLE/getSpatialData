#' Check download availability of records
#' 
#' \code{check_availability} checks for each record whether it is available for direct download (can be downloaded instantly) or not (and thus must be ordered before download).
#'
#' @inheritParams get_previews
#' 
#' @note To use this function, you must be logged in at the services required for your request. See the examples and \link{login} for details.
#' @return A data frame of records (as defined with argument \code{records}), extended by a column \code{download_available} (logical).
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @export

check_availability <- function(records, verbose = TRUE){
  
  # checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  .check_login(records)
  
  # create new colunm
  records.names <- colnames(records)
  records$download_available <- NA
  
  # Sentinel
  if("Sentinel" %in% records$product_group){
    out("Checking instant availability for Sentinel records...")
    records.sentinel <- records[records$product_group == "Sentinel",]
    records.sentinel$cred <- .lapply(records.sentinel$product, function(x){
      .CopHub_select(x = "auto", p = x, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass"))
    })
    records[records$product_group == "Sentinel",]$download_available <- .apply(records.sentinel, MARGIN = 1, function(x, names = colnames(records.sentinel)){
      as.logical(toupper(unlist(.get_odata(x$entity_id, x$cred, field = "Online/$value"))))
    })
  }
  
  # Landsat
  if("Landsat" %in% records$product_group){
    out("Checking availability for Landsat records...")
    records[records$product_group == "Landsat",]$download_available <- records[records$product_group == "Landsat",]$level == "l1"
    
    # check for order column
    if(length(records$order_i) > 0){
      if(any(!is.na(records$order_id))){
        status <- sapply(records$order_id[!is.na(records$order_id)], function(id){
          sapply(content(gSD.get(paste0(getOption("gSD.api")$espa, "item-status/", x), getOption("gSD.usgs_user"), getOption("gSD.usgs_pass")))[[1]], function(y) y$status, USE.NAMES = F)
        }, USE.NAMES = F)
        if(any(status[status != "complete"])) status[status != "complete"] <- "FALSE"
        status <- gsub("complete", "TRUE", status)
        records$download_available[!is.na(records$order_id)] <- as.logical(status)
      }
    }
  }
  
  # MODIS
  if("MODIS" %in% records$product_group){
    out("Checking instant availability for MODIS records...")
    records[records$product_group == "MODIS",]$download_available <- TRUE
  }
  
  out(paste0(as.character(length(which(records$download_available))), "/", nrow(records), " records are currently available for download (this includes any completed order)."), type = 1)
  return(.column_summary(records, records.names))
}