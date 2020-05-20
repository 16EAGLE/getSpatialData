#' Order datasets
#' 
#' \code{orde_data} oders datasets that are not available for immediate download (on-demand) but need to be ordered or restored before download. Use \link{check_availability} to see which datasets need to be ordered before download.
#'
#' @inheritParams get_data
#' @param wait_for_order logical, whether to wait until all datasets have been successfully ordered and are ready for download (default) or not. If \code{FALSE}, orders are only placed.
#' 
#' @note To use this function, you must be logged in at the services required for your request. See the examples and \link{login} for details.
#' @return A data frame of records (as defined with argument \code{records}), extended by additional columns.
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom httr HEAD authenticate
#' @export

order_data <- function(records, wait_for_order = FALSE, ..., verbose = TRUE){
  
  # checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  extras <- list(...)
  if(is.null(extras$hub)) extras$hub <- "auto"
  records <- .check_records(records, c("product", "product_group", "entity_id", "level", "record_id", "summary"))
  
  # save names
  records.names <- colnames(records)
  
  # login check
  groups <- unique(records$product_group)
  if("Sentinel" %in% groups){
    .check_login("Copernicus")
    out("Please note: The Copernicus LTA quota currently permits users to request a maximum of one LTA dataset per 30 minutes!", type = 2)
  }
  if(any("Landsat" %in% groups, "MODIS" %in% groups)){
    .check_login("USGS")
  }
  
  # check availability
  if(is.null(records$download_available)){
    out("Column 'download_available' not present, calling check_availabilty() to check download availability of records...")
    records <- check_availability(records, verbose = FALSE)
    if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  }
  if(all(records$download_available)){
    out("All supplied records are available for download.")
  } else{
    sub <- which(!records$download_available)
    
    # get credendtial info
    records$gSD.cred <- NA
    records[sub,]$gSD.cred <- .apply(records[sub,], MARGIN = 1, function(x){
      if(x$product_group == "Sentinel"){
        list(.CopHub_select(x = extras$hub, p = x$product, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass")))
      } else if(x$product_group == "Landsat"){
        list(user = getOption("gSD.usgs_user"), pw = getOption("gSD.usgs_pass"))
      }
    })
    
    # items and head
    records$gSD.item <- 1:nrow(records)
    records$gSD.head <- .sapply(records$gSD.item, function(i, n = nrow(records)) paste0("[Dataset ", toString(i), "/", toString(n), "] "))
    
    # order/restore items
    if(is.null(records$ordered)) records$ordered <- FALSE
    records$order_id <- NA
    
    records[sub,] <- do.call(rbind, .lapply(1:nrow(records[sub,]), function(i){
      x <- records[i,]
      
      if(isFALSE(x$ordered)){
        if(x$product_group == "Sentinel"){
          out(paste0(x$gSD.head, "Requesting to restore '", x$record_id, "' from Copernicus Long Term Archive (LTA)..."))
          
          out("Assembling dataset URLs...")
          x$gSD.dataset_url <- NA
          x$gSD.dataset_url <- .get_ds_urls(x)
          
          # get head first
          request_head <- try(HEAD(x$gSD.dataset_url, authenticate(unlist(x$gSD.cred)[1], unlist(x$gSD.cred)[2])), silent = T)
          if(!inherits(request_head, "try-error")){
            if(request_head$status_code == 200) x$ordered <- TRUE
            if(request_head$status_code == 202){
              request <- try(.get(x$gSD.dataset_url, username = unlist(x$gSD.cred)[1], password = unlist(x$gSD.cred)[2]), silent = T)
              x$ordered <- TRUE
            }
          }
          if(any(inherits(request_head, "try-error"), inherits(request, "try-error"))){
            out(paste0(x$gSD.head, "Restoring of '", x$record_id, "' failed. You may have exceeded the quota of allowed LTA requests."), type = 2)
            x$ordered <- FALSE
          }
        }
        
        if(x$product_group == "Landsat"){
          out(paste0(x$gSD.head, "Requesting order of '", x$record_id, "' at ESPA..."))
          
          request <- try(.ESPA_order(id = x$record_id, level = x$level,
                                     username = unlist(x$gSD.cred)[1], password = unlist(x$gSD.cred)[2],
                                     format = "gtiff", verbose = verbose))
          if(!inherits(request, "try-error")){
            x$ordered <- TRUE
            x$order_id <- request
          } else x$ordered <- FALSE
        }
      } else{
        out(paste0(x$gSD.head, "Skipping '", x$record_id, "', since it already has been ordered..."))
        x$ordered <- TRUE
      }
      x$gSD.cred <- NULL
      return(x)
    }))
    
    if(any(!records[sub,]$ordered)) out("Some datasets could not be ordered succesfully. Check column 'ordered' and retry later for those records that are FALSE.", type = 2)
  }
  
  if(isTRUE(wait_for_order)){
    out("Waiting for orders to be completed, since 'wait_for_order' has been set to 'TRUE' (you may abort any time and check manually using 'check_availability()')...")
    recheck <- TRUE
    while(isTRUE(recheck)){
      records <- check_availability(records)
      if(all(records[sub,]$download_available)) recheck <- FALSE
    }
  }
  
  return(.column_summary(records, records.names))
}