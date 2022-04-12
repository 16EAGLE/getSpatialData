#' Order datasets
#' 
#' \code{order_data} oders datasets that are not available for immediate download (on-demand) but need to be ordered or restored before download. Use \link{check_availability} to see which datasets need to be ordered before download.
#'
#' @inheritParams get_data
#' @param wait_to_complete logical, whether to wait until all successful orders are complete and available for download. This is useful if you want to automatize subsequent downloads of placed orders.
#' 
#' @note To use this function, you must be logged in at the services required for your request. See the examples and \link{login} for details.
#' @return A data frame of records (as defined with argument \code{records}), extended by additional columns.
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom httr HEAD authenticate
#' @importFrom cli get_spinner
#' @export

order_data <- function(records, wait_to_complete = FALSE, ..., verbose = TRUE){
  
  # checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  extras <- list(...)
  if(is.null(extras$hub)) extras$hub <- "auto"
  if(is.null(extras$wait_interval)) wait_interval <- 15 else wait_interval <- extras$wait_interval
  records <- .check_records(records, c("product", "product_group", "entity_id", "level", "record_id"))
  
  # save names
  records.names <- colnames(records)
  
  # login check
  groups <- unique(records$product_group)
  if("sentinel" %in% groups){
    .check_login("Copernicus")
    out("Please note: The Copernicus LTA quota and retention time is dynamically set according to the usage patterns to ensure efficient access to the most recent and frequently downloaded data. ", type = 2)
  }
  if(any("landsat" %in% groups, "modis" %in% groups)){
    .check_login("USGS")
  }
  
  # check availability
  if(is.null(records$download_available)){
    out("Column 'download_available' not present, calling check_availabilty() to check download availability of records...")
    records <- check_availability(records, verbose = FALSE)
    if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  }
  
  sub <- !(records$download_available | records$order_status == "scheduled" | records$order_status == "oncache")
  if(all(!sub)){
    out("No new orders were placed since the supplied records have been ordered already or are available for download.")
  } else{
    if(any(!sub)){
      out(paste0("Already ordered records present, skipping order of'", paste0(records[!sub,]$record_id, collapse = "', '"), "'..."), msg = T)
    }
    
    # get credendtial info
    records$gSD.cred <- NA
    records[sub,]$gSD.cred <- .apply(records[sub,], MARGIN = 1, function(x){
      if(x$product_group == "sentinel"){
        list(.CopHub_select(x = extras$hub, p = x$product, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass")))
      } else if(x$product_group == "landsat"){
        list(user = getOption("gSD.usgs_user"), pw = getOption("gSD.usgs_pass"))
      }
    })
    
    # get URLs
    out("Assembling dataset URLs...")
    records$gSD.dataset_url <- records$gSD.item <- records$gSD.head <- records$gSD.espa <-NA
    records[sub,]$gSD.dataset_url <- .get_ds_urls(records[sub,])
    
    # items and head
    records[sub,]$gSD.item <- 1:nrow(records[sub,])
    records[sub,]$gSD.head <- .sapply(records[sub,]$gSD.item, function(i, n = nrow(records[sub,])) paste0("[Dataset ", toString(i), "/", toString(n), "] "))
    
    # order/restore items
    if(is.null(records$ordered)) records$ordered <- TRUE
    records[sub,]$ordered <- FALSE
    records$order_id <- NA
    
    out("Attempting orders...")
    # sentinel individually
    #sub_sentinel <- sub & records$product_group == "sentinel"
    records_sub <- do.call(rbind, .lapply(1:nrow(records[sub,]), function(i){
      x <- records[sub,][i,]
      request <- NA
      
      if(x$product_group == "sentinel"){  
        out(paste0(x$gSD.head, "Requesting to restore '", x$record_id, "' from Copernicus Long Term Archive (LTA)..."))
        
        #out("Assembling dataset URLs...")
        x$gSD.dataset_url <- NA
        x$gSD.dataset_url <- .get_ds_urls(x)
        
        # get head first
        retry_count <- 3
        while(retry_count != 0){
          request_head <- try(HEAD(x$gSD.dataset_url, authenticate(unlist(x$gSD.cred)[1], unlist(x$gSD.cred)[2])), silent = T)
          if(!inherits(request_head, "try-error")){
            if(request_head$status_code == 200){
              out(paste0(x$gSD.head, "Record '", x$record_id, "' already available."))
              retry_count <- 0
              x$ordered <- TRUE
            }
            if(request_head$status_code == 202){
              request <- try(.get(x$gSD.dataset_url, username = unlist(x$gSD.cred)[1], password = unlist(x$gSD.cred)[2]), silent = T)
              x$ordered <- TRUE
              retry_count <- 0
              #retry$last_LTA_success <- Sys.time()
            }
            if(request_head$status_code == 503){
              Sys.sleep(2)
              retry_count <- retry_count-1
              if(retry_count == 0){
                out(paste0(x$gSD.head, "Requesting to restore '", x$record_id, "' failed: Copernicus LTA cannot accept new requests since is is busy in handling other requests. Retry later. "), type = 2)
              }
              x$ordered <- FALSE
            }
            if(request_head$status_code == 403){
              out(paste0(x$gSD.head, "Requesting to restore '", x$record_id, "' failed: Coperncius LTA rejected the request, since the number of submitted requested exceeded the dynamic user quota. Retry later."), type = 2)
              x$ordered <- FALSE
              retry_count <- 0
            }
          }
          if(any(inherits(request_head, "try-error"), inherits(request, "try-error"))){
            retry_count <- retry_count-1
            if(retry_count == 0){
              out(paste0(x$gSD.head, "Requesting to restore '", x$record_id, "' failed: Could not place request."), type = 2)
            }
            x$ordered <- FALSE
          }
        }
      }
      if(x$product_group == "landsat"){
        out(paste0(x$gSD.head, "Checking availability of record '", x$record_id, "' for order at ESPA..."))
        r <- .get(url = paste0(getOption("gSD.api")$espa, "available-products/", x$record_id),
                  username = unlist(x$gSD.cred)[1],
                  password = unlist(x$gSD.cred)[2])
        rcon <- try(content(r))
        if(inherits(rcon, "try-error")){
          out(paste0(x$gSD.head, "Record '", x$record_id, "' could not be checked, request failed due to unknown reason."), type = 2)
        }else if("not_implemented" %in% names(rcon)){
          x$order_status <- "not_implemented"
          out(paste0(x$gSD.head, "Record '", x$record_id, "' skipped: invalid ID, as it cannot be found in the ESPA database."), type = 2)
        }else if("date_restricted" %in% names(rcon)){
          x$order_status <- "date_restricted"
          out(paste0(x$gSD.head, "Record '", x$record_id, "' skipped: restricted ID, for which ESPA rejects processing requests."), type = 2)
        }else{
          #out(paste0(x$gSD.head, "Record '", x$record_id, "' ready to be ordered at ESPA."))
          x$gSD.espa <- names(rcon)[1]
        }
      }
      return(x)
    }))
    records[sub,] <- records_sub[,colnames(records)]
    
    # now, order landsat ESPA all at once
    sub_espa <- sub & !is.na(records$gSD.espa)
    levels <- unique(records[sub_espa,]$level)
    for(l in levels){
      sub_espa_level <- sub & !is.na(records$gSD.espa) & records$level == l
      out(paste0("Requesting ESPA order for level '", l, "' of records '", 
                 paste0(records[sub_espa_level,]$record_id, collapse = "', '"), "' all at once..."))
      req.data <- .lapply(1:nrow(records[sub_espa_level,]), function(i){
        list(records[sub_espa_level,][i,]$gSD.espa, records[sub_espa_level,][i,]$record_id)
      })
      
      # submit order
      order_id <- try(.ESPA_order(
        req.data,
        id = records[sub_espa_level,]$record_id,
        level = l, 
        username = records[sub_espa_level,]$gSD.cred[[1]]$user,
        password = records[sub_espa_level,]$gSD.cred[[1]]$pw
      ))
      
      if(!inherits(order_id, "try-error")){
        records[sub_espa_level,]$ordered <- TRUE
        records[sub_espa_level,]$order_id <- order_id
      } else{
        records[sub_espa_level,]$ordered <- FALSE
      }
    }
    if(any(!records[sub,]$ordered)) out(paste0(length(which(!records[sub,]$ordered)), " dataset(s) could not be ordered succesfully. Check column 'ordered' and possibly 'order_status'."), type = 2)
  }
  
  # if(isTRUE(wait_for_order)){
  #   out("Waiting for orders to be completed, since 'wait_for_order' has been set to 'TRUE' (you may abort any time and check manually using 'check_availability()')...")
  #   
  #   retry <- list(do = TRUE, count = 0, sleep = 60)
  #   while(retry$do){
  #     records <- check_availability(records)
  #     if(all(records[sub,]$download_available)) retry$do <- FALSE else retry$count <- retry$count + 1
  #     Sys.sleep(retry$sleep)
  #   }
  # }
  
  if(!all(is.na(records$ordered))){
    if(any(!records[records$ordered,]$download_available) & isTRUE(wait_to_complete)){
      
      cat("\n")
      while(isTRUE(wait_to_complete)){
        download_available <- check_availability(records[records$ordered,], verbose = F)$download_available
        if(all(download_available)){
          cat("\n")
          out("All placed orders are now available for download.")
          records[records$ordered,]$download_available <- download_available
          wait_to_complete <- FALSE
        } else{
          spinner <- get_spinner("earth")
          frames <- spinner$frames
          cycles <- ceiling(wait_interval/(length(frames)*(spinner$interval/1000)))
          for(i in 1:(length(frames) * cycles)-1){
            fr <- unclass(frames[i%%length(frames) + 1])
            cat("\r", fr, "Waiting for orders to be completed, since 'wait_to_complete' was set to 'TRUE'...", sep = "")
            Sys.sleep(spinner$interval/1000)
          }
        }
      }
    }
    if(all(nrow(records[records$ordered,]) > 0, all(records[records$ordered,]$download_available))) out("All placed orders are now available for download.")
  }
  
  return(.column_summary(records, records.names))
}


#' @rdname getSpatialData-deprecated
#' @export
getSentinel_restore <- function(...){
  .Deprecated("order_data", "getSpatialData", "This function is deprecated. Use order_data to order/restore records.")
  
  if(missing(records)){
    extras <- list(...)
    records <- extras$record
  }
  
  order_data(records = records, ...)
}

# records[sub,] <- do.call(rbind, .lapply(1:nrow(records[sub,]), function(i){
#   x <- records[i,]
#   
#   if(isFALSE(x$ordered)){
#     if(x$product_group == "Sentinel"){
#       if(dt_nextorder < Sys.time()){
#         out(paste0(x$gSD.head, "Requesting to restore '", x$record_id, "' from Copernicus Long Term Archive (LTA)..."))
#       
#         out("Assembling dataset URLs...")
#         x$gSD.dataset_url <- NA
#         x$gSD.dataset_url <- .get_ds_urls(x)
#         
#         # get head first
#         request_head <- try(HEAD(x$gSD.dataset_url, authenticate(unlist(x$gSD.cred)[1], unlist(x$gSD.cred)[2])), silent = T)
#         if(!inherits(request_head, "try-error")){
#           if(request_head$status_code == 200) return(TRUE)
#           if(request_head$status_code == 202) request <- try(gSD.get(x$gSD.dataset_url, username = unlist(x$gSD.cred)[1], password = unlist(x$gSD.cred)[2]), silent = T)
#         }
#         if(any(inherits(request_head, "try-error"), inherits(request, "try-error"))){
#           out(paste0(x$gSD.head, "Restoring of '", x$record_id, "' failed. You may have exceeded the quota of allowed LTA requests."), type = 2)
#           return(FALSE)
#         } else dt_nextorder <- Sys.time()+(30*60)
# 
#       
#       }
#     }
        