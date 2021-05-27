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
#' 
#' @importFrom xml2 xml_text xml_children
#' 
#' @export

check_availability <- function(records, verbose = TRUE){
  
  # checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  #extras <- list(...)
  #if(is.null(extras$return_status)) return_status <- FALSE else return_status <- extras$return_status
  .check_login(records)
  
  # create new colunm
  records.names <- colnames(records)
  records$download_available <- NA
  records$order_status <- "unknown"
  
  # sentinel
  if(any(records$product_group == "sentinel")){
    if(any(!records$is_gnss, na.rm = T)){
      out("Checking instant availability for Sentinel records...")
      records.sentinel <- records[records$product_group == "sentinel" & !records$is_gnss,]
      records.sentinel$cred <- .lapply(records.sentinel$product, function(x){
        .CopHub_select(x = "auto", p = x, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass"))
      })
      records[records$product_group == "sentinel" & !records$is_gnss,]$download_available <- .apply(records.sentinel, MARGIN = 1, function(x, names = colnames(records.sentinel)){
        as.logical(toupper(unlist(.get_odata(x$entity_id, x$cred, field = "Online/$value"))))
      })
    }
    if(any(records$is_gnss, na.rm = T)){
      records[records$is_gnss,]$download_available <- TRUE
    }
  }
  
  # landsat
  if("landsat" %in% records$product_group){
    out("Checking availability for Landsat records...")
    sub <- records$product_group == "landsat"
    
    records[sub,]$download_available <- records[sub,]$level == "l1"
    records$gSD.order_id <- if(is.null(records$order_id)) NA else records[sub,]$order_id
    
    if(any(is.na(records[sub,]$gSD.order_id) & !records[sub,]$download_available)){
      
      out("Investigating matching ESPA orders in the past...")
      # get all order ids of user
      order_ids <- content(.get(paste0(getOption("gSD.api")$espa, "/list-orders"), getOption("gSD.usgs_user"), getOption("gSD.usgs_pass")))
      
      # extract order ids of last 7 days
      order_ids <- gsub('\\["', "", gsub('"]', "", strsplit(xml_text(xml_children(order_ids)[[1]]), '\", \"')[[1]]))
      order_dates <- lapply(order_ids, function(x) strptime(strsplit(x, "-")[[1]][3], format = "%m%d%Y"))
      order_ids <- order_ids[sapply(order_dates, function(x) difftime(Sys.time(), x, units = "days")) <= 7]
      
      # if there is something, digg deeper
      if(!is.na(order_ids[1])){
        
        # get item ids for each order
        item_ids <- lapply(order_ids, function(x){
          response <- content(.get(paste0(getOption("gSD.api")$espa, "/order/", x), getOption("gSD.usgs_user"), getOption("gSD.usgs_pass")))
          response <- unlist(response$product_opts)
          response[grep("inputs", names(response))]
        })
        
        # extract order ids that match records and are still hot for download
        records[sub,]$gSD.order_id <- order_ids[sapply(records[sub,]$record_id, function(x) which(x == item_ids)[1])]
      }
    }
    
    # check for order column
    if(any(!is.na(records[sub,]$gSD.order_id))){
      status <- sapply(records[sub,]$gSD.order_id[!is.na(records[sub,]$gSD.order_id)], function(id){
        sapply(content(.get(paste0(getOption("gSD.api")$espa, "item-status/", id), getOption("gSD.usgs_user"), getOption("gSD.usgs_pass")))[[1]], function(y) y$status, USE.NAMES = F)
      }, USE.NAMES = F)
      #if(isTRUE(return_status)){
      records[sub,]$order_status <- status
      #}
      if(any(status != "complete")) status[status != "complete"] <- "FALSE"
      status <- gsub("complete", "TRUE", status)
      records[sub,]$download_available[!is.na(records[sub,]$gSD.order_id)] <- as.logical(status)
      if(any(as.logical(status))) out("--> Found completed ESPA orders available for download.")
    }
    
    records$order_id <- records$gSD.order_id
    records$ordered <- NA
    records$ordered[!is.na(records$gSD.order_id)] <- TRUE
  }
  
  # modis
  if("modis" %in% records$product_group){
    out("Checking instant availability for MODIS records...")
    records[records$product_group == "modis",]$download_available <- TRUE
  }
  
  # srtm
  if("srtm" %in% records$product_group){
    out("Checking instant availability for SRTM records...")
    records[records$product_group == "srtm",]$download_available <- TRUE
  }
  
  n_avail <- length(which(records$download_available))
  out(paste0(as.character(n_avail), "/", nrow(records), " records are currently available for download (this includes past completed orders that are still available for download)."), type = 1)
  if(n_avail < nrow(records)) out("Use order_data() to order/restore records that are not available for download.")
  return(.column_summary(records, records.names))
}