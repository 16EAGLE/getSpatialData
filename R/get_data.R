#' Download datasets
#' 
#' \code{get_data} downloads the full datasets per records. File paths are added to the records data frame.
#'
#' @inheritParams get_previews
#' @param md5_check logical, whether to check md5 checksums (if available) or not.
#' 
#' @note To use this function, you must be logged in at the services required for your request. See the examples and \link{login} for details.
#' @return A data frame of records (as defined with argument \code{records}), extended by 
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom xml2 xml_children xml_contents
#' @importFrom httr content http_error
#' 
#' @name get_data
#' @export

get_data <- function(records, dir_out = NULL, md5_check = TRUE, ..., verbose = TRUE){
  
  # check arguments
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  extras <- list(...)
  if(is.null(extras$hub)) extras$hub <- "auto"
  if(is.null(extras$source)) extras$source <- "auto"
  records <- .check_records(records, c("product", "product_group", "entity_id", "level", "record_id", "summary"))
  
  # save names
  records.names <- colnames(records)
  
  # login check
  groups <- unique(records$product_group)
  if("Sentinel" %in% groups){
    .check_login("Copernicus")
  }
  if(any("Landsat" %in% groups, "MODIS" %in% groups)){
    .check_login("USGS")
  }
  
  # check availability
  if(is.null(records$available_instantly)){
    out("Column records$available_instantly not present, calling check_availabilty() to check download availability of records...")
    records <- check_availability(records, verbose = FALSE)
    if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  }
  if(all(!records$available_instantly)) out("All supplied records are currently not availabe for download. Use order_records() to make them available for download.", type = 3)
  sub <- which(records$available_instantly)
  if(any(sub)) out("Some records are currently not available for download and will be skipped (see records$available_instantly). Use order_records() to make them available for download.", type = 2)
  
  # get credendtial info
  records$gSD.cred <- NA
  records[sub,]$gSD.cred <- .apply(records[sub,], MARGIN = 1, function(x){
    if(x$product_group == "Sentinel"){
      list(.CopHub_select(x = extras$hub, p = x$product, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass")))
    } else NA
  })
  
  # get MD5 checksums
  records$md5_checksum <- NA
  if(isTRUE(md5_check)){
    out("Receiving MD5 checksums...")
    records[sub,]$md5_checksum <- unlist(.apply(records[sub,], MARGIN = 1, function(x){
      if(x$product_group == "Sentinel"){
        cred <- unlist(x$gSD.cred)
        if(!is.null(x$md5_url)) content(gSD.get(x$md5_url, cred[1], cred[2]), USE.NAMES = F) else NA
      } else NA
    }, verbose = F))
  }
  
  # get URLs
  out("Assembling dataset URLs...")
  records$dataset_url <- NA
  records[sub,]$dataset_url <- .apply(records[sub,], MARGIN = 1, function(x){
    
    # Sentinel Copernicus Hub
    if(x$product_group == "Sentinel"){
      paste0(unlist(x$gSD.cred)[3], "odata/v1/Products('", x$entity_id, "')/$value")
      
    # Landsat 8 Level 1A AWS
    } else if(x$product == "LANDSAT_8_C1" & x$level == "l1"){
      
      # assemble index url
      hv <- strsplit(x$record_id, "_")[[1]][3]
      index_url <- paste0(getOption("gSD.api")$aws.l8, substr(hv, 1, 3), "/", substr(hv, 4, 6), "/", x$record_id, "/index.html")
      
      # get file urls
      list(paste0(gsub("index.html", "", index_url), .sapply(as.character(xml_children(xml_children(xml_contents(content(gSD.get(index_url), encoding = "UTF-8"))[2])[4])), function(y){
        strsplit(y,  '\"')[[1]][2]
      }, USE.NAMES = F)))
    
    # MODIS LAADS
    } else if(x$product_group == "MODIS"){
      
      # assemble file url
      fn <- gsub("Entity ID: ", "", strsplit(x$summary, ", ")[[1]][1]) #positional
      ydoy <- gsub("A", "", strsplit(fn, "[.]")[[1]][2]) #positional
      url <- paste0(getOption("gSD.api")$laads, toString(as.numeric(strsplit(fn, "[.]")[[1]][4])), "/", strsplit(fn, "[.]")[[1]][1], "/", substr(ydoy, 1, 4),
             "/", substr(ydoy, 5, nchar(ydoy)), "/", fn)
      
      # test url
      if(http_error(url)){
        fn.names <- gSD.get(paste0(paste0(head(strsplit(url, "/")[[1]], n = -1), collapse = "/"), ".csv"))
        fn.names <- .sapply(gsub("\r", "", strsplit(content(fn.names), "\n")[[1]][-1]), function(y) strsplit(y, ",")[[1]][1], USE.NAMES = F)
        
        # assign correct fn
        fn <- grep(paste0(strsplit(tail(strsplit(url, "/")[[1]], n=1), "[.]")[[1]][1:4], collapse = "."), fn.names, value = T)
        
        # redefine URL and file
        return(paste0(paste0(head(strsplit(url, "/")[[1]], n=-1), collapse = "/"), "/", fn))
      } else return(url)
      
    } else NA
  }, verbose = verbose)
  
  # create directories
  dir_out <- .check_dir_out(dir_out, "datasets")
  records$gSD.dir <- paste0(dir_out, "/", records$product, "/")
  catch <- .sapply(records$gSD.dir, function(x) if(!dir.exists(x)) dir.create(x, recursive = T))
  
  # file name
  records$dataset_file <- NA
  records[sub,]$dataset_file <- .apply(records[sub,], MARGIN = 1, function(x){
    file <- paste0(x$gSD.dir, "/", x$record_id)
    
    if(x$product_group == "Sentinel"){
      if(isTRUE(x$is_gnss)){
        paste0(file, ".TGZ")
      } else if(x$product == "Sentinel-5P"){
        paste0(file, ".nc")
      } else{
        paste0(file, ".zip")
      }
      
    } else if(x$product == "LANDSAT_8_C1" & x$level == "l1"){
      if(!dir.exists(file)) catch <- try(dir.create(file, recursive = T), silent = T)
      list(paste0(file, "/", .sapply(unlist(x$dataset_url, recursive = T), function(x) tail(strsplit(x, "/")[[1]], n = 1), USE.NAMES = F)))
    
    } else if(x$product_group == "MODIS"){
      paste0(x$gSD.dir, "/", tail(strsplit(x$dataset_url, "/")[[1]], n=1)[1])
    } else NA
  })
  
  # items and head
  records$gSD.item <- 1:nrow(records)
  records$gSD.head <- .sapply(records$gSD.item, function(i, n = nrow(records)) paste0("[Dataset ", toString(i), "/", toString(n), "] "))
  
  # download file
  records$dataset_file <- .apply(records, MARGIN = 1, function(x){
    
    if(isTRUE(x$available_instantly)){
      dataset_url <- unlist(x$dataset_url, recursive = T)
      dataset_file <- unlist(x$dataset_file, recursive = T)
      if(any(is.na(dataset_url), is.na(dataset_file))) NA else {
        
        # attempt download
        download <- .sapply(1:length(dataset_url), function(i){
          file.head <- gsub("]", paste0(" | File ", i, "/", length(dataset_url), "]"), x$gSD.head)
          .retry(gSD.download, url = dataset_url[i],
                 file = dataset_file[i],
                 name = paste0(x$record_id, if(!is.na(x$level)) paste0(" (", x$level, ")") else NULL), 
                 head = file.head, type = "dataset", md5 = x$md5_checksum, prog = if(isTRUE(verbose)) TRUE else FALSE,
                 fail = expression(out(paste0("Attempts to download '", name, "' failed.", type = 2))),
                 retry = expression(out(paste0("[Attempt ", toString(3-n+1), "/3] Reattempting download of '", name, "'..."), msg = T)), delay = 0, value = T)
        })
        
        # return downloaded files
        files <- dataset_file[download]
        if(length(files) > 0) list(files) else return(NA)
      }
    } else{
      out(paste0(x$gSD.head, "Skipping download of dataset '", paste0(x$record_id, if(!is.na(x$level)) paste0(" (", x$level, ")") else NULL), "', since it is not available for download..."), msg = T)
      return(NA)
    }
  })
  return(.column_summary(records, records.names))
}


#' @rdname get_data
#' 
#' @export
getSentinel_data <- get_data

#' @rdname get_data
#' @export

getLandsat_data <- get_data

#' @rdname get_data
#' @export

getMODIS_data <- get_data
