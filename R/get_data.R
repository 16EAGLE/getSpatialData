#' Download datasets
#' 
#' \code{get_data} downloads the full datasets per records. File paths are added to the records data frame.
#'
#' @inheritParams get_previews
#' 
#' @note To use this function, you must be logged in at the services required for your request. See the examples and \link{login} for details.
#' @return A data frame of records (as defined with argument \code{records}), extended by 
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom raster stack values xyFromCell crop crs extent projectRaster crs<- extent<-
#' @importFrom sf st_transform st_coordinates st_sfc
#' 
#' @name get_data
#' @export

get_data <- function(records, dir_out = NULL, ..., verbose = TRUE){
  
  # check arguments
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  extras <- list(...)
  if(is.null(extras$hub)) extras$hub <- "auto"
  
  # save names
  records.names <- colnames(records)
  
  # create directories
  dir_out <- .check_dir_out(dir_out, "datasets")
  records$gSD.dir <- paste0(dir_out, "/", records$product, "/")
  catch <- sapply(records$gSD.dir, function(x) if(!dir.exists(x)) dir.create(x, recursive = T))
  
  # fields
  records$gSD.item <- 1:nrow(records)
  records$gSD.head <- sapply(records$gSD.item, function(i, n = nrow(records)) paste0("[Dataset ", toString(i), "/", toString(n), "] "))
  records$md5_checksum <- NA
  
  ###############################
  ## exceptions to implement:
  # availability
  # order
  ###############################
  
  # login check
  groups <- unique(records$product_group)
  if("Sentinel" %in% groups){
    .check_login("Copernicus")
  }
  if(any("Landsat" %in% groups, "MODIS" %in% groups)){
    .check_login("USGS")
  }
  
  # get credendtial info
  records$gSD.cred <- apply(records, MARGIN = 1, function(x){
    if(x$product_group == "Sentinel"){
      .CopHub_select(x = extras$hub, p = x$product, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass"))
    } else NA
  })
  
  # get MD5 checksums
  records$md5_checksum <- unlist(apply(records, MARGIN = 1, function(x){
    if(x$product_group == "Sentinel"){
      content(gSD.get(x$md5_url, x$gSD.cred[[1]][1], x$gSD.cred[[1]][2]), USE.NAMES = F)
    } else NA
  }))
  
  # get URLs
  sub.ls8l1 <- which(records$product == "LANDSAT_8_C1" & records$level == "l1")
  if(length(sub.ls8l1) > 0){
    
    # Landsat 8 Level 1 AWS
    records[sub.ls8l1,]$dataset_url <- lapply(records[sub.ls8l1,]$dataset_url, function(x){
      paste0(gsub("index.html", "", x), sapply(as.character(xml_children(xml_children(xml_contents(content(gSD.get(x), encoding = "UTF-8"))[2])[4])), function(y){
        strsplit(y,  '\"')[[1]][2]
      }, USE.NAMES = F))
    })
  }
  
  # file name
  records$dataset_file <- unlist(apply(records, MARGIN = 1, function(x){
    file <- paste0(dir_out, "/", x$record_id)
    
    if(x$product_group == "Sentinel"){
      if(isTRUE(records$is_gnss)){
        paste0(file, ".TGZ")
      } else if(x$product == "Sentinel-5P"){
        paste0(file, ".nc")
      } else{
        paste0(file, ".zip")
      }
    } else NA
  }))
  
  # download file
  records$dataset_file <- apply(records, MARGIN = 1, function(x){
    if(x$product_group == "Sentinel"){
      download <- .retry(gSD.download, url = x$dataset_url, file = x$dataset_file, name = x$record_id, head = x$gSD.head, type = "dataset", prog = T,
                         fail = expression(out(paste0("Attempts to download '", name, "' failed.", type = 2))),
                         retry = expression(out(paste0("[Attempt ", toString(3-n+1), "/3] Reattempting download of '", name, "'..."), msg = T)), delay = 0, value = T)
      if(isTRUE(download)) x$dataset_file else NA
    } else NA
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
