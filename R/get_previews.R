#' Get previews of records
#' 
#' \code{get_previews} downloads and georeferences preview images per records and saves them to disk. File paths are added to the records data frame.
#'
#' @inheritParams get_records
#' @param records records data frame, containing one or multiple records (each represented by one row), as returned by \link{get_records}
#' @param dir_out character, a directory, to which the downloaded file(s) should be saved. By default, these are saved to the archive directory defined with \code{set_archive}.
#' @param force logical, whether to force download of files already existing or not.
#'  
#' @note To use this function, you must be logged in at the services required for your request. See the examples and \link{login} for details.
#' @return A data frame of records (as defined with argument \code{records}), extended by two columns: \code{preview_file} (character, path to georeferenced preview) and \code{preview_file_jpg} (character, path to preview JPG).
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom raster stack values xyFromCell crop crs extent projectRaster crs<- extent<-
#' @importFrom sf st_transform st_coordinates st_sfc
#' @export

get_previews <- function(records, dir_out = NULL, force = FALSE, as_sf = TRUE, ..., verbose = TRUE){
  
  # check hidden arguments
  extras <- list(...)
  if(is.null(extras$hub)) extras$hub <- "auto"
  
  # checks
  records <- .check_records(records, c("product", "product_group", "record_id", "preview_url"), as_sf = TRUE)
  if(all(is.na(records$preview_url))) out(paste0("No previews are available for ", nrow(records), "/", nrow(records), " of the supplied records."), type = 3)
  dir_out <- .check_dir_out(dir_out, "previews")
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  
  # save names
  records.names <- colnames(records)
  
  # head 
  records$gSD.item <- 1:nrow(records)
  records$gSD.head <- sapply(records$gSD.item, function(i, n = nrow(records)) paste0("[Preview ", toString(i), "/", toString(n), "] "))
  
  # file 
  records$gSD.dir <- paste0(dir_out, "/", records$product, "/")
  catch <- sapply(records$gSD.dir, function(x) if(!dir.exists(x)) dir.create(x, recursive = T))
  rm(catch)

  files <- paste0(records$gSD.dir, records$record_id, "_preview.jpg")
  
  # check login
  groups_download <- unique(records$product_group[!sapply(files, file.exists)])
  records$gSD.cred <- NA
  if("sentinel" %in% groups_download){
    .check_login(services = "Copernicus")
    records[records$product_group == "sentinel",]$gSD.cred <- lapply(records[records$product_group == "sentinel",]$product, function(x){
      .CopHub_select(x = extras$hub, p = x, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass"))
    })
  }
  
  # download preview jpg files
  out("Downloading previews...")
  records$preview_file_jpg <- unlist(mapply(url = records$preview_url, file = paste0(records$gSD.dir, records$record_id, "_preview.jpg"), 
                                     head = records$gSD.head, cred = records$gSD.cred, name = records$record_id,
                                     function(url, file, name, head, cred){
                                       
    # download
    if(is.list(url)) url <- url[[1]][[1]]
    if(isFALSE(is.url(url))) return(NA) else{
      download <- .download(url = url, file = file, name = name, head = head, type = "preview", prog = F, force = force,
                               username = if(!is.na(cred[[1]][1])) cred[[1]][1] else NULL,
                               password = if(!is.na(cred[[1]][1])) cred[[1]][2] else NULL)
      if(isFALSE(download)) return(NA) else return(file)
    }
  }, USE.NAMES = F, SIMPLIFY = F))
  
  # georeferncing
  out("\nGeoreferncing previews...")
  records$preview_file <- unlist(mapply(file.jpg = records$preview_file_jpg, file.tif = gsub(".jpg", ".tif", records$preview_file_jpg),
                                 group = records$product_group, footprint = records$footprint, head = records$gSD.head, function(file.jpg, file.tif, group, footprint, head, records.crs = st_crs(records)){
    
    if(is.na(file.jpg)) return(NA)
    if(file.exists(file.tif)){
      out(paste0(head, "Skipping converting of '", file.jpg, "', since '", file.tif, "' already exists..."))
      return(file.tif)
    }
    
    # process ###### EDIT THIS SECTION: PROJ/WKT, GDAL, raster? #########
    tryCatch({
      out(paste0(head, "Converting '", file.jpg, "' into '", file.tif, "'..."))
      
      # crop preview
      prev <- stack(file.jpg)
      v <- values(prev)
      rm.prev <- apply((v == 0), MARGIN = 1, function(x) all(x))
      cc.keep <- xyFromCell(prev, which(rm.prev == F))
      # check if has non-zeros DNs, it should!
      has_non_zeros <- any(v > 0)
      if (has_non_zeros) {
        prev <- crop(prev, extent(min(cc.keep[,1]), max(cc.keep[,1]), min(cc.keep[,2]), max(cc.keep[,2])))
      } else {
        stop()
      }
      # assign preview CRS and footprint
      footprint <- st_sfc(footprint, crs = records.crs)
      if(group == "modis") footprint <- st_transform(x = footprint, crs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
      crs(prev) <- crs(as(footprint, "Spatial"))
      footprint <- st_coordinates(footprint)
      x_dim <- footprint[, "X"]
      y_dim <- footprint[, "Y"]
      extent(prev) <- extent(min(x_dim), max(x_dim), 
                             min(y_dim), max(y_dim))
      wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
      if(group == "modis") {
        prev <- projectRaster(prev, crs = crs(wgs84))
        prev[prev<0] <- 0
      } else {
        crs(prev) <- wgs84 # ensure preview has its crs
      }

      # write
      prev <- .ensure_minmax(prev) # sometimes values are above 255 (modis), ensure 0-255
      writeRaster(prev, file.tif)
      return(file.tif)
    }, error = function(e){
      out(paste0(head, "Could not process '", file.jpg, "'..."), type = 2)
      return(NA)
    })
  }, USE.NAMES = F, SIMPLIFY = F))
  
  records <- .check_records(records, as_sf = as_sf)
  return(.column_summary(records, records.names))
}
