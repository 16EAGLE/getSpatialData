# ---------------------------------------------------------------------
# name: internal
# description: These are internal functions that rather work as utils.
# This internal file should only contain generic functions that can be
# used multiple times within the package.
# ---------------------------------------------------------------------

#' creates a temp dir (tmp_dir) and/or deletes it
#' @param dir_out character directory as parent dir.
#' @param action numeric, 1 for create.
#' @param change_raster_tmp logical if TRUE the raster tmp dir will be 
#' changed to the created tmp dir.
#' @param tmp_orig character directory. If change_raster_tmp == TRUE and action == 2
#' the raster tmp dir will be changed to this dir.
#' @importFrom raster rasterOptions
#' @keywords internal
#' @noRd
.tmp_dir <- function(dir_out, action = 2, change_raster_tmp = F, tmp_orig = NULL) {
  
  tmp_dir <- file.path(dir_out,"tmp")
  if (dir.exists(tmp_dir) && action ==2) {unlink(tmp_dir,recursive=T)}
  if (action == 1) {dir.create(tmp_dir)}
  if (isTRUE(change_raster_tmp)) {
    if (action == 1) {
      rasterOptions(tmpdir=tmp_dir)
    } else {
      rasterOptions(tmpdir=tmp_orig)
    }
    
  }
  return(tmp_dir)
  
}

#' extracts the grd and gri file path from a raster object and deletes them
#' @param dir character directory the tmp dir.
#' @param patterns character vector of file extensions of files to be deleted.
#' Default are ".gri" and ".grd".
#' @return nothing. Deletes all files in tmp folder on disk
#' @keywords internal
#' @noRd
.delete_tmp_files <- function(dir, patterns = c(".gri",".grd")) {
  
  patterns <- paste0(patterns,"$")
  files <- unlist(sapply(patterns,function(p) list.files(dir,pattern=p)))
  paths_del <- file.path(dir,files)
  del <- sapply(paths_del,function(path) {
    try <- try(unlink(path))
  })
  
}

#' column summary 
#'
#' @param records df
#' @param records.names character
#' 
#' @keywords internal
#' @noRd
.column_summary <- function(records, records.names, download_success = F){
  
  # remove internal columns
  gSD.cols <- grep("gSD", colnames(records))
  if(length(gSD.cols > 0)) records <- records[,-gSD.cols]
  diff.cols <- setdiff(colnames(records), records.names)
  if(length(diff.cols) > 0) out(paste0("Columns added to records: '", paste0(diff.cols, collapse = "', '"), "'"))
  
  if(isTRUE(download_success)){
    if(!is.null(records$download_success)){
      if(any(!records$download_success)){
        out(paste0("Some downloads have not been succesfull after ", max(records$download_attempts), " attempt(s) (see column 'download_success'). Please retry later."), type = 2)
      } else{
        out(paste0("All downloads have been succesfull after ", max(records$download_attempts), " attempt(s)."), msg = T)
      }
    }
  }
  return(records)
}


#' translate records column names to gSD standard
#' @param records df as returned by client
#' @param product_name product name 
#' @keywords internal
#' @noRd
.translate_records <- function(records, product_name){

  #"relativeorbit_number.1" "sensor_mode.1"          "level.1"
  # set-up column name dictionary
  records.names <- colnames(records)
  dict <- getOption("gSD.clients_dict")
  
  # translate
  records <- cbind(do.call(cbind, .gsd_compact(lapply(1:nrow(dict), function(i){
    x <- records[[dict$clients[i]]]
    if(!is.null(x)){
      x <- data.frame(x, stringsAsFactors = F)
      colnames(x) <- dict$gSD[i]
    }
    return(x)
  }))), records[,!sapply(records.names, function(x) x %in% dict$clients, USE.NAMES = F)])
  
  # colnames(records) <- sapply(colnames(records), function(x){
  #   i <- which(x == dict[,which.col])
  #   if(length(i) > 0) dict$gSD[i] else x
  # }, USE.NAMES = F)
  
  # specific cases: all EE products
  # if(which(which.col) > 2){
  #   records <- records[,-sapply(c("ordered", "bulkOrdered", "orderUrl", "dataAccessUrl", "downloadUrl", "cloudCover"), function(x) which(x == colnames(records)), USE.NAMES = F)]
  # }
  
  records <- records[,!sapply(colnames(records), function(x) x %in%  c("orderUrl", "bulkOrdered", "ordered", "product", "dataAccessUrl", "sceneBounds", "platformshortname", "mission",
    "hv_order_tileid", "level1cpdiidentifier", "datatakesensingstart", "s2datatakeid", "identifier", "gmlfootprint",
    "status", "filename", "format", "url", "downloadUrl", "mode", "productlevel"), USE.NAMES = F)]

  # product groups
  products <- get_products(grouped = T, update_online = F)
  records$product_group <- names(products)[sapply(products, function(x) any(grepl(product_name, x)))]
  records$product <- product_name
  
  # product-specfic cases
  if(unique(records$product_group) == "Sentinel"){
    records$date_acquisition <- sapply(strsplit(records$start_time, "T"), '[', 1)
    records$md5_url <- paste0(records$md5_url, "Checksum/Value/$value")
  }
  if(unique(records$product == "Sentinel-2")) records$tile_id[is.na(records$tile_id)] <- sapply(strsplit(records$record_id[is.na(records$tile_id)], "_"), function(x){
      gsub("T", "", x[nchar(x) == 6 & substr(x, 1, 1) == "T"])
  })
  
  # sort columns
  return(records)
}


# -------------------------------------------------------------
# data.frame utils
# -------------------------------------------------------------

#' 'unlists' all columns of a data.frame
#' @param records data.frame.
#' @param records data.frame with all columns unlisted
#' @keywords internal
#' @noRd
.unlist_df <- function(records) {
  for (i in 1:NCOL(records)) {
    column <- records[,i]
    # when sf we need one more [[1]] to reach
    not_matrix <- ifelse(inherits(records, SF()), !is.matrix(column[[1]][[1]]), !is.matrix(column[[1]][[1]][[1]]))
    if (inherits(column, LIST())) {
      # if it's a matrix it's a footprint thing
      if (not_matrix) {
        column <- unlist(column)
        if (!is.matrix(column) && length(column) == NROW(records)) {
          records[,i] <- unlist(column)
        }
      }
    }
  }
  return(records)
}

#' rbind different dfs
#' @param x list of dfs
#' @keywords internal
#' @noRd
rbind.different <- function(x) {
  
  if (.is_empty_array(x)) {
    return(x)
  } else {
    x.bind <- x[[1]]
    for(i in 2:length(x)){
      x.diff <- setdiff(colnames(x.bind), colnames(x[[i]]))
      y.diff <- setdiff(colnames(x[[i]]), colnames(x.bind))
      
      x.bind[, c(as.character(y.diff))] <- NA
      x[[i]][, c(as.character(x.diff))] <- NA
      
      x.bind <- rbind(x.bind, x[[i]])
    }
    return(x.bind)
  }
}

# -------------------------------------------------------------
# aoi area and coverage
# -------------------------------------------------------------

#' calculates area in aoi in km2
#' @param aoi aoi.
#' @return aoi_area numeric
#' @importFrom sf st_area st_as_sf
#' @keywords internal
#' @noRd
.calc_aoi_area <- function(aoi) {
  if (!.is_sf(aoi)) {
    aoi <- st_as_sf(aoi)
  }
  aoi_area <- st_area(aoi)
  if (length(aoi_area) > 1) { # in case of multipolygon
    aoi_area <- sum(aoi_area)
  }
  return(as.numeric(aoi_area) / 1000) # km2
  
}

#' calculates the number of cells of value 1 covering the aoi
#' @param x raster for which the percentage of value 1 in aoi shall be calculated.
#' @param aoi aoi.
#' @param aoi_ncell list of numerics if the needed values. If they have already been calculated they can
#' be provided here.
#' @return \code{percent} numeric percentage of value 1 covering the aoi
#' @importFrom raster ncell area getValues
#' @keywords internal
#' @noRd
.calc_aoi_coverage <- function(x, aoi, aoi_ncell = NULL) {
  
  if (is.null(aoi_ncell)) aoi_ncell <- .calc_aoi_corr_vals(aoi, x)
  x_vals <- getValues(x)
  # calc number of pixels with value 1
  x_valid <- length(x_vals[!is.na(x_vals)])
  
  # calculate percentage of pixels with value 1 in aoi
  percent <- (x_valid / aoi_ncell) * 100
  return(percent)
  
}

#' calculate aoi correction values for coverage calculation
#' @param aoi aoi.
#' @param x raster with the resolution.
#' @return integer number of pixels in aoi.
#' @importFrom sf st_bbox
#' @importFrom raster raster res crs values<-
#' @keywords internal
#' @noRd
.calc_aoi_corr_vals <- function(aoi, x) {
  # calculate aoi number of cells (calculation is suitable for large areas)
  e <- as.vector(st_bbox(aoi))
  # calculate area of aoi in order to get a suitable resolution for percentage cells computations
  r <- raster(xmn=e[1],xmx=e[3],ymn=e[2],ymx=e[4], crs=crs(x), resolution=res(x))
  values(r) <- as.integer(1)
  r <- .mask_raster_by_polygon(r, aoi)
  r_vals <- getValues(r)
  aoi_npixels <- length(which(r_vals == 1))
  return(aoi_npixels)
}

#' checks if records data.frame has SAR records (Sentinel-1) and if all records are SAR
#' @param products character vector of all products in records.
#' @return \code{has_SAR} numeric 1 for TRUE, 2 for FALSE, 100 for "all".
#' @keywords internal
#' @noRd
.has_SAR <- function(products) {
  
  sentinel1 <- name_product_sentinel1()
  if (sentinel1 %in% products) {
    has_SAR <- ifelse(all(products == sentinel1), 100, 1)
  } else {
    has_SAR <- 0
  }
  
}

#' creates a tileid where not given, except Sentinel-1
#' @param records data.frame.
#' @return \code{records} data.frame with a completely filled tile_id column.
#' @keywords internal
#' @noRd
.make_tileid <- function(records) {
  
  if(is.null(getElement(records, name_tile_id()))) records[name_tile_id()] <- NA_character_
  
  # first, try using the horizontal / vertical columns
  has_tileid <- !is.na(getElement(records, name_tile_id()))
  has_vertical <- if(!is.null(getElement(records, name_tile_number_vertical()))) !is.na(getElement(records, name_tile_number_vertical())) else rep(FALSE, nrow(records))
  has_horizontal <- if(!is.null(getElement(records, name_tile_number_horizontal()))) !is.na(getElement(records, name_tile_number_horizontal())) else rep(FALSE, nrow(records))
  
  sub <- !has_tileid & has_vertical & has_horizontal
  if(any(sub)){
    horizontal <- getElement(records[sub,], name_tile_number_horizontal())
    vertical <- getElement(records[sub,], name_tile_number_vertical())
    records[sub, name_tile_id()] <- paste0(horizontal, vertical)
  }
  
  # in many cases now value is given in horizontal / vertical
  # ensure that this is given in all cases, sensor-specifically
  records <- .make_tileid_sentinel1(records)
  records <- .make_tileid_sentinel2(records)
  records <- .make_tileid_sentinel3(records)
  records <- .make_tileid_landsat(records)
  records <- .make_tileid_modis(records)
  
  return(records)
}

#' creates tile ids for Sentinel-1 records from its footprints
#' @param records data.frame
#' @return records data.frame with added tile id of Sentinel-1 records
#' @keywords internal
#' @noRd
.make_tileid_sentinel1 <- function(records) {
  
  TILEID <- name_tile_id()
  RECORD_ID <- name_record_id()
  FOOTPRINT <- name_footprint()
  SENTINEL1 <- "S1"
  POINT_SEP <- "\\."

  record_ids <- records[[RECORD_ID]]
  is_sentinel1 <- intersect(which(!is.na(record_ids)), which(startsWith(record_ids, SENTINEL1)))
  if (!.is_empty_array(is_sentinel1)) {
    no_tileid <- is_sentinel1[is.na(records[is_sentinel1, TILEID])]
    footprints <- records[is_sentinel1, FOOTPRINT]
    if (!is.na(no_tileid) && !.is_empty_array(no_tileid) && !.is_empty_array(footprints)) {
      tileids <- sapply(footprints, function(footprint) {
        tryCatch({
          footprint <- footprint[[1]]#[[1]]
          horizontal <- strsplit(as.character(mean(footprint[,1][1:4])), POINT_SEP)[[1]]
          vertical <- strsplit(as.character(mean(footprint[,2][1:4])), POINT_SEP)[[1]]
          id <- paste0("h", horizontal[1], ".", substr(horizontal[2], 1, 1),
                       "v", vertical[1], ".", substr(vertical[2], 1, 1))
        }, error = function(err) {
          return(NA)
        }) 
      })
      records[no_tileid, TILEID] <- tileids
    }
  } 
  return(records)
}

#' creates tile ids for Sentinel-2 records
#' @param records data.frame
#' @return records data.frame with added tile id of Sentinel-2 records
#' @keywords internal
#' @noRd
.make_tileid_sentinel2 <- function(records) {
  
  RECORD_ID <- name_record_id()
  TILEID <- name_tile_id()
  SENTINEL2 <- "S2"
  
  # Sentinel-2
  record_ids <- records[[RECORD_ID]]
  is_sentinel2 <- intersect(which(!is.na(record_ids)), which(startsWith(record_ids, SENTINEL2)))
  if (!.is_empty_array(is_sentinel2)) {
    no_tileid <- is_sentinel2[is.na(records[is_sentinel2, TILEID])]
    if (!is.na(no_tileid) && !.is_empty_array(no_tileid)) {
      tileids <- sapply(records[no_tileid, RECORD_ID], function(x) {
        id <- strsplit(x, "_")[[1]][6]
      })
      records[no_tileid, TILEID] <- tileids
    }
  }
  return(records)
  
}

#' creates tile ids for Sentinel-3 records
#' @param records data.frame
#' @return records data.frame with added tile id of Sentinel-3 records
#' @keywords internal
#' @noRd
.make_tileid_sentinel3 <- function(records) {
  
  RECORD_ID <- name_record_id()
  TILEID <- name_tile_id()
  SENTINEL3 <- "S3"
  
  # Sentinel-3
  record_ids <- records[[RECORD_ID]]
  is_sentinel3 <- intersect(which(!is.na(record_ids)), which(startsWith(record_ids, SENTINEL3)))
  if (!.is_empty_array(is_sentinel3)) {
    no_tileid <- is_sentinel3[is.na(records[is_sentinel3, TILEID])]
    if (!is.na(no_tileid) && !.is_empty_array(no_tileid)) {
      tileids <- sapply(records[no_tileid, RECORD_ID], function(x){
        tryCatch({
          sep <- "______"
          if (grepl(sep, x)) { # does it contain the "_" separator at the end 
            splitted <- strsplit(x, sep)[[1]][1]
            splitted1 <- strsplit(splitted, "_")[[1]]
            len <- length(splitted1)
            last <- tail(splitted1, 1)
            if (grepl("[^0-9]", last)) { # should be chars
              id <- last
            } else { # only contains integers
              id <- paste0(splitted1[len-1], splitted1[len])
            }
          } else {
            splitted <- strsplit(x, "_LN1")[[1]]
            splitted1 <- strsplit(splitted, "_")[[1]]
            len <- length(splitted1)
            if (len == 18) {
              id <- paste0(splitted1[len-6], splitted1[len-5])
            } else {
              id <- paste0(splitted1[len-2], splitted1[len-1])
            }
          }
        }, error = function(err) {
          return(NA)
        })
      })
      records[no_tileid, TILEID] <- tileids
    }
  }
  return(records)
}

#' creates tile ids for Landsat records
#' @param records data.frame
#' @return records data.frame with added tile id of Landsat records
#' @keywords internal
#' @noRd
.make_tileid_landsat <- function(records) {
  
  RECORD_ID <- name_record_id()
  TILEID <- name_tile_id()
  LANDSAT <- name_product_group_landsat()
  
  record_ids <- records[[RECORD_ID]]
  is_landsat <- intersect(which(!is.na(record_ids)), which(startsWith(record_ids, LANDSAT)))
  if (!.is_empty_array(is_landsat)) {
    no_tileid <- is_landsat[is.na(records[is_landsat, TILEID])]
    if (!is.na(no_tileid) && !.is_empty_array(no_tileid)) {
      tileids <- sapply(records[no_tileid, RECORD_ID], function(x) {
        splitted <- strsplit(x, "_")[[1]][3]
        id <- paste0(substr(splitted, 1, 3), substr(splitted, 4, 7))
      })
      records[no_tileid, TILEID] <- tileids
    }
  }
  return(records)
  
}

#' creates tile ids for MODIS records
#' @param records data.frame
#' @return records data.frame with added tile id of MODIS records
#' @keywords internal
#' @noRd
.make_tileid_modis <- function(records) {
  
  RECORD_ID <- name_record_id()
  PRODUCT <- name_product()
  TILEID <- name_tile_id()
  MODIS <- name_product_group_modis()
  POINT_SEP <- "\\."
  h <- "h"
  v <- "v"
  
  product_names <- records[[PRODUCT]]
  is_modis <- intersect(which(!is.na(product_names)), which(startsWith(product_names, MODIS)))
  if (!.is_empty_array(is_modis)) {
    no_tileid <- is_modis[is.na(records[is_modis, TILEID])]
    if (!is.na(no_tileid) && !.is_empty_array(no_tileid)) {
      tileids <- sapply(records[no_tileid, RECORD_ID], function(x) {
        tryCatch({
          splitted <- strsplit(x, POINT_SEP)[[1]]
          splitted1 <- splitted[which(grepl(v, splitted) * grepl(h, splitted) == 1)]
          splitted2 <- strsplit(splitted1, v)[[1]]
          is_horizontal <- grepl(h, splitted2)
          id <- paste0(h, strsplit(splitted2[is_horizontal], h)[[1]][2], v, splitted2[!is_horizontal])
        }, error = function(err) {
          return(NA)
        })
      })
      records[no_tileid, TILEID] <- tileids
    }
  }
  return(records)
  
}


# -------------------------------------------------------------
# date utils
# -------------------------------------------------------------

#' returns the smallest and largest date of a character vector of dates.
#' @param dates character vector of dates ("2019-01-01").
#' @return \code{period} character vector of two dates
#' @keywords internal
#' @noRd
.identify_period <- function(dates) {
  dates_sorted <- sort(dates)
  period <- c(dates_sorted[1], tail(dates_sorted,1))
  return(period)
}

#' calculates the number of days between two dates
#' @param period character vector of start and end date.
#' @return \code{days} numeric number of days between.
#' @keywords internal
#' @noRd
.period_days <- function(period) {
  days <- as.integer(unclass(as.Date(period[2])) - unclass(as.Date(period[1])))
  return(days)
}

# -------------------------------------------------------------
# raster utils
# -------------------------------------------------------------

#' mask the edges of Landsat preview raster
#' @param preview raster.
#' @return \code{preview_masked} masked preview
#' @importFrom methods as slot slot<-
#' @importFrom raster crs extent crs<-
#' @keywords internal
#' @noRd
.landsat_preview_mask_edges <- function(preview) {
  polygons <- "polygons"
  COORDS_SLOT <- "coords"
  ext <- try(extent(preview))
  if (inherits(ext, TRY_ERROR())) return (preview)
  poly <- as(ext, SPATIAL_POLYGONS())
  crs(poly) <- crs(preview)
  # get the vertices of the extent and modify them
  coords <- slot(slot(slot(poly, polygons)[[1]], "Polygons")[[1]], COORDS_SLOT)
  coords[1,1] <- coords[1,1] + 0.08
  coords[2,1] <- coords[2,1] + 0.42
  coords[3,1] <- coords[3,1] - 0.08
  coords[4,1] <- coords[4,1] - 0.42
  coords[5,1] <- coords[5,1] + 0.08
  coords[1,2] <- coords[1,2] + 0.38
  coords[2,2] <- coords[2,2] - 0.06
  coords[3,2] <- coords[3,2] - 0.38
  coords[4,2] <- coords[4,2] + 0.05
  coords[5,2] <- coords[5,2] + 0.38
  slot(slot(slot(poly, polygons)[[1]], "Polygons")[[1]], COORDS_SLOT) <- coords
  preview_masked <- .mask_raster_by_polygon(preview, poly)
  extent(preview_masked) <- extent(c(coords[1,1], coords[3,1], coords[4,2], coords[2,2]))
  return(preview_masked)
}

#' mask the edges of Sentinel-2 preview raster
#' @param preview raster.
#' @return \code{preview_masked} masked preview
#' @importFrom methods as slot slot<-
#' @importFrom raster crs extent crs<-
#' @keywords internal
#' @noRd
.sentinel2_preview_mask_edges <- function(preview) {
  polygons <- "polygons"
  COORDS_SLOT <- "coords"
  ext <- try(extent(preview))
  if (inherits(ext, getSpatialData:::TRY_ERROR())) return (preview)
  poly <- as(ext, getSpatialData:::SPATIAL_POLYGONS())
  crs(poly) <- crs(preview)
  # get the vertices of the extent and modify them
  coords <- slot(slot(slot(poly, polygons)[[1]], "Polygons")[[1]], COORDS_SLOT)
  val <- 0.005
  coords[1,1] <- coords[1,1] + val
  coords[2,1] <- coords[2,1] + val
  coords[3,1] <- coords[3,1] - val
  coords[4,1] <- coords[4,1] - val
  coords[5,1] <- coords[5,1] + val
  coords[1,2] <- coords[1,2] + val
  coords[2,2] <- coords[2,2] - val
  coords[3,2] <- coords[3,2] - val
  coords[4,2] <- coords[4,2] + val
  coords[5,2] <- coords[5,2] + val
  slot(slot(slot(poly, polygons)[[1]], "Polygons")[[1]], COORDS_SLOT) <- coords
  preview_masked <- getSpatialData:::.mask_raster_by_polygon(preview, poly)
  extent(preview_masked) <- extent(c(coords[1,1], coords[3,1], coords[4,2], coords[2,2]))
  return(preview_masked)
} 

#' ensures min/max values of raster, stack or brick are between 0 and 255
#' @param x raster, stack or brick.
#' @return raster, stack or brick with all its values between 0 and 255.
#' @importFrom raster minValue maxValue
#' @keywords internal
#' @noRd
.ensure_minmax <- function(x) {
  max <- 255
  min_below_zero <- which(minValue(x) < 0)
  max_above_255 <- which(maxValue(x) > max)
  if (!.is_empty_array(min_below_zero)) {
    
  }
  if (!.is_empty_array(min_below_zero)) {
    for (i in min_below_zero) {
        x[[i]][x[[i]] < min] <- min
    }
  }
  if (!.is_empty_array(max_above_255)) {
    for (i in max_above_255) {
        x[[i]][x[[i]] > max] <- max
    }
  }
  return(x)
}

#' calculates percentage of a value in a raster or polygon with different modes.
#' @param x raster.
#' @param mode character specifies the mode of calculation. Mode "na" calculates
#' percentage of non-NA values in an aoi. Mode "custom" calculates the percentage
#' share of a specified value in the collection of two specified values. Mode "aoi"
#' is a special mode for aoi percentage calculation and only needed in special cases
#' where the number of values in aoi shall be provided through aoi_ncell. This option
#' exists to speedup the process especially when it is frequently done for a specific aoi.
#' It calculates the percentage of value 1 in the aoi.
#' @param custom numeric vector with two values: 
#' [1] are e.g. cloud values [2] are e.g. non-cloud values. Only if mode == "custom".
#' @param aoi aoi.
#' @param aoi_ncell integer number of cells in aoi.
#' @return \code{percent} numeric percentage
#' @keywords internal
#' @importFrom raster as.matrix extent res crs
#' @noRd
.raster_percent <- function(x, mode = "na", custom = c(), aoi = NULL, aoi_ncell = NULL) {
  
  if (mode == "na") {
    na_mask <- is.na(x)
    x <- .mask_raster_by_polygon(na_mask, aoi)
    x_mat <- as.integer(as.matrix(x))
    # clouds = 1 and clear = 0 here
    percent <- (length(which(x_mat == 1)) / length(which(!is.na(x_mat)))) * 100
  } else if (mode == "custom") {
    x_mat <- as.integer(as.matrix(x))
    val1 <- length(which(x_mat == custom[[1]]))
    val2 <- length(which(x_mat == custom[[2]]))
    percent <- (val1 / sum(val1, val2)) * 100
  } else if (mode == "aoi") {
    percent <- .calc_aoi_coverage(x, aoi, aoi_ncell)
  }
  # due to the calculation based on pixel values it might happen that percent 
  # exceeds 100 slightly. In these cases use 100
  percent <- ifelse(percent > 100, 100, percent)
  
}

#' aggregates rasters according to the aoi area size.
#' @param x character vector of paths to rasters to check on. All have to have
#' the same resolution.
#' @param x_names character vector of names refering to x.
#' @param aoi aoi.
#' @param factor numeric adjustment for aoi_area resulting in an adjustment 
#' @param dir_out character directory where to save adjusted rasters if necessary
#' @return x_adj or x (if nothing modified in data) characer vector of paths to (aggregated) rasters.
#' @importFrom raster raster aggregate writeRaster res dataType nlayers
#' @keywords internal
#' @noRd
.aggr_rasters <- function(x, x_names, aoi, factor = 750000, dir_out) {
  
  aoi_area <- .calc_aoi_area(aoi) # km2
  adj <- aoi_area / factor
  res_ref <- mean(res(raster(x[[1]]))) # check the resolution and modify adjustment according to it
  target_res <- 0.0019 * adj # the Sentinel-2 preview resolution * adj is the target res also for Landsat, MODIS
  # do not reduce more than the equivalent of double the Sentinel-2 preview resolution
  if (target_res > 0.0042) target_res <- 0.004 
  adj <- target_res / res_ref
  adj <- ifelse(adj < 2 && adj > 1, 2, adj)
  if (adj > 1) {
    x_adj <- sapply(1:length(x), function(i) {
      r_save_path <- file.path(dir_out,paste0(x_names[i],"_aggr.tif"))
      if (file.exists(r_save_path)) return(r_save_path)
      r_load <- stack(x[[i]])
      r_aggr <- aggregate(r_load, adj)
      writeRaster(r_aggr, r_save_path, overwrite=T, datatype=dataType(r_load))
      return(r_save_path)
    })
  } else {
    return(x)
  }
  return(x_adj)
  
}

#' create mosaic
#' @description The rasters from \code{x} will be mosaicked in a stupid way: everything is mosaicked that is in this list.
#' @param x list of paths to raster files.
#' @param save_path character, full path where to save the mosaic (has to end with '.tif').
#' @param mode character, optional. If mode == "rgb" no masking of the raster to only 1 values is done. 
#' If mode == "mask" the raster will be returned with 1 and NA. Default is mode == "mask".
#' @param srcnodata character nodata value in x. Default is for FLT4S.
#' @return \code{mos} raster mosaic
#' @keywords internal
#' @importFrom gdalUtils gdalbuildvrt
#' @noRd
.make_mosaic <- function(x, save_path, mode = "mask", 
                         srcnodata = NULL, datatype = NULL) {
  
  write_mos <- try(gdalbuildvrt(x,save_path,resolution="highest",
                                srcnodata=as.character(srcnodata),
                                vrtnodata="0",
                                seperate=F,overwrite=T,
                                datatype=datatype))

  if (inherits(write_mos, TRY_ERROR())) {
    return(NA)
  } else {
    mos <- raster(save_path)
    if (mode == "rgb") return(mos) else mos <- mos==1
  }
  
}

#' wrapper for masking raster by polygon
#' @param x RasterLayer, RasterStack or RasterBrick
#' @param mask sfc or sp
#' @return x masked
#' @importFrom raster mask
#' @importFrom sf as_Spatial
#' @keywords internal
#' @noRd
.mask_raster_by_polygon <- function(x, polygon) {
  if (!inherits(polygon, SPATIAL_POLYGONS())) {
    polygon <- as_Spatial(st_zm(polygon))
  }
  return(mask(x, polygon))
}

#' wrapper for cropping raster by polygon
#' @param x RasterLayer, RasterStack or RasterBrick
#' @param mask sfc or sp
#' @return x masked
#' @importFrom raster mask
#' @importFrom sf as_Spatial
#' @keywords internal
#' @noRd
.crop_raster_by_polygon <- function(x, polygon) {
  if (!inherits(polygon, SPATIAL_POLYGONS())) {
    polygon <- as_Spatial(polygon)
  }
  return(crop(x, polygon))
} 


#' calculates the normalized difference of two rasters
#' @param x RasterLayer
#' @param y RasterLayer
#' @return RasterLayer normalized difference of x and y
#' @keywords internal
#' @noRd
.normalized_difference <- function(x, y) {
  a <- x - y
  b <- x + y
  norm_diff <- a / b
  return(norm_diff)
}

#' rescales raster to 0-100
#' @param x RasterLayer
#' @return RasterLayer rescaled
#' @importFrom raster minValue maxValue
#' @keywords internal
#' @noRd
.rescale_raster <- function(x) {
  return((x - minValue(x)) / (maxValue(x) - minValue(x)) * 100)
}

#' mask NA-like DNs in previews (very low RGB). Only in case of Landsat, Sentinel-2 and Sentinel-3 OLCi
#' @param preview RasterLayer
#' @param record sf data.frame
#' @return na_mask RasterLayer
#' @importFrom raster mask
#' @keywords internal
#' @noRd
.create_preview_na_mask <- function(preview, record) {
  product_group <- record[[name_product_group()]]
  is_olci <- .record_is_olci(record)
  is_landsat_or_sentinel2 <- product_group %in% c(name_product_group_landsat(), name_product_group_sentinel())
  is_continental_s3 <- .record_is_s3_continental(record)
  if (any(is_olci, is_landsat_or_sentinel2, is_continental_s3)) {
    MIN_DN <- ifelse(is_landsat_or_sentinel2, 3, 0)
    # mask NA values in preview (considered as RGB DN < MIN_DN here)
    NA_mask <- ((preview[[1]] > MIN_DN) + (preview[[2]] > MIN_DN) + (preview[[3]] > MIN_DN)) >= 1
  } else {
    NA_mask <- preview[[1]] > -100
  }
  return(NA_mask)
}


# -------------------------------------------------------------
# miscellaneous
# -------------------------------------------------------------

#' add aoi to a mapview map
#' @param map a mapview object
#' @param aoi_colour colour of aoi
#' @param homebutton whether to show layer home buttons or not
#' @return nothing. runs expression
#' @keywords internal
#' @noRd
.add_aoi <- function(map = NULL, aoi_colour, homebutton = F){
  if(isFALSE(getOption("gSD.aoi_set"))){
    out("No AOI is displayed, since no AOI has been set yet (use 'set_aoi()' to define an AOI).", type = 2)
  } else{
    aoi.sf <- getOption("gSD.aoi")
    map.aoi <- mapview(aoi.sf, layer.name = "AOI", label = "AOI", lwd = 6, color = aoi_colour, fill = F, legend = F, homebutton = homebutton)
    if(!is.null(map)) return(map + map.aoi) else return(map.aoi)
  }
}

#' removes NULLs and NAs from list or data.frame.
#' @param x list or vector.
#' @return x list or vector without NULLs and NAs.
#' @keywords internal
#' @noRd
.gsd_compact <- function(x) {
  if (inherits(x, LIST()) || length(x) > 1) {
    not_na <- sapply(x, function(y) {return((!is.na(y) && !is.null(y)))})
    if (length(x) > 0) {
      x <- x[not_na]
    }
  }
  return(x)
}

#' returns TRUE if a vector or list has length 0/is.null() or is.na()
#' @param x vector/list of any type
#' @return logical
#' @keywords internal
#' @noRd
.is_empty_array <- function(x) {
  all_null <- sapply(x, function(element) {
    return(ifelse(inherits(element, LIST()), FALSE, is.null(element)))
  })
  all_na <- sapply(x, function(element) {
    return(ifelse(inherits(element, LIST()), FALSE, is.na(element)))
  })
  return(length(x) == 0 || is.null(x) || is.na(x) || all_null || all_na)
}

#' checks if a character can be integer
#' @param x character
#' @return logical
#' @keywords internal
#' @noRd
.char_can_be_int <- function(x) {
  return(!grepl("[^0-9]", x))
}

#' evaluate records footprints after csv read (they get wasted when writing to csv)
#' @param records sf data.frame
#' @param as_sf logical if records shall be returned as sf
#' @return records data.frame sf or data.frame
#' @importFrom sf st_multipolygon st_sfc
#' @keywords internal
#' @noRd
.eval_records_footprints <- function(records, as_sf = TRUE) {
  name_footprint <- name_footprint()
  footprints <- list()
  for (i in 1:NROW(records)) {
    record <- records[i,]
    f <- record[[name_footprint]][[1]]
    is_sfg <- inherits(f, "sfg")
    is_sfc <- inherits(f, "sfc")
    if (is_sfg) {
      footprints[[i]] <- f
    } else if (is_sfc) {
      footprints[[i]] <- f[[1]]
    } else {
      footprint_eval <- try(unlist(eval(parse(text = f))))
      if (inherits(footprint_eval, "try-error")) {
        out("Could not create footprint", type = 2)
        footprints[[i]] <- f
      } else {
        ncol <- 2
        nrow <- length(footprint_eval) / ncol
        m <- matrix(data = footprint_eval, nrow = nrow, ncol = ncol)
        footprints[[i]] <- st_multipolygon(list(list(m)))
      }
    }
  }
  # assign footprints
  records[[name_footprint]] <- st_sfc(footprints, crs = 4326)
  return(.check_records(records, as_df = !as_sf))
}

#' generate file name according to date time
#' @param name character suffix to be used after date time, e.g. 'records'.
#' @param extension character file format extension, e.g. '.tif'.
#' @param sep character separator between date, time and name. Default is '_'.
#' @return file_name character file name
#' @keywords internal
#' @noRd
.generate_datetime_filename <- function(name, extension = "", sep = "_") {
  return(paste(Sys.Date(), format(Sys.time(), "%Hh%Mm%Ss"), paste0(name, extension), sep = "_"))
}

#' generate records filename
#' @param file_name character file basename
#' @param dir_out character directory
#' @param driver character driver name as returned by st_drivers() plus "csv"
#' @keywords internal
#' @noRd
.generate_records_filename <- function(file_name = NULL, dir_out = NULL, driver = NULL) {
  if (is.null(driver)) driver <- "GeoJSON" # be able to provide a NULL driver through ... on higher level
  ext <- get_records_drivers()[[driver]]
  if (is.null(file_name)) {
    file_name <- .generate_datetime_filename("records", extension = ext)
  } else {
    file_name <- paste0(file_name, ext)
  }
  file <- file.path(dir_out, file_name)
  return(file)
}

#' sets the verbose option
#' @param verbose logical
#' @keywords internal
#' @noRd
.set_verbose <- function(verbose) {
  options("gSD.verbose" = verbose)
}

#' convert data.frame characters to their actual class (character, numeric, integer)
#' @param df (sf) data.frame
#' @keywords internal
#' @noRd
.uncharacter_dataframe <- function(df) {
  if (inherits(df, "data.frame")) {
    for (i in 1:NCOL(df)) {
      column <- df[[i]]
      column_new <- c()
      if (inherits(column, "character")) {
        for (j in 1:length(column)) {
          element <- column[j]
          if (is.null(element)) {
            column_new[j] <- element
          } else if (is.na(element)) {
            column_new[j] <- element
          } else if (is.na(suppressWarnings(try(as.numeric(element))))) {
            char <- as.character(element)
            if (char == "NA") char <- NA
            column_new[j] <- char
          } else {
            column_new[j] <- as.numeric(element)
          }
        }
        df[[i]] <- column_new
      }
    }
  }
  return(df)
}

#' create a string from date and time
#' @return character date time string
#' @keywords internal
#' @noRd
.create_datetime_string <- function() {
  return(gsub(":", "_", gsub(" ", "_", as.character(Sys.time()))))
}



#' retry the evaluation of an expression n times, if it fails, evaluate another expression
#' @param fun function to execute
#' @param ... arguments to fun
#' @param fail expression evaluated when fun fails, such as expression(stop("Failure"))
#' @param ini expression evaluated before caling fun
#' @param final expression evaluated after success
#' @param n number of retries
#' @param delay delay in seconds per retry
#' @param value whether to return x or not
#' @return value of test.epxr or nothing
#' @keywords internal
#' @noRd
.retry <- function(fun, ..., fail, ini = NULL, retry = NULL, final = NULL, n = 3, delay = 0, value = TRUE, verbose = T){
  
  # get ...
  extras <- list(...)
  if(length(extras) > 0) for(i in 1:length(extras)) assign(names(extras)[[i]], extras[[i]])
  
  # initial evaluation
  if(!is.null(ini)) eval(ini)
  
  # retry evluation
  while(n != 0){
    x <- try(fun(...), silent = T)
    if(inherits(x, "try-error")){
      
      # retry evluation
      if(grepl("aborted", as.character(attributes(x)$condition))) out("Operation was aborted by an application callback.", type = 3) else eval(retry)
      
      # fail evaluation
      n <- n-1
      if(n == 0) x <- eval(fail)
    } else n <- 0
    Sys.sleep(delay)
  }
  
  # final evaluation
  eval(final)
  if(isTRUE(value)) return(x)
}

#' verbose lapply
#'
#' @importFrom pbapply pblapply
#' @noRd 
.lapply <- function(X, FUN, ..., verbose = FALSE){
  if(isTRUE(verbose)) pblapply(X, FUN, ...) else lapply(X, FUN, ...)
}

#' verbose lapply
#'
#' @importFrom pbapply pblapply
#' @noRd 
.sapply <- function(X, FUN, ..., verbose = FALSE){
  if(isTRUE(verbose)) pbsapply(X, FUN, ...) else sapply(X, FUN, ...)
}

#' verbose apply
#'
#' @importFrom pbapply pbapply
#' @noRd 
.apply <- function(X, MARGIN, FUN, ..., verbose = FALSE){
  if(isTRUE(verbose)) pbapply(X, MARGIN, FUN, ...) else apply(X, MARGIN, FUN, ...)
}



#' On package startup
#' @importFrom pbapply pboptions
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname){
  
  pboptions(type = "timer", char = "=", txt.width = getOption("width")-30) # can be changed to "none"
  clients_dict <-  rbind.data.frame(c("summary", "product_group"), # place holder
                                    c("summary", "product"), # place holder
                                    c("title", "record_id"),
                                    c("displayId", "record_id"),
                                    c("uuid", "entity_id"),
                                    c("entityId", "entity_id"),
                                    c("summary", "summary"),
                                    c("beginposition", "date_acquisition"),
                                    c("acquisitionDate", "date_acquisition"),
                                    c("beginposition", "start_time"),
                                    c("StartTime", "start_time"),
                                    c("AcquisitionStartDate", "start_time"),
                                    c("endposition", "stop_time"),
                                    c("StopTime", "stop_time"),
                                    c("AcquisitionEndDate", "stop_time"),
                                    c("ingestiondate", "date_ingestion"),
                                    c("modifiedDate", "date_modified"),
                                    c("creationdate", "date_creation"),
                                    c("footprint", "footprint"),
                                    c("tileid", "tile_id"),
                                    c("WRSPath", "tile_number_horizontal"),
                                    c("WRSRow", "tile_number_vertical"),
                                    c("HorizontalTileNumber", "tile_number_horizontal"),
                                    c("VerticalTileNumber", "tile_number_vertical"),
                                    c("url.alt", "md5_url"),
                                    c("browseUrl", "preview_url"),
                                    c("url.icon", "preview_url"),
                                    c("metadataUrl", "meta_url"),
                                    c("fgdcMetadataUrl", "meta_url_fgdc"),
                                    c("slicenumber", "slice_number"),
                                    c("orbitnumber", "orbit_number"),
                                    c("orbitdirection", "orbit_direction"),
                                    c("lastorbitnumber", "lastorbit_number"),
                                    c("relativeorbitnumber", "relativeorbit_number"),
                                    c("lastrelativeorbitnumber", "lastrelativeorbit_number"),
                                    c("passnumber", "pass_number"),
                                    c("passdirection", "pass_direction"),
                                    c("relorbitdir", "relativeorbit_direction"),
                                    c("relpassnumber", "relativepass_number"),
                                    c("relpassdirection", "relativepass_direction"),
                                    c("lastorbitdirection", "lastorbit_direction"),
                                    c("lastpassnumber", "lastpass_number"),
                                    c("lastpassdirection", "lastpass_direction"),
                                    c("lastrelorbitdirection", "lastrelativeorbit_direction"),
                                    c("lastrelpassnumber", "lastrelativepass_number"),
                                    c("lastrelpassdirection", "lastrelativepass_direction"),
                                    c("swathidentifier", "swath_id"),
                                    c("producttype", "product_type"),
                                    c("productclass", "product_class"),
                                    c("productconsolidation", "product_consolidation"),
                                    c("timeliness", "timeliness"),
                                    c("platformname", "platform"),
                                    c("platformidentifier", "platform_id"),
                                    c("platformserialidentifier", "platform_serial"),
                                    c("instrumentname", "sensor"),
                                    c("instrumentshortname", "sensor_id"),
                                    c("SensorIdentifier", "sensor_id"),
                                    c("sensoroperationalmode", "sensor_mode"),
                                    c("polarisationmode", "polarisation_mode"),
                                    c("acquisitiontype", "aquistion_type"),
                                    c("size", "size"),
                                    c("is_gnss", "is_gnss"),
                                    c("LandCloudCover", "cloudcov_land"),
                                    c("SceneCloudCover", "cloudcov"),
                                    c("cloudCover", "cloudcov"),
                                    c("cloudcoverpercentage", "cloudcov"),
                                    c("highprobacloudspercentage", "cloudcov_highprob"),
                                    c("mediumprobacloudspercentage", "cloudcov_mediumprob"),
                                    c("notvegetatedpercentage", "cloudcov_notvegetated"),
                                    c("snowicepercentage", "snowice"),
                                    c("unclassifiedpercentage", "unclassified"),
                                    c("vegetationpercentage", "vegetation"),
                                    c("waterpercentage", "water"),
                                    c("processinglevel", "level"),
                                    c("level", "level"),
                                    c("AutoQualityFlag", "flag_autoquality"),
                                    c("AutoQualityFlagExplanation", "flag_autoquality_expl"),
                                    c("ScienceQualityFlag", "flag_sciencequality"),
                                    c("ScienceQualityFlagExpln", "flag_sciencequality_expl"),
                                    c("MissingDataPercentage", "missingdata"), stringsAsFactors = F)
  colnames(clients_dict) <- c("clients", "gSD")
  
  op <- options()
  op.gSD <- list(
    gSD.api = list(dhus = 'https://scihub.copernicus.eu/dhus/',
                   #s3 = 'https://scihub.copernicus.eu/s3/',
                   s5p = 'https://s5phub.copernicus.eu/',
                   gnss = 'https://scihub.copernicus.eu/gnss/',
                   espa = 'https://espa.cr.usgs.gov/api/v1/',
                   ee = 'https://earthexplorer.usgs.gov/inventory/json/v/1.4.0/',
                   aws.l8 = 'https://landsat-pds.s3.amazonaws.com/c1/L8/',
                   aws.l8.sl = 'https://landsat-pds.s3.amazonaws.com/c1/L8/scene_list.gz',
                   laads = 'https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/'),
    gSD.api.names = list(dhus = "ESA Copernicus Open Hub",
                         #s3 = "ESA Copernicus S3 Hub",
                         s5p = "ESA Copernicus S5P Hub",
                         gnss = "ESA Copernicus GNSS Hub",
                         espa = "USGS-EROS ESPA",
                         ee = "USGS EarthExplorer",
                         aws.l8 = "AWS Landsat 8",
                         laads = "NASA DAAC LAADS"),
    gSD.copnames = data.frame(name = c("Sentinel-1", "Sentinel-2", "Sentinel-3", "Sentinel-5P", "GNSS"),
                              api = c("dhus", "dhus", "dhus", "s5p", "gnss"), stringsAsFactors = F),
    gSD.sen2cor = list(win = "http://step.esa.int/thirdparties/sen2cor/2.5.5/Sen2Cor-02.05.05-win64.zip",
                       linux = "http://step.esa.int/thirdparties/sen2cor/2.5.5/Sen2Cor-02.05.05-Linux64.run",
                       mac = "http://step.esa.int/thirdparties/sen2cor/2.5.5/Sen2Cor-02.05.05-Darwin64.run"),
    gSD.verbose = FALSE,
    gSD.dhus_session = NULL,
    gSD.dhus_user = FALSE,
    gSD.dhus_pass = FALSE,
    gSD.dhus_set = FALSE,
    gSD.dhus_time = NULL,
    gSD.usgs_user = FALSE,
    gSD.usgs_pass = FALSE,
    gSD.usgs_set = FALSE,
    gSD.usgs_time = NULL,
    gSD.usgs_refresh = 60, # minutes
    gSD.usgs_apikey = FALSE,
    gSD.ed_user = FALSE,
    gSD.ed_pass = FALSE,
    gSD.ed_set = FALSE,
    gSD.ed_time = NULL,
    gSD.ed_refresh = 60, # minutes
    gSD.archive = FALSE,
    gSD.archive_set = FALSE,
    gSD.aoi = FALSE,
    gSD.aoi_set = FALSE,
    gSD.products = NULL,
    gSD.clients_dict = clients_dict
  )
  toset <- !(names(op.gSD) %in% names(op))
  if(any(toset)) options(op.gSD[toset])
  
  ## allocate gdal on load
  #gdalUtils::gdal_setInstallation(rescan = T)
  
  invisible()
}

#' On package unload (logouts)
#' @keywords internal
#' @noRd
.onUnload <- function(libname, pkgname) {
  
  ## logout from USGS
  if(isTRUE(getOption("gSD.usgs_set"))) .ERS_logout(getOption("gSD.usgs_apikey"))
}