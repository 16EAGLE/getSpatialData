#' ---------------------------------------------------------------------
#' @name internal_select
#' @description This document is the backend of select functionalities. 
#' @details It contains select_ specific methods. Rather generic methods that
#' might be useful for other package-internal functionalities are situated
#' in internal. Checks are in checks. The frontends of select_ are located in dedicated select_ functions.
#' @author Henrik Fisser, 2019
#' @keywords internal
#' ---------------------------------------------------------------------

#### SELECT PREP

#' checks if a record is supported by .select_*() or not
#' @param record with one row
#' @return \code{is_supported} logical
#' @keywords internal
#' @noRd
.select_supported <- function(record) {
  if (record$product == "Sentinel-1") {
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
#' @keywords internal
#' @noRd
.select_params <- function(records, mode) {
  
  modes <- list("TS"="timeseries","BT"="bitemporal","UT"="unitemporal")
  params <- list(selected_col=paste0("selected_for_",modes[[mode]]), # logical column if a record is selected at all
                 pmos_col="rgb_mosaic_file", # path to the RGB mosaic tif where record is included
                 cmos_col="cmask_mosaic_file", # path to the cloud mask mosaic tif where record is included
                 timestamp_col="selected_for_timestamp", # for the timestamp number for which the record is selected
                 aoi_cc_col="aoi_HOT_cloudcov_percent",
                 tileid_col="tile_id",
                 preview_col="preview_file",
                 cloud_mask_col="cloud_mask_file",
                 date_col="date_acquisition",
                 identifier="record_id",
                 sub_period_col="sub_period")
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
  
  records[[params$date_col]] <- sapply(records[[params$date_col]],as.character)
  period <- .identify_period(records[[params$date_col]])
  # calculates the sub_period column
  records <- .select_sub_periods(records,period,num_timestamps,params$date_col)
  # check which records in records are supported by select and mark unsupported records with NA in 'sub_period'
  supported <- sapply(1:NROW(records), function(i) .select_supported(records[i,]))
  records[!supported, params$sub_period_col] <- NA
  
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
  
  records <- .unlist_df(records)
  records <- .make_tileid(records)
  params <- .select_params(records,mode)
  params$period <- .identify_period(records[[params$date_col]])
  records <- .select_prep(records,num_timestamps,params)
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
    val <- ifelse(col == selected_col,FALSE,NA)
    records[[col]] <- val
  }
  return(records)
  
}

#### SELECT MOSAICKING FUNCTIONS

#' bridge to .make_mosaic
#' @param paths character paths to rasters to be mosaicked.
#' @param aoi aoi.
#' @param save_path save_path (should end with '.tif').
#' @param mode character mode can be "rgb" or "mask" as in .make_mosaic.
#' @return mos_base_crop.
#' @importFrom raster mask crop raster dataType
#' @keywords internal
#' @noRd
.select_bridge_mosaic <- function(paths, aoi, save_path, mode = "mask") {
  
  tmp_load <- raster(paths[1])
  src_datatype <- dataType(tmp_load)
  srcnodata <- ifelse(src_datatype == "INT2S","-32768","-3.3999999521443642e+38")
  mos_base <- .make_mosaic(paths,save_path,mode=mode,srcnodata=srcnodata,datatype=src_datatype)
  if (class(mos_base) != "RasterLayer") {
    return(NA)
  } else {
    
    mos_base_mask <- mask(mos_base,aoi)
    mos_base_crop <- crop(mos_base_mask,aoi)
    writeRaster(mos_base_crop,save_path,overwrite=T,
                srcnodata=srcnodata,datatype=src_datatype)
    
    return(mos_base_crop)
  }
  
}

#' creates a mosaic of all used cloud masks and writes it as .tif
#' @param s list 'selected' of a timestamp holding everything inserted in select_*().
#' @param aoi aoi.
#' @param dir_out character directory.
#' @return \code{save_path_cmos} character path where cloud mask mosaic is saved
#' @importFrom raster tmpDir
#' @keywords internal
#' @noRd
.select_cmask_mos <- function(s, aoi, dir_out) {
  
  vec <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n")
  save_str <- paste0(sample(vec,10),collapse = "")
  save_path_cmos <- file.path(dir_out,paste0(save_str,"cloud_mask_mosaic_timestamp",s$timestamp,".tif"))
  cMask_mosaic <- .select_bridge_mosaic(s$cMask_paths,aoi,save_path_cmos)
  .delete_tmp_files(raster::tmpDir())
  return(save_path_cmos)
  
}

#' creates a cloud-masked preview RGB mosaic and writes it as .tif
#' @param records data.frame.
#' @param s list 'selected' of a timestamp holding everything inserted in select_*().
#' @param aoi aoi.
#' @param i integer index in the loop.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @param dir_out character directory below which to save intermediate product in tmp.
#' @param cloud_mask_col character name of cloud mask path column.
#' @param preview_col character name of the preview path column.
#' @param sensors_given character vector of the product_group given in records.
#' @return \code{save_pmos_final} character path where preview RGB mosaic is saved
#' @importFrom raster writeRaster stack mask tmpDir
#' @keywords internal
#' @noRd
#' @author Henrik Fisser
.select_preview_mos <- function(records, s, aoi, i, identifier, dir_out, 
                                cloud_mask_col, preview_col, sensors_given) {
  
  tmp_dir_orig <- base::tempdir()
  tmp_dir <- .tmp_dir(dir_out,action=1,TRUE)
  
  r <- "RasterLayer"
  wgs_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  id_sel <- sapply(s$ids,function(x) which(records[[identifier]]==x))
  vec <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","")
  save_str <- paste0(sample(vec,10),"_",collapse = "")
  save_pmos <- file.path(tmp_dir,paste0(save_str,"preview_mosaic_timestamp",s$timestamp))
  layers <- c("red","green","blue")
  
  # disaggregation adjustment parameters
  #sensors_ref <- c("Sentinel-2","Sentinel-3")
  #sensors_adj <- c("Landsat","MODIS")
  #disaggr_needed <- any(sensors_given %in% sensors_ref) && any(sensors_given %in% sensors_adj)
  #if (disaggr_needed) y_sensor <- sensors_ref[which(sensors_given %in% sensors_ref)[1]]
  
  # cloud mask previews band-wise
  # this returns a list of paths to each of the three cloud-masked bands per record
  preview_paths <- lapply(id_sel,function(id_index) {
    
    p_path <- records[id_index,preview_col]
    if (is.na(p_path) || !file.exists(p_path)) return(NA)
    cMask <- raster(records[id_index,cloud_mask_col]) # cloud mask
    preview <- stack(p_path)
    if (is.na(crs(cMask))) crs(cMask) <- wgs_crs
    if (is.na(crs(preview))) crs(preview) <- wgs_crs
    
    #product_group_given <- records[id_index,"product_group"]
    #do_disaggregation <- product_group_given %in% sensors_adj && disaggr_needed
    #if (do_disaggregation) {
    # disaggregate preview to the resolution of other sensor in records that has higher resolution
    #  preview <- try(.disaggr_raster(preview,x_sensor=product_group_given,y_sensor=y_sensor))
    #}
    
    preview_cloud_masked <- preview * cMask
    preview_save <- file.path(tmp_dir,paste0(records[id_index,identifier],"_cmasked"))
    
    paths_sep <- sapply(1:nlayers(preview_cloud_masked),function(j) {
      layer_save <- paste0(preview_save,"_",id_index,"_",layers[j],".tif")
      writeRaster(preview_cloud_masked[[j]],layer_save,overwrite=T)
      return(layer_save)
    })
    
  })
  
  preview_paths <- .gsd_compact(preview_paths)
  
  # create the mosaic band-wise
  # this returns a list of band-wise mosaics
  preview_mos <- lapply(1:length(layers),function(j) {
    curr_layers <- lapply(preview_paths,function(x) path <- x[j])
    save_path_pmos <- paste0(save_pmos,"_",layers[j],".grd")
    pmos <- try(.select_bridge_mosaic(unlist(curr_layers),aoi,save_path_pmos,mode="rgb"))
    if (class(pmos) != r) return(NA) else return(pmos)
  })
  
  if (any(sapply(preview_mos,class) != r)) out("Could not create preview RGB mosaic",2)
  
  # stack all bands and write to file
  preview_mos_stack <- stack(preview_mos)
  save_pmos_final <- file.path(dir_out,paste0("rgb_preview_mosaic_ts",i,".tif"))
  writeRaster(preview_mos_stack,save_pmos_final,overwrite=T)
  
  # cleanup
  .delete_tmp_files(tmp_dir)
  .tmp_dir(dir_out,action=2,TRUE,tmp_dir_orig)
  
  return(save_pmos_final)
  
}

#' create mosaic consecutively in the order of ordered records (according to aoi cloud cover)
#' Important: the cloud masks have to have NA where clouds or no data.
#' @param records data.frame that contains all records within the sub-period but will be subsetted to \code{sub}.
#' @param base_records character vector of paths to cloud masks that create a base mosaic.
#' @param aoi aoi.
#' @param sub list of numeric vectors. Each vector represents one tile id and indexes records in \code{records}.
#' @param cloud_mask_col character name of cloud mask path column.
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' The value is the percentage of not yet covered area that shall be covered additionally when adding the record.
#' @param satisfaction_value numeric.
#' @param ts numeric of the current timestamp.
#' @param dir_out character directory where to save intermediate product.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @param delete_files logical TRUE if all files in tmp_dir shall be deleted at the end of the call.
#' If it is a selection for multiple sensors this should be FALSE as long there is a next sensor following.
#' @return selected list of [[1]] character ids of selected records, [[2]] percentage of valid pixels in mosaic.
#' Side effect: creates a tmp_dir, writes into it, deletes tmp_dir with all files.
#' @importFrom raster minValue maxValue writeRaster raster crs crs<- dataType intersect
#' @keywords internal
#' @noRd
#' @author Henrik Fisser
.select_calc_mosaic <- function(records, base_records, aoi, sub_within, cloud_mask_col, 
                                min_improvement, satisfaction_value, 
                                ts, dir_out, identifier, delete_files) {
  
  tmp_dir_orig <- tempdir()
  tmp_dir <- .tmp_dir(dir_out,1,TRUE)
  
  if (is.null(base_records)) out("Current coverage of valid pixels..")
  
  r <- "RasterLayer"
  curr_sensor <- unique(records$product)
  if (!class(aoi)[1] %in% c("SpatialPolygons","SpatialPolygonsDataFrame")) aoi <- as(aoi,"Spatial")
  le_first_order <- length(sub_within[[1]])
  
  if (length(sub_within) > 1) {
    # sort the orders of sub_within according to the aoi cloud cover of the same tile in previous order
    # this way for each order those tiles will be checked first that had the highest cloud cover in previous order
    # it shall result in handling large gaps first and small gaps late
    tile_mirror <- sapply(sub_within,function(order) return(sapply(order,function(i) return(records[i,"tile_id"]))))
    cc_col <- "aoi_HOT_cloudcov_percent"
    sub_within_sorted <- append(sub_within[1],lapply(2:length(sub_within),function(i) {
      curr_tiles <- tile_mirror[[i]]
      prev_tiles <- tile_mirror[[i-1]]
      tile_in_previous <- which(curr_tiles %in% prev_tiles)
      cc_previous <- records[sub_within[[i-1]][which(prev_tiles %in% curr_tiles)],cc_col] # cloud cover of records of previous order
      sorted_order <- sub_within[[i]][tile_in_previous][order(cc_previous,decreasing=TRUE)]
      return(sorted_order)
    }))
    
    sub_within <- unlist(.gsd_compact(sub_within_sorted)) # vector of integer indices
  }
  
  # get paths to cloud masks. This is the queue for processing
  collection <- sapply(sub_within,function(x) cMask_path <- as.character(records[[cloud_mask_col]][x])) 
  # this vector are all orders ordered from best records (lowest aoi cc) 
  # to worst records (highest aoi cc) in a queue and respecting the tile order
  names(collection) <- sapply(sub_within,function(x) return(records[x,identifier]))
  collection <- collection[intersect(which(collection != "NONE"),which(!is.na(collection)))]
  le_collection <- length(collection)
  
  # create the first base mosaic from the first order of collection (best records per tile)
  # if base_records are given through arguments these are records selected for a prio_sensor
  # that create the base mosaic
  if (is.null(base_records)) {
    # paths of first record of each tile
    base_records <- collection[1:le_first_order]
    start <- le_first_order + 1 # if base mosaic is the first order skip it during further mosaic
  } else {
    start <- 1 # if base mosaic is the mosaic of a prio sensor process all of this sensor
  }
  
  # if the base mosaics contains already all available records
  start <- ifelse(start > le_collection,le_collection,start)
  
  # aggregate raster adjusted to aoi area size in order to speed up process
  names <- names(base_records)
  base_records <- .aggr_rasters(base_records,names,aoi=aoi,dir_out=tmp_dir)
  names(base_records) <- names
  # this base mosaic will be updated with each added record after check if valid cover is increased
  base_mos_path <- normalizePath(file.path(tmp_dir,"base_mosaic_tmp.tif"))
  base_mos <- .select_bridge_mosaic(base_records,aoi,base_mos_path)
  if (class(base_mos) != r) start <- 1
  # cleanup
  .delete_tmp_files(tmp_dir) # delete temp raster .grd files in r tmp
  rm(base_mos)
  base_coverage <- -1000
  n_pixel_aoi <- .calc_aoi_corr_vals(aoi,raster(base_records[1])) # correction values for coverage calc
  
  # add next cloud mask consecutively and check if it decreases the cloud coverage
  for (i in start:le_collection) {
    
    x <- collection[i] # current cloud mask
    base_mos <- raster(base_mos_path) # current base mosaic
    
    if (i == start) {
      is_last_record <- i == le_collection
      base_coverage <- .raster_percent(base_mos,mode="aoi",aoi=aoi,n_pixel_aoi)
      .select_out_cov(base_coverage,ifelse(is_last_record,i,i-1),le_collection,curr_sensor)
      if (is_last_record) break
    }
    
    name_x <- names(x)
    # before calculating the next mosaic, 
    # check if record tile is within the area of non-covered pixels at all
    x <- .aggr_rasters(x,name_x,aoi=aoi,dir_out=tmp_dir)
    names(x) <- name_x
    next_record <- raster(x) # record to be added if it supports the mosaic
    curr_base_mos_crop <- crop(base_mos,next_record) # crop base mosaic to tile area of next
    aoi_subset <- as(extent(next_record),"SpatialPolygons")
    crs(aoi_subset) <- crs(next_record)
    aoi_subset <- intersect(aoi_subset,aoi)
    cov_init <- .raster_percent(curr_base_mos_crop,mode="aoi",aoi=aoi_subset,n_pixel_aoi)
    
    if (round(cov_init) == 99) {
      add_it <- FALSE
    } else {
      crop_p <- file.path(tmp_dir,"crop_tmp.tif")
      curr_mos_tmp_p <- normalizePath(file.path(tmp_dir,"curr_crop_mos_tmp.tif"))
      writeRaster(curr_base_mos_crop,crop_p,overwrite=T,datatype=dataType(base_mos))
      curr_mos_tmp <- .select_bridge_mosaic(c(crop_p,x),aoi,curr_mos_tmp_p) # in this tile add next_record
      
      if (class(curr_mos_tmp) != r) {
        add_it <- FALSE
      } else {
        cov_aft <- .raster_percent(curr_mos_tmp,mode="aoi",aoi=aoi_subset) # check new coverage
        # cleanup
        .delete_tmp_files(tmp_dir)
        unlink(curr_mos_tmp_p)
        rm(next_record,curr_base_mos_crop,curr_mos_tmp,base_mos)
        
        # calculate if valid coverage in whole aoi is improved > min_improvement 
        # when adding the record to the tile area
        if (((cov_init + (cov_init / 100) * min_improvement)) > 100) {
          add_it <- cov_aft > cov_init && cov_init < 99
        } else {
          add_it <- .select_exceeds_improvement(min_improvement,cov_init,cov_aft)
        }
      }
    }
    
    if (add_it) {
      save_str <- "base_mos_tmp_"
      curr <- paste0(save_str,i,".tif")
      base_mos_path_tmp <- normalizePath(file.path(tmp_dir,curr))
      base_records <- c(base_records,x) # add save path of current mosaic
      base_mos <- .select_bridge_mosaic(base_records,aoi,base_mos_path_tmp) # mosaic with added record
      
      if (class(base_mos) == r) {
        # cleanup
        rm(base_mos)
        # delete previous base_mos_tmp tifs
        base_mos_tmp_files <- list.files(tmp_dir,pattern=save_str)
        base_mos_tmp_files <- base_mos_tmp_files[which(base_mos_tmp_files!=curr)]
        del <- sapply(base_mos_tmp_files,function(del_file) {
          del_path <- file.path(tmp_dir,del_file)
          if (file.exists(del_path) && del_file != curr) unlink(del_path)
        })
        base_mos <- raster(base_mos_path_tmp)
        base_coverage <- .raster_percent(base_mos,mode="aoi",aoi=aoi,n_pixel_aoi)
        base_mos_path <- normalizePath(base_mos_path_tmp)
        # cleanup
        rm(base_mos)
        .delete_tmp_files(tmp_dir) # delete temp raster grd files in r tmp
        # coverage console update
        .select_out_cov(base_coverage,i,le_collection,curr_sensor)
      }
    }
    if (base_coverage >= satisfaction_value) {
      break
    }
  }
  
  # return ids of selected records and percentage of valid pixels of final mosaic
  selected <- list(ids=names(base_records),
                   cMask_paths=records[which(records[[identifier]] %in% names(base_records)),
                                       cloud_mask_col],
                   valid_pixels=base_coverage)
  
  if (delete_files) {
    .tmp_dir(dir_out,2,TRUE,tmp_dir_orig)
  }
  
  return(selected)
}

#' calculates the final cloud mask and preview RGB mosaic per timestamp
#' @param records data.frame.
#' @param selected list of lists, each of the list is one timestamp and holds the ids, timestamp numbers and cloud mask paths.
#' @param aoi aoi.
#' @param params list.
#' @param dir_out character directory.
#' @return records with four additional columns: selected_col, timestamp_col, pmos_col, cmos_col. Cloud mask and preview mosaics are saved in dir_out.
#' @keywords internal
#' @noRd
#' @author Henrik Fisser
.select_save_mosaics <- function(records, selected, aoi, 
                                 params, dir_out) {
  
  console_info <- list()
  cols <- c(params$selected_col,params$timestamp_col,params$pmos_col,params$cmos_col)
  records <- .select_prep_cols(records,cols,params$selected_col)
  
  for (i in 1:length(selected)) {
    
    s <- selected[[i]]
    id_sel <- s$ids
    #A cloud mask mosaic
    save_path_cmos <- .select_cmask_mos(s,aoi,dir_out)
    #B preview mosaic
    save_path_pmos <- .select_preview_mos(records,s,aoi,i,params$identifier,dir_out,
                                          cloud_mask_col=params$cloud_mask_col,
                                          preview_col=params$preview_col,
                                          sensors_given=params$product_group)
    #C add columns to records
    insert <- c(TRUE,s$timestamp,save_path_pmos,save_path_cmos)
    for (j in 1:length(cols)) {
      records[which(records[[params$identifier]] %in% id_sel),cols[j]] <- insert[j]      
    }
    # get print info
    console_info[[i]] <- .select_final_info(s)
    
  }
  each_timestamp <- .out_vector(console_info)
  return(records)
  
}

#### SELECT PROCESSES

#' select main process
#' @param records data.frame.
#' @param aoi aoi.
#' @param has_SAR numeric vector indicating if and how much SAR is in records.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps. 
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param satisfaction_value numeric.
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @param params list holding everything inserted into this parameter list in .select_params().
#' @param dir_out character directory where to save intermediate product.
#' @param cols_initial character vector of records column names as input from user.
#' @return records data.frame ready for return to user.
#' @keywords internal
#' @noRd
#' @author Henrik Fisser
.select_main <- function(records,
                         aoi,
                         has_SAR,
                         num_timestamps,
                         min_distance,
                         min_improvement,
                         max_sub_period,
                         max_cloudcov_tile,
                         satisfaction_value,
                         prio_sensors,
                         dir_out,
                         params,
                         cols_initial) {
  
  # if all are SAR records
  if (has_SAR == 100) {
    records <- .select_all_SAR(records, max_sub_period,
                               min_distance, num_timestamps, params)
    records <- .column_summary(records,cols_initial)
    return(records)
  }
  
  #### Start Process for optical data selection
  selected <- list() # list to be filled by all selected 'record_id' ids, the valid coverage percentage per timestamp and the cloud mask paths
  sub_periods <- unique(na.omit(records$sub_period))
  
  # select per sub-period (=timestamp) best mosaic. The sub-periods are adjusted dynamically according to min_distance, max_sub_period
  for (t in 1:length(sub_periods)) {
    
    previous_period <- ifelse(t > 1,selected[[t-1]]$period,NA)
    
    selected_ts <- try(.select_process(records,
                                       aoi,
                                       timestamp=t,
                                       min_distance=min_distance, 
                                       max_sub_period=max_sub_period,
                                       max_cloudcov_tile=max_cloudcov_tile, 
                                       min_improvement=min_improvement,
                                       previous_period=previous_period,
                                       satisfaction_value=satisfaction_value,
                                       prio_sensors=prio_sensors,
                                       params=params,
                                       dir_out=dir_out))
    
    if (inherits(selected_ts,"try-error")) {
      out("\nSelection failed for timestamp: ",t)
    }
    selected_ts[["timestamp"]] <- t
    selected[[t]] <- selected_ts # insert 'selected_ts' list into selected list
    
  }
  
  # if some are SAR records
  if (has_SAR == 1) {
    selected <- .select_some_SAR(records, selected, max_sub_period,
                                 min_distance, num_timestamps, params)
  }
  
  #A Create and save final cloud mask mosaic
  #B Create and save final RGB preview mosaic
  #C Add 3 columns to records data.frame:
  #1 logical column if a record is selected at all
  #2 path to the RGB mosaic tif where record is included
  #3 the timestamp number for which the record is selected
  mode_console <- ifelse(length(selected)==1,""," per timestamp")
  sep <- params$sep
  out(paste0(sep,"\nSelection Process Summary",
             mode_console))
  # create final mosaics for each timestamp and summary message per timestamp
  records <- .select_save_mosaics(records,selected=selected,aoi=aoi,
                                  params=params,dir_out=dir_out)
  # create optional warning(s) and overall summary message
  csw <- .select_summary_ts(selected)
  w <- csw[2:3] # warnings
  w <- w[which(w!="NULL")]
  summary <- .out_vector(csw[[1]])
  if (length(w) > 0) to_console <- sapply(w,function(x) .out_vector(x,type=2))
  
  records <- subset(records,select=-sub_period) # remove sub-period column
  
  records <- .column_summary(records,cols_initial)
  
  return(records)
  
}

#' selection process
#' @param records data.frame.
#' @param aoi aoi.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' @param previous_period character vector selected period of previous timestamp.
#' @param satisfaction_value numeric.
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @param params list holding everything inserted into this parameter list in .select_params().
#' @param dir_out character directory where to save intermediate product.
#' @param timestamp numeric of the current timestamp.
#' @return \code{selected} list of selected records with all items returned by .select_process_sub
#' @keywords internal
#' @noRd
.select_process <- function(records, aoi,
                            timestamp,
                            min_distance, max_sub_period, max_cloudcov_tile, 
                            min_improvement, previous_period,
                            satisfaction_value, prio_sensors = NULL,
                            params, dir_out) {
  
  completed_info <- paste0("\nCompleted selection process for timestamp: ",timestamp)
  period_new <- c() # for selection from multiple sensors
  base_records <- c() # same 
  ids <- c() # same
  valid_pixels <- 0 # same
  
  if (is.null(prio_sensors) || length(prio_sensors) == 1) {
    le_prio_is_one <- TRUE
    prio_sensors <- "none"
  } else {
    le_prio_is_one <- FALSE
  }
  
  for (s in prio_sensors){
    
    if (le_prio_is_one) {
      # in case prio_sensors is not given process all sensors together
      s_match <- which(!is.na(records$product))  
    } else {
      # in case prio_sensors is given process sensors in this order
      s_match <- which(records$product==s)
      # in case of MODIS as prio_sensor we get 'MODIS' from the user, which does not match any product name
      if (length(s_match) == 0) {
        s_match <- which(startsWith(records$product, s)) # all MODIS products
      }
    }
    sensor_match <- intersect(which(records$sub_period==timestamp),s_match)
    if (length(sensor_match) == 0) { # no records for sensors s at timestamp
      if (le_prio_is_one) .catch_empty_records(data.frame(),timestamp) else break
    } 
    
    tstamp <- list()
    tstamp$records <- records[sensor_match,]
    tstamp$records <- records[which(!is.na(records[[params$sub_period_col]])),]
    tstamp$records <- tstamp$records[which(!is.na(tstamp$records[[params$preview_col]])),]
    .catch_empty_records(tstamp$records, timestamp)
    tstamp$period <- .identify_period(tstamp$records[[params$date_col]])
    
    if (timestamp > 1) {
      # enforce to min_distance from previous timestamp
      tstamp$first_date <- .select_force_distance(previous_period,min_distance)
      tstamp$period <- .select_handle_next_sub(first_date=tstamp$first_date,
                                               period_initial=tstamp$period,
                                               min_distance,max_sub_period)
      tstamp$records <- .select_within_period(records,tstamp$period,params$date_col) # subset to records in period
    }
    
    delete_files <- ifelse(le_prio_is_one,FALSE,ifelse(s == tail(prio_sensors,1),TRUE,FALSE))
    
    # run the selection process
    selected <- .select_process_sub(tstamp$records,
                                    aoi,
                                    tstamp$period,
                                    period_new=period_new,
                                    base_records=base_records,
                                    max_sub_period,
                                    max_cloudcov_tile,
                                    min_improvement,
                                    satisfaction_value,
                                    delete_files,
                                    params,
                                    dir_out,
                                    ts=timestamp)
    
    if (class(selected) != "list") {
      .catch_empty_records(data.frame(),timestamp)
    } else {
      if (isFALSE(le_prio_is_one)) {
        # if combined selection of multiple optical sensors
        if (selected$valid_pixels < satisfaction_value) out("\nSelecting records for next sensor in 'prio_sensors'")
        base_records <- c(base_records,selected$cMask_paths) # for base mosaic for selection from next sensor
        ids <- unique(c(ids,selected$ids)) # ids of selected records
        names(base_records) <- ids
        valid_pixels <- selected$valid_pixels # percentage of valid pixels in aoi
        period_new <- .identify_period(c(period_new,selected$period)) # combined period
      } else {
        # if only one optical sensor is given
        out(completed_info)
        return(selected)
      }
    }  
  }
  
  if (length(ids) == 0) .catch_empty_records(data.frame(),timestamp)
  
  selected <- list(ids=ids,
                   cMask_paths=base_records,
                   valid_pixels=valid_pixels,
                   period=period_new)
  
  out(completed_info)
  
  return(selected)
  
}

#' calls the different steps of selection for a sub-period
#' this includes enforcement of max_cloudcov_tile and max_sub_period
#' @param records data.frame subsetted to a sub-period.
#' @param aoi aoi.
#' @param period character vector of start and end date.
#' @param period_new character vector an existing period for the timestamp. Default is c().
#' @param base_records character vector of paths to cloud masks that create a base mosaic. Default is NULL.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' The value is the percentage of not yet covered area that shall be covered additionally when adding the record.
#' @param delete_files logical TRUE if all files in tmp_dir shall be deleted at the end of the call.
#' If it is a selection for multiple sensors this should be FALSE as long there is a next sensor following.
#' @param satisfaction_value numeric.
#' @param params list holding everything inserted into this parameter list in the calling select function (8 parameters).
#' @param dir_out character directory where to save intermediate product.
#' @param ts numeric of the current timestamp.
#' @return \code{selected} list of [[ids]] character vector of selected ids, [[cMask_paths]] character vector to cloud masks, 
#' [[valid_pixels]] percentage of valid pixels in mosaic with the given selection, [[period]] charater vector of two dates.
#' @keywords internal
#' @noRd
.select_process_sub <- function(records, aoi,
                                period, period_new = NULL, base_records = NULL,
                                max_sub_period, max_cloudcov_tile, min_improvement, satisfaction_value,
                                delete_files, params, dir_out, ts) {
  
  # the sub is an ordering of all available records per tile according to aoi cloud cover
  # this is also the step where max_cloudcov_tile is ensured
  sub <- .select_sub(records=records,
                     tiles=params$tileids,
                     max_cloudcov_tile=max_cloudcov_tile,
                     aoi_cc_col=params$aoi_cc_col,
                     tileid_col=params$tileid_col,
                     date_col=params$date_col,
                     identifier=params$identifier)
  
  sub <- .check_list(sub)
  if (class(sub) != "list") return(NA)
  
  # this step enforces max_sub_period. It returns a list of vectors of indices 
  # pointing to records in records. The list is ordererd according to aoi cloud cover
  sub_within <- .select_force_period(records,sub,period,max_sub_period,period_new=period_new,
                                     date_col=params$date_col,aoi_cc_col=params$aoi_cc_col)
  
  sub_within <- .check_list(sub_within)
  if (class(sub_within) != "list") return(NA)
  
  # calculate best mosaic of cloud masks for first timestamp
  if (is.null(base_records)) {
    out(params$sep)
    out(paste0("Calculating mosaic for timestamp: ",ts))
  }
  
  selected <- try(.select_calc_mosaic(records,
                                      base_records=base_records,
                                      aoi,
                                      sub_within,
                                      params$cloud_mask_col,
                                      min_improvement=min_improvement,
                                      satisfaction_value=satisfaction_value,
                                      ts=ts,
                                      dir_out,
                                      params$identifier,
                                      delete_files=delete_files))
  if (inherits(selected,"try-error")) {
    out(paste0("\nSelection error at timestamp: ",ts),2)
  }
  selected$period <- .identify_period(records[which(records[[params$identifier]] %in% selected$ids),params$date_col])
  return(selected)
  
}

#' select timestamps for SAR data according to num_timestamps, min_distance and max_sub_period.
#' @param records data.frame.
#' @param period_new_all list of character vectors of two dates, one vector for each timestamp.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param max_sub_period numeric maximum length of sub-period.
#' @param params list holding everything inserted into this parameter list in the calling select function.
#' @return \code{SAR_selected} list of [[ids]] character vector of selected ids per timestamp, [[period]] character vector
#' of two dates and [[sub-period]] numeric the sub-period number
#' character vector of two dates (start and end date of SAR sub-period)
#' @keywords internal
#' @noRd
.select_SAR <- function(records, period_new_all = NULL,
                        max_sub_period, min_distance, num_timestamps, params) {
  
  subperiods <- unique(records$sub_period)
  # if the length of the current sub-period is longer than max_sub_period grade all dates in sub_period
  # according to the number of the given records for each date and calculate the distance of each record
  # from this date. Exclude records consecutively until max_sub_period is reached
  selected_SAR <- list() # to be filled with the selected lists of selected ids and sub-periods
  
  for (s in 1:length(subperiods)) {
    
    records_in_s <- records[which(records$sub_period==s),]
    period_new <- period_new_all[[s]]
    previous <- s-1
    
    if (s > 1) {
      # enforce min_distance
      # combined period of period_new from optical records (if not NULL because has_SAR == 100) and previous SAR period
      previous_period <- .identify_period(c(period_new_all[[previous]],selected_SAR[[previous]]$period))
      period_initial <- .identify_period(records_in_s[[params$date_col]])
      # earliest date of next sub-period adjusted
      first_date <- .select_force_distance(previous_period,min_distance)
      period_s <- .select_handle_next_sub(first_date,period_initial,
                                          min_distance,max_sub_period)
      records_in_s <- .select_within_period(records_in_s,period_s,params$date_col) # subset to records within period_s
    }
    
    dates_s <- sapply(records_in_s[[params$date_col]],as.Date)
    
    if (!is.null(period_new)) {
      period_new_dates <- sapply(period_new,as.Date)
      dates_combined <- c(dates_s,period_new_dates)
      dates_s <- min(dates_combined,max(dates_combined))
    }
    
    min_dates_s <- min(dates_s)
    max_dates_s <- max(dates_s)
    sub_period_le <- max_dates_s-min_dates_s
    # grade dates and exclude consecutively
    
    if (sub_period_le > max_sub_period) {
      dates_seq <- min_dates_s:max_dates_s
      date_grade <- sapply(dates_seq,function(date) {
        records_match <- which(dates_s==date)
        # include one record per tile for the grading as for SAR several records on one date = no benefit
        grade <- length(unique(records[records_match,params$tileid_col]))
      })
      
      # calculate best_period based on date_grade, in combination with period_new
      # and while ensuring max_sub_period
      best_period <- .select_best_period(date_grade=date_grade, 
                                         dates_seq=dates_seq, 
                                         min_date=min(dates_seq), 
                                         max_date=max(dates_seq),
                                         period_new=period_new,
                                         max_sub_period=max_sub_period)
      
      if (!is.na(best_period)) {
        incl <- .select_subset_to_best_period(dates_s, best_period)
        ids <- records_in_s[incl,params$identifier] # ids of selected records in upated sub-period
      }
    } else {
      ids <- records_in_s[[params$identifier]] # all ids of records in sub-period
    }
    dates_sel <- records_in_s[which(ids %in% records[[params$identifier]]),params$date_col]
    period <- .identify_period(dates_sel)
    selected_SAR[[s]] <- list("ids"=ids,"period"=period,"timestamp"=s)
  }
  
  return(selected_SAR)
  
}

#' selection process when only SAR given in records
#' @param records data.frame.
#' @param max_sub_period numeric max_sub_period.
#' @param min_distance numeric min_distance.
#' @param num_timestamps numeric num_timestamps.
#' @param params list par.
#' @return records data.frame ready for return to user
#' @keywords internal
#' @noRd
.select_all_SAR <- function(records, max_sub_period,
                            min_distance, num_timestamps,
                            params) {
  
  selected_SAR <- .select_SAR(records,period_new = NULL,
                              max_sub_period,min_distance,
                              num_timestamps,params)
  records <- .select_finish_SAR(records, selected_SAR, num_timestamps, params)
  return(records)
  
}

#' selection process for SAR when some SAR given in records
#' @param records data.frame.
#' @param selected list selected.
#' @param max_sub_period numeric max_sub_period.
#' @param min_distance numeric min_distance.
#' @param num_timestamps numeric num_timestamps.
#' @param params list par.
#' @return selected list of lists where each list is a selected list of one timestamp
#' @keywords internal
#' @noRd
.select_some_SAR <- function(records, selected, max_sub_period,
                             min_distance, num_timestamps, params) {
  
  # SAR records shall be searched within max_sub_period combined
  # with the periods selected for optical 
  period_new_all <- lapply(selected,function(x) return(x[["period"]]))
  selected_SAR <- .select_SAR(records,period_new_all,max_sub_period,min_distance,
                              num_timestamps,params)
  
  # add selected ids to selected list of optical records
  for (ts in 1:length(selected_SAR)) {
    ts_SAR <- selected_SAR[[ts]]
    optical_ids <- selected[[ts]][["ids"]]
    selected[[ts]][["ids"]] <- append(optical_ids,ts_SAR[["ids"]])
    selected[[ts]][["period"]] <- .identify_period(c(selected[[ts]]$period,ts_SAR[["period"]]))
  }
  return(selected)
  
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

#### SELECT SUB-PROCESSES

#' selects initial records for a sub-period while ensuring max_cloudcov_tile
#' @param records data.frame subsetted to a sub-period.
#' @param tiles character vector of the tile ids.
#' @param period character vector of start and end date.
#' @param aoi_cc_col character name of aoi cloud cover column.
#' @param tileid_col character name of tile id column.
#' @param date_col character name of the date column.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @return \code{sub} list of numeric vectors, each number is an index to a record in \code{records}
#' @keywords internal
#' @noRd
.select_sub <- function(records, tiles, max_cloudcov_tile,
                        aoi_cc_col, tileid_col, date_col, identifier) {
  
  sub <- lapply(tiles,function(x) {
    rec_tile_sub <- records[which(records[[tileid_col]]==x),]
    i <- as.integer(0)
    lwst_cc <- as.integer(0)
    selected <- c()
    rec_ord <- rec_tile_sub[order(rec_tile_sub[[aoi_cc_col]]),]
    while(!is.na(lwst_cc) && i <= NROW(rec_tile_sub)) {
      i <- i+1
      if (i > NROW(rec_tile_sub)) {
        lwst_cc <- NA
      } else {
        lwst_cc <- rec_ord[i,identifier] # id of i lowest
        # ensure max_cloudcov_tile
        cc <- rec_ord[i,aoi_cc_col]
        above_max <- ifelse(is.na(cc) || any(is.null(c(lwst_cc,cc))),TRUE,
                            cc > max_cloudcov_tile)
        if (above_max) {
          lwst_cc <- NA
        } else {
          selected[i] <- which(records[,identifier] == lwst_cc) # get index of record in records
        }
      }
    }
    return(unique(selected))
  })
  names(sub) <- tiles
  return(sub)
}

#' returns a sub list of indices pointing to records within max_sub_period, 
#' returned in orders according to aoi cloud cover 
#' @param records data.frame.
#' @param sub list of numeric vectors each pointing to a record.
#' @param period character vector of start and end date.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param period_new character vector of an existing period for this timestamp.
#' @param date_col character name of the date column.
#' @param aoi_cc_col character name of aoi cloud cover column.
#' @return \code{sub_within} list of numeric vectors, each number is an index to a record in \code{records}
#' @keywords internal
#' @noRd
.select_force_period <- function(records, sub, period, max_sub_period, period_new, 
                                 date_col, aoi_cc_col) {
  
  # check if covered period of timestamp is within max_sub_period
  # and re-calculate period consecutively with record of next-lowest cloud cover
  max_num_sel <- max(sapply(sub,length))
  orders <- sapply(1:max_num_sel,function(i) unlist(sapply(sub,function(x) return(x[i]))))
  orders <- data.frame(orders)
  sub_within <- list()
  covered_tiles <- list()
  
  for (i in 1:NCOL(orders)) {
    order <- orders[,i]
    order <- order[!is.na(order)]
    # first, try to use all records of this order
    dates_x <- records[order,date_col]
    period_tmp <- .select_bridge_period(dates_x,period_new)
    period_tmp_le <- .period_days(period_tmp)
    
    if (period_tmp_le <= max_sub_period) { # the case where all records from current order x are within period_new
      period_new <- period_tmp
      sub_within[[i]] <- order
    } else {
      # for the case where at least one of record of order x is not within period_new
      # try with all values in all possible combinations (not orders). Might be that 
      # from 0 to all records except one are within period
      order_within <- .select_remove_dates(order,records,period_new,max_sub_period,
                                           date_col, aoi_cc_col)
      if (!is.na(order_within[1])) {
        period_new <- c(.select_bridge_period(records[order_within,date_col],period_new))
        
        # check which tile ids were not given in first order and move this record to first order
        covered_tiles[[i]] <- records[order_within,"tile_id"]
        if (i > 1) {
          tiles_not_in_first <- which(!covered_tiles[[i]] %in% covered_tiles[[1]])
          if (length(tiles_not_in_first) > 0) {
            sub_within[[1]] <- append(sub_within[[1]],order_within[tiles_not_in_first])
            covered_tiles[[1]] <- append(covered_tiles[[1]],covered_tiles[[i]])
            order_within <- order_within[-tiles_not_in_first]
          }
        }
        sub_within[[i]] <- order_within
      }
    }
  }
  
  return(sub_within)
  
}

#' finds optimal dates from a records order within a period of a timestamp and max_sub-period.
#' @param x numeric vector. All values index a record in records.
#' @param records data.frame.
#' @param period_new character vector of two dates.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param date_col date_col.
#' @param aoi_cc_col aoi_cc_col.
#' @return \code{order} numeric vector a subset of x with all records within max_sub_period length
#' @importFrom stats median
#' @keywords internal
#' @noRd
.select_remove_dates <- function(x, records, period_new, max_sub_period, date_col, aoi_cc_col) {
  
  dates <- sapply(records[x,date_col],function(d) {as.Date(d)})
  # for each of the dates count how many records are given, this creates a distribution of the records in time as a basis for reference date selection
  # grade each date according to number of given records and the mean aoi cloud cover of this records
  min_date <- min(dates)
  max_date <- max(dates)
  dates_seq <- min_date:max_date
  date_grade <- list()
  for (d in dates_seq) {
    sel <- which(dates == d)
    # turn low cc values into high ones because high counts of sel are good
    cc <- sapply(records[sel,aoi_cc_col],function(c) {100-as.numeric(c)}) 
    value <- ifelse(length(cc)==0,0,mean(length(sel) * cc))
    date_grade[[as.character(d)]] <- value
  }
  
  # select best period with maximum values in sum of grades while ensuring max_sub_period
  # if a period_new is given test all possible periods combined with period_new
  best_period <- try(.select_best_period(date_grade = date_grade, dates_seq = dates_seq, 
                                         min_date = min_date, max_date = max_date,
                                         period_new = period_new,
                                         max_sub_period = max_sub_period))
  if (inherits(best_period,"try-error")) {
    return(NA)
  } else {
    incl <- .select_subset_to_best_period(dates,best_period)
    order <- if (length(incl) == 0) NA else x[incl]
    return(order)
  }
  
}

#' helper for subsetting records to the best_period
#' @param dates numeric vector of dates as days since 1970-01-01.
#' @param best_period numeric vector of two dates in the same format as dates.
#' @return \code{order} 
#' @keywords internal
#' @noRd
.select_subset_to_best_period <- function(dates, best_period) {
  incl <- intersect(which(dates > best_period[1]),which(dates < best_period[2]))
}

#' selects best period from graded dates of a timestamp, optionally combined
#' with a period_new while ensuring max_sub_period.
#' @param date_grade numeric vector of grade values.
#' @param dates_seq sequence of characters all dates in given records of adjusted sub-period.
#' @param min_date numeric date as days since 1970-01-01. Minimum date of dates_seq.
#' @param max_date numeric date as days since 1970-01-01. Maximum date of dates_seq.
#' @param period_new character vector of two dates. A period to which given records shall
#' be adjusted. Can be NULL if not existing.
#' @param max_sub_period numeric number of days.
#' @return \code{best_period} numeric vector of two date values as days since 1970-01-01.
#' @keywords internal
#' @noRd
.select_best_period <- function(date_grade, dates_seq, min_date, max_date, 
                                period_new, max_sub_period) {
  
  max_sub_half <- round((max_sub_period - 1) / 2)
  air_plus <- ifelse(max_sub_half*2+1 < max_sub_period,max_sub_half+1,max_sub_half)
  air_minus <- max_sub_half
  if (is.null(period_new)) {
    # for each date in dates_seq create the sub_period around it according to max_sub_period
    # calculate the sum grade of all dates within that sub_period
    # check optionally if this sub_period matches max_sub_period together within period_new
    # return the mean grade value or NA
    sum_grade <- sapply(dates_seq,function(d) {
      period_tmp <- c(d - air_minus, d + air_plus)
      if (period_tmp[1] < min_date) period_tmp[1] <- min_date
      if (period_tmp[2] > max_date) period_tmp[2] <- max_date
      first <- which(dates_seq==period_tmp[1])
      last <- which(dates_seq==period_tmp[2])
      sum_grade <- sum(unlist(date_grade[first:last]))
      return(sum_grade)
    })
    best_mid_date <- dates_seq[which(sum_grade == max(sum_grade))][1]
    best_period <- c(round(best_mid_date-air_minus),(best_mid_date+air_plus))
  } else {
    # find optimal new sub-period from period_new and given grades of dates
    # 1 remove dates from dates_seq and date_grade that cannot be within
    # max_sub_period when combining with period_new
    period_new_date <- sapply(period_new,as.Date)
    period_new_seq <- period_new_date[1]:period_new_date[2]
    # how many new dates can be added before reaching max_sub_period
    air <- max_sub_period - length(period_new_seq)
    if (air <= 0) {
      best_period <- period_new_date
      return(best_period)
    } else {
      # try all possible periods with air and return the sum of grades within that
      air_minus <- air:0
      air_plus <- rev(air_minus)
      shifted_grades <- sapply(1:length(air_minus),function(i) {
        a <- air_minus[i]
        period_tmp <- as.integer(period_new_date[1]-a):as.integer(period_new_date[2]+air_plus[i])
        sum_grade <- sum(unlist(date_grade[which(period_tmp %in% dates_seq)]))
        return(sum_grade)
      })
      shifted_grades <- shifted_grades[!is.na(shifted_grades)]
      if (length(shifted_grades) == 0) {
        return(NA)
      }
      best_grade <- which(shifted_grades == max(shifted_grades))[1]
      best_period <- c(period_new_date[1]-air_minus[best_grade],
                       period_new_date[2]+air_plus[best_grade])
      return(best_period)
    }
  }
  
}

#' creates initial sub-periods 
#' @param records data.frame.
#' @param period character vector of a start date [1] and an end date [2].
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param date_col character name of the date column.
#' @return \code{records} with one added numeric column 'sub_period' indicating in which sub-period the record is situated.
#' @keywords internal
#' @noRd
.select_sub_periods <- function(records, period, num_timestamps, params) {
  
  period <- sapply(period,as.Date)
  days <- as.integer(diff(period))
  le_subperiods <- days / num_timestamps
  dates <- sapply(0:num_timestamps,function(i) date <- period[1] + (i * le_subperiods))
  l <- length(dates)
  dates[l] <- dates[l] + 1
  date_col_mirr <- sapply(records[[params$date_col]],as.Date) # mirror of the date column as days since 1970-01-01
  for (i in 1:num_timestamps) {
    within <- intersect(which(date_col_mirr >= dates[i]),which(date_col_mirr < dates[i+1]))
    records[within, params$sub_period_col] <- i
  }
  return(records)
  
}

#' checks first possible date for selection after previous selection according to user-specified min_distance
#' @param period character vector of dates. Last is the end date.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @return \code{min_date} character indicating the minimum possible date for the next sub_period.
#' @keywords internal
#' @noRd
.select_force_distance <- function(period, min_distance) {
  next_date <- as.Date(period[2]) + min_distance 
}

#' handles the determined earliest start date for next sub-period calculated from min_distance
#' @param first_date character date the earliest possible date of next sub-period.
#' @param period_initial character vector of two dates. Period derived from records of next sub-period.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed. This determines how close together
#' @return \code{period} character vector the actual period for next sub-period.
#' @keywords internal
#' @noRd
.select_handle_next_sub <- function(first_date, period_initial, min_distance, max_sub_period) {
  
  dfirst_date <- as.Date(first_date)
  dperiod_initial <- as.Date(period_initial)
  
  if (dfirst_date >= dperiod_initial[1] || dfirst_date < dperiod_initial[1]) {
    period <- as.character(c(dfirst_date,dperiod_initial[2]))
  } else if (dfirst_date >= dperiod_initial[2]) {
    out(paste0("Argument 'min_distance' between acquisitions used for dinstinguished timestamps is: ",min_distance," days.
               The 'max_period' of covered acquisitions for one timestamp is: ",max_sub_period,". With the given 'num_timestamps'
               these values disable the creation of a temporally consistent selection. Modify the values (most likely decrease (some of) them."),3)  
  }
  
  }

#### SELECT OUT

#' communicates a selection process start info
#' @param mode character selection mode.
#' @param sep character aesthetic element for console output.
#' @return nothing. Console output.
#' @keywords internal
#' @noRd
.select_start_info <- function(mode,sep) {
  out(paste0(sep,"\n           Starting ",mode," Selection Process           ",sep))
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
  out(paste0("\r", "-      ",cov,"  %      after having checked on ",i," of ",le_collection," available records of sensor ",sensor,"  "), flush = T)
  
}

#' checks if an new coverage percentage exceeds the min_improvement argument
#' @param min_improvement numeric.
#' @param cov_init numeric.
#' @param cov_aft numeric.
#' @param proportion numeric the proportion of the aoi covered by the current tile.
#' Has to be between 0 and 1.
#' @return exceeds logical.
#' @keywords internal
#' @noRd
.select_exceeds_improvement <- function(min_improvement, cov_init, cov_aft) {
  
  add_it <- min_improvement < (((cov_aft - cov_init) / cov_init) * 100)
  return(add_it)
  
}

#' checks which records are within a period of time
#' @param records data.frame.
#' @param period character vector of dates. Last is the end date.
#' @param date_col character name of the date column.
#' @return \code{records} data.frame reduced to matching records.
#' @keywords internal
#' @noRd
.select_within_period <- function(records, period, date_col) {
  
  dates <- as.Date(records[[date_col]])
  cond <- intersect(which(dates >= period[1]),which(dates <= period[2]))
  records <- records[cond,]
  
}

#' bridge function to the period identifier \link{.identify_period}. Enables to calculate from
#' a given period with added dates a new period.
#' @param dates_tmp character vector of character dates.
#' @return period_new character holding a period of dates.
#' @keywords internal
#' @noRd
.select_bridge_period <- function(dates_tmp,period_new) {
  
  period_curr <- .identify_period(dates_tmp)
  period_new <- .identify_period(c(period_new,period_curr))
  
}