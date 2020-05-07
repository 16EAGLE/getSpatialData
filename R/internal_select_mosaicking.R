#' ---------------------------------------------------------------------
#' name: internal_select_mosaicking
#' description: These are the functions that conduct the spatial selection
#' process and everything related to cloud mask and preview mosaicking within select.
#' author: Henrik Fisser, 2019
#' ---------------------------------------------------------------------

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
  srcnodata <- ifelse(src_datatype == INT2S(),"-32768","-3.3999999521443642e+38")
  mos_base <- .make_mosaic(paths,save_path, mode=mode, srcnodata=srcnodata,
                           datatype=src_datatype)
  if (!inherits(mos_base, RASTER_LAYER())) {
    return(NA)
  } else {
    
    mos_base_mask <- .mask_raster_by_polygon(mos_base, aoi)
    mos_base_crop <- crop(mos_base_mask, aoi)
    writeRaster(mos_base_crop, save_path, overwrite=T,
                srcnodata=srcnodata, datatype=src_datatype)
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
  
  save_path_cmos <- file.path(dir_out, paste0("cloud_mask_mosaic_ts", s$timestamp, ".tif"))
  cMask_mosaic <- .select_bridge_mosaic(s$cMask_paths, aoi, save_path_cmos)
  rm(cMask_mosaic)
  .delete_tmp_files(tmpDir())
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
  
  tmp_dir_orig <- tempdir()
  tmp_dir <- .tmp_dir(dir_out,action=1,TRUE)
  
  r <- RASTER_LAYER()
  id_sel <- sapply(s$ids,function(x) which(records[[identifier]]==x))
  save_str <- paste0(sample(LETTERS[1:20], 10),"_",collapse = "")
  save_pmos <- file.path(tmp_dir,paste0(save_str,"preview_mosaic_timestamp",s$timestamp))
  layers <- c("red", "green", "blue")
  
  # cloud mask previews band-wise
  # this returns a list of paths to each of the three cloud-masked bands per record
  preview_paths <- lapply(id_sel,function(id_index) {
    
    p_path <- records[id_index,preview_col]
    if (is.na(p_path) || !file.exists(p_path)) return(NA)
    cMask <- raster(records[id_index,cloud_mask_col]) # cloud mask
    preview <- stack(p_path)
    cMask <- .check_crs(cMask)
    preview <- .check_crs(preview)
    
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
    if (!inherits(pmos, r)) return(NA) else return(pmos)
  })
  
  if (any(sapply(preview_mos, class) != r)) out("Could not create preview RGB mosaic",2)
  
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
#' @importFrom sf st_union st_intersection st_as_sfc st_bbox
#' @keywords internal
#' @noRd
#' @author Henrik Fisser
.select_calc_mosaic <- function(records, base_records, aoi, sub_within, cloud_mask_col, 
                                min_improvement, satisfaction_value, 
                                ts, dir_out, identifier, delete_files) {
  
  tmp_dir_orig <- tempdir()
  tmp_dir <- .tmp_dir(dir_out,1,TRUE)
  
  if (is.null(base_records)) out("Current coverage of valid pixels..")
  
  TMP_CROP <- "crop_tmp.tif"
  TMP_CROP_MOS <- "curr_crop_mos_tmp.tif"
  TMP_BASE_MOS <- "base_mos_tmp_"
  r <- RASTER_LAYER()
  curr_sensor <- unique(records$product)
  le_first_order <- length(sub_within[[1]])
  
  if (length(sub_within) > 1) {
    # sort the orders of sub_within according to the aoi cloud cover of the same tile in previous order
    # this way for each order those tiles will be checked first that had the highest cloud cover in previous order
    # it shall result in handling large gaps first and small gaps late
    tile_mirror <- sapply(sub_within,function(order) return(sapply(order,function(i) return(records[i,][[name_tile_id()]]))))
    cc_col <- name_aoi_hot_cloudcov_percent()
    sub_within_sorted <- append(sub_within[1],lapply(2:length(sub_within),function(i) {
      curr_tiles <- tile_mirror[[i]]
      prev_tiles <- tile_mirror[[i-1]]
      tile_in_previous <- which(curr_tiles %in% prev_tiles)
      # cloud cover of records of previous order
      cc_previous <- records[sub_within[[i-1]][which(prev_tiles %in% curr_tiles)],cc_col] 
      sorted_order <- sub_within[[i]][tile_in_previous][order(cc_previous, decreasing=TRUE)]
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
  start <- ifelse(start > le_collection, le_collection,start)

  # aggregate raster adjusted to aoi area size in order to speed up process
  names <- names(base_records)
  base_records <- .aggr_rasters(base_records, names, aoi=aoi, dir_out=tmp_dir)
  names(base_records) <- names
  # this base mosaic will be updated with each added record after check if valid cover is increased
  base_mos_path <- file.path(tmp_dir,"base_mosaic_tmp.tif")
  base_mos <- .select_bridge_mosaic(base_records, aoi, base_mos_path)
  if (!inherits(base_mos, r)) start <- 1
  # cleanup
  .delete_tmp_files(tmp_dir) # delete temp raster .grd files in r tmp
  rm(base_mos)
  base_coverage <- -1000
  n_pixel_aoi <- .calc_aoi_corr_vals(aoi, raster(base_records[1])) # correction values for coverage calc
  
  # add next cloud mask consecutively and check if it decreases the cloud coverage
  for (i in start:le_collection) {

    x <- collection[i] # current cloud mask
    base_mos <- raster(base_mos_path) # current base mosaic
    
    if (i == start) {
      is_last_record <- i == le_collection
      base_coverage <- .raster_percent(base_mos, mode="aoi", aoi=aoi, n_pixel_aoi)
      .select_out_cov(base_coverage,ifelse(is_last_record,i,i-1),le_collection,curr_sensor)
      if (is_last_record) break
    }
    
    name_x <- names(x)
    # before calculating the next mosaic, 
    # check if record tile is within the area of non-covered pixels at all
    x <- .aggr_rasters(x, name_x, aoi=aoi, dir_out=tmp_dir)
    names(x) <- name_x
    next_record <- raster(x) # record to be added if it supports the mosaic
    next_record <- .mask_raster_by_polygon(next_record, aoi) # mask to aoi because saved cloud mask is not aoi cloud mask
    next_record <- .check_crs(next_record)
    curr_base_mos_crop <- crop(base_mos, next_record) # crop base mosaic to tile area of next
    aoi_subset <- st_as_sfc(st_bbox(next_record), crs=4326)
    aoi_subset <- .check_crs(aoi_subset)
    aoi_union <- st_union(aoi) # ensure it's a single feature
    aoi_subset <- st_intersection(aoi_subset, aoi_union) # get intersection of tile and aoi
    cov_init <- .raster_percent(curr_base_mos_crop, mode="aoi", aoi=aoi_subset, n_pixel_aoi)

    if (round(cov_init) == 99) {
      add_it <- FALSE
    } else {
      crop_p <- file.path(tmp_dir, TMP_CROP)
      curr_mos_tmp_p <- normalizePath(file.path(tmp_dir, TMP_CROP_MOS))
      writeRaster(curr_base_mos_crop,crop_p,overwrite=T,datatype=dataType(base_mos))
      curr_mos_tmp <- .select_bridge_mosaic(c(crop_p, x), aoi, curr_mos_tmp_p) # in this tile add next_record
      
      if (inherits(curr_mos_tmp, r)) {        
        print("5")
        cov_aft <- .raster_percent(curr_mos_tmp,mode="aoi",aoi=aoi_subset) # check new coverage
        # cleanup
        .delete_tmp_files(tmp_dir)
        unlink(curr_mos_tmp_p)
        rm(next_record,curr_base_mos_crop,curr_mos_tmp,base_mos)
        print("6")
        
        # calculate if valid coverage in whole aoi is improved > min_improvement 
        # when adding the record to the tile area
        if (((cov_init + (cov_init / 100) * min_improvement)) > 100) {
          add_it <- cov_aft > cov_init && cov_init < 99
        } else {
          add_it <- .select_exceeds_improvement(min_improvement,cov_init,cov_aft)
        }
      } else {
        add_it <- FALSE
      }
    }
    
    if (add_it) {
      print("7")
      save_str <- TMP_BASE_MOS
      curr <- paste0(save_str,i, ".tif")
      base_mos_path_tmp <- normalizePath(file.path(tmp_dir,curr))
      base_records <- c(base_records, x) # add save path of current mosaic
      base_mos <- .select_bridge_mosaic(base_records,aoi,base_mos_path_tmp) # mosaic with added record
      print("8")
      if (inherits(base_mos, r)) {
        # cleanup
        rm(base_mos)
        # delete previous base_mos_tmp tifs
        base_mos_tmp_files <- list.files(tmp_dir,pattern=save_str)
        base_mos_tmp_files <- base_mos_tmp_files[which(base_mos_tmp_files!=curr)]
        del <- sapply(base_mos_tmp_files, function(del_file) {
          del_path <- file.path(tmp_dir, del_file)
          if (file.exists(del_path) && del_file != curr) unlink(del_path)
        })
        rm(del)
        base_mos <- raster(base_mos_path_tmp)
        base_coverage <- .raster_percent(base_mos ,mode="aoi", aoi=aoi, n_pixel_aoi)
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
    .tmp_dir(dir_out, 2, TRUE, tmp_dir_orig)
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
  cols <- c(params$selected_col, params$timestamp_col, params$pmos_col, params$cmos_col)
  records <- .select_prep_cols(records,cols,params$selected_col)
  
  for (i in 1:length(selected)) {
    
    s <- selected[[i]]
    ids_selected <- s$ids
    
    if (isFALSE(.is_empty_array(ids_selected))) {
      #A cloud mask mosaic
      save_path_cmos <- .select_cmask_mos(s,aoi,dir_out)
      #B preview mosaic
      save_path_pmos <- .select_preview_mos(records,s,aoi,i,params$identifier,dir_out,
                                            cloud_mask_col=params$cloud_mask_col,
                                            preview_col=params$preview_col,
                                            sensors_given=params$product_group)
    } else {
      save_path_cmos <- NA
      save_path_pmos <- NA
    }
    
    #C add columns to records
    save_path_pmos <- normalizePath(save_path_pmos)
    save_path_cmos <- normalizePath(save_path_cmos)
    insert <- c(TRUE, s$timestamp, save_path_pmos, save_path_cmos)
    for (j in 1:length(cols)) {
      records[which(records[[params$identifier]] %in% ids_selected),cols[j]] <- insert[j]      
    }
    # get print info
    console_info[[i]] <- .select_final_info(s)
    
  }
  each_timestamp <- .out_vector(console_info)
  rm(each_timestamp)
  return(records)
  
}