#' ---------------------------------------------------------------------
#' @name internal
#' @description These are internal functions that rather work as utils.
#' This internal file should only contain generic functions that can be
#' used multiple times within the package.
#' @keywords internal
#' ---------------------------------------------------------------------

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

#' first character to upper
#'
#' @param x character
#' @keywords internal
#' @noRd

firstup <- function(x){
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

#' Simplifies check of variables being FALSE
#'
#' @param evaluate variable or expression to be evaluated
#'
#' @keywords internal
#' @noRd
is.FALSE <- isFALSE <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x

#' Simplifies check of variables being TRUE
#'
#' @param evaluate variable or expression to be evaluated
#'
#' @keywords internal
#' @noRd
is.TRUE <- isTRUE <- function (x) is.logical(x) && length(x) == 1L && !is.na(x) && x

#' check if url
#' @param url a url
#' @keywords internal
#' @noRd
is.url <- function(url) grepl("www.|http:|https:", url)

#' gSD.get
#' @param url url
#' @param username user
#' @param password pass
#' @param dir.file output file path
#' @param prog show or not show progress console
#' 
#' @importFrom httr GET stop_for_status warn_for_status message_for_status progress authenticate write_disk
#' 
#' @keywords internal
#' @noRd
gSD.get <- function(url, username = NULL, password = NULL, dir.file = NULL, prog = F){

  x <- NULL # needed due to checks
  get.str <-"x <- try(GET(url"
  if(!is.null(username)) get.str <- paste0(get.str, ", authenticate(username, password)")
  if(!is.null(dir.file)) get.str <- paste0(get.str, ", write_disk(dir.file)")
  if(isTRUE(prog)) get.str <- paste0(get.str, ", progress()")
  get.str <- paste0(get.str, "), silent = T)")
  eval(parse(text = get.str))

  if(inherits(x, "try-error")) out(paste0("Could not process request: ", gsub("  ", "", strsplit(x[[1]], "\n")[[1]][2])), type=3)
  stop_for_status(x, "process request")
  warn_for_status(x)
  #message_for_status(x); cat("\n")
  return(x)
}

#' gSD.post
#' @param url url
#' @param username user
#' @param password pass
#' @param body body
#' 
#' @importFrom httr POST stop_for_status warn_for_status message_for_status progress
#' 
#' @keywords internal
#' @noRd
gSD.post <- function(url, username = NULL, password = NULL, body = FALSE){

  x <- NULL # needed due to checks
  post.str <-"x <- POST(url"
  if(!is.null(username)) post.str <- paste0(post.str, ", authenticate(username, password)")
  post.str <- paste0(post.str, ", body = body)")
  #eval(parse(text = post.str))

  if(!is.null(username)) x <- POST(url, authenticate(username, password), body = body) else x <- POST(url, body = body)
  stop_for_status(x, "connect to server.")
  warn_for_status(x)
  #message_for_status(x); cat("\n")}
  return(x)
}

#' gSD.download
#' @param x file record
#' @param names column names of record
#' @param prog logical
#' @param force logical
#' @importFrom tools md5sum
#' @keywords internal
#' @noRd
# gSD.download <- function(x, names, prog = T, force = F){
#   
#   x <- as.list(x)
#   names(x) <- names
#   
#   if(file.exists(x$dataset_file) & !isTRUE(force)){
#     out(paste0(x$gSD.head, "Skipping download of '", x$dataset_name, "', since '", x$dataset_file, "' already exists..."), msg = T)
#     return(TRUE)
#   } else{
#   
#     out(paste0(x$gSD.head, "Downloading '", x$dataset_name, "' to '", x$dataset_file, "'..."), msg = T)
#     file.tmp <- tempfile(tmpdir = paste0(head(strsplit(x$dataset_file, "/")[[1]], n=-1), collapse = "/")) #, fileext = ".tar.gz")
#     gSD.get(x$dataset_url, dir.file = file.tmp, prog = prog)
#     
#     if(!is.null(x$md5_checksum)){
#       if(as.character(md5sum(file.tmp)) == tolower(x$md5_checksum)){ out("Successfull download, MD5 check sums match.", msg = T)
#       } else{
#         out(paste0("Download failed, MD5 check sums do not match."), type = 2)
#         file.remove(file.tmp)
#         return(FALSE)
#       }
#     }
#   }
#   
#   file.rename(file.tmp, x$dataset_file)
#   return(TRUE)
# }
gSD.download <- function(url, file, name, head, type = "dataset", md5 = NULL, prog = T, force = F, ...){
  
  if(file.exists(file) & !isTRUE(force)){
    out(paste0(head, "Skipping download of ", type, " '", name, "', since '", file, "' already exists..."), msg = T)
    return(TRUE)
  } else{
    
    out(paste0(head, "Downloading ", type, " '", name, "' to '", file, "'..."), msg = T)
    file.tmp <- tempfile(tmpdir = paste0(head(strsplit(file, "/")[[1]], n=-1), collapse = "/")) #, fileext = ".tar.gz")
    gSD.get(url, dir.file = file.tmp, prog = prog, ...)
    
    if(!is.null(md5)){
      if(as.character(md5sum(file.tmp)) == tolower(md5)){ out("Successfull download, MD5 check sums match.", msg = T)
      } else{
        out(paste0("Download failed, MD5 check sums do not match."), type = 2)
        file.remove(file.tmp)
        return(FALSE)
      }
    }
  }
  
  file.rename(file.tmp, file)
  return(TRUE)
}

#' gSD.retry
#' @param files files data.frame
#' @param ... additional arguments
#' @param FUN function to retry
#' @param n.retry How many retries?
#' @importFrom tools md5sum
#' @keywords internal
#' @noRd
gSD.retry <- function(files, FUN, ..., n.retry = 3, delay = 0, verbose = T){
  
  files$download_attempts <- NA
  i.retry <- n.retry
  retry <- T
  
  out(paste0("[Attempt ", toString((n.retry-i.retry)+1), "/", toString(n.retry), "] Attempting downloads..."))
  while(isTRUE(retry) & i.retry > 0){
    
    # download per record
    files$download_success <- apply(files, MARGIN = 1, FUN, ...)
    files[which(files$download_success == TRUE & is.na(files$download_attempts)), "download_attempts"] <- (n.retry-i.retry)+1
    
    if(all(files$download_success == TRUE)){
      retry <- F
    } else{
      
      files <- files[files$download_success == F,]
      i.retry <- i.retry-1
      
      if(isTRUE(verbose)) out(paste0("[Attempt ", toString((n.retry-i.retry)+1), "/", toString(n.retry), "] Reattempting downloads..."))
    }
  }
  
  files$download_attempts[files$download_success == F] <- n.retry
  return(files)
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

#' get Copernicus Hub API url and credentials from user input
#'
#' @param x API keyword or URL
#' @param p platform
#' @param user user name
#' @param pw password
#' @keywords internal
#' @noRd
.CopHub_select <- function(x, p, user, pw){ #cophub_api
  if(is.url(x)){
    url <- x
  } else{
    if(x == "auto"){
      x <- getOption("gSD.copnames")[getOption("gSD.copnames")$name == p, 2]
    }
    if(x == "dhus"){url <- getOption("gSD.api")$dhus}
    if(x == "s5p"){
      url <- getOption("gSD.api")$s5p
      user <- "s5pguest"
      pw <- "s5pguest"
    }
    if(x == "gnss"){
      url <- getOption("gSD.api")$gnss
      user <- "gnssguest"
      pw <- "gnssguest"
    }
  }
  return(c(user, pw, url, x))
}


#' get odata for uuid
#'
#' @param uuid one or multiple uuids
#' @param cred return of .CopHub_select c(user, password, API ulr)
#' @param field field to be checked
#' @keywords internal
#' @noRd
.get_odata <- function(uuid, cred, field = ""){
  lapply(uuid, function(x) content(gSD.get(paste0(cred[3], "/odata/v1/Products('", x, "')/", field),  cred[1], cred[2])))
}

#' get ERS API key from user input
#'
#' @param username username
#' @param password password
#' @keywords internal
#' @importFrom httr POST content stop_for_status warn_for_status content_type
#' @importFrom utils URLencode
#' @noRd
.ERS_login <- function(username, password, n_retry = 3){
  x <- POST(url = paste0(getOption("gSD.api")$ee, "login"),
            body = URLencode(paste0('jsonRequest={"username":"', username, '","password":"', password, '","authType":"EROS","catalogId":"EE"}')),
            content_type("application/x-www-form-urlencoded; charset=UTF-8"))
  stop_for_status(x, "connect to server.")
  warn_for_status(x)
  v <- content(x)$data
  if(is.null(v)) out("Login failed. Please retry later or call services_avail() to check if USGS services are currently unavailable.", type = 3)
  return(v)
}

#' logout from ERS with API key
#'
#' @param api.key api.key
#' @keywords internal
#' @noRd
.ERS_logout <- function(api.key){
  x <- gSD.get(paste0(getOption("gSD.api")$ee, 'logout?jsonRequest={"apiKey":"', api.key, '"}'))
  stop_for_status(x, "connect to server.")
  warn_for_status(x)
  content(x)$data
}

#' get EE products
#'
#' @param api.key api.key
#' @param wildcard wildcard
#' @keywords internal
#' @noRd
.EE_ds <- function(api.key, wildcard = NULL){
  q <- paste0(getOption("gSD.api")$ee, 'datasets?jsonRequest={"apiKey":"', api.key, '"}') #, if(is.null(wildcard)) '}' else  ',"datasetName":"', wildcard, '"}')
  if(!is.null(wildcard)) q <- gsub("}", paste0(',"datasetName":"', wildcard, '"}'), q)
  x <- gSD.get(q)
  sapply(content(x)$data, function(y) y$datasetName, USE.NAMES = F)
}


#' query EE
#'
#' @param aoi aoi
#' @param time_range time_range
#' @param product_name name
#' @param api.key api.key
#' @param meta.fields meta.fields
#'
#' @importFrom sf st_bbox st_as_text
#' @importFrom xml2 as_list
#'
#' @keywords internal
#' @noRd
.EE_query <- function(aoi, time_range, product_name, api.key, meta.fields = NULL){
  
  spatialFilter <- paste0('"spatialFilter":{"filterType":"mbr","lowerLeft":{"latitude":', st_bbox(aoi)$ymin, ',"longitude":', st_bbox(aoi)$xmin, '},"upperRight":{"latitude":', st_bbox(aoi)$ymax, ',"longitude":', st_bbox(aoi)$xmax, '}}')
  temporalFilter <- paste0('"temporalFilter":{"startDate":"', time_range[1], '","endDate":"', time_range[2], '"}')
  
  out(paste0("Searching records for product name '", product_name, "'..."))
  query <- lapply(product_name, function(x, ak = api.key, sf = spatialFilter, tf = temporalFilter) gSD.get(paste0(getOption("gSD.api")$ee, 'search?jsonRequest={"apiKey":"', ak,'","datasetName":"', x,'",',sf,',', tf, ',"startingNumber":1,"sortOrder":"ASC","maxResults":50000}')))
  query.cont <- lapply(query, content)
  if(all(c(length(product_name) == 1), query.cont[[1]]$error != "")){
    out("No results could be obtained for this product, time range and AOI.", msg = T)
  } else{
    
    query.use <- sapply(query.cont, function(x) if(x$error == "" & length(x$data$results) != 0) T else F, USE.NAMES = F)
    query.cont <- query.cont[query.use]
    query.names <- product_name[query.use]
    
    query.results <- lapply(query.cont, function(x) x$data$results)
    if(length(query.results) != 0){
      
      query.df <- unlist(mapply(y = query.results, n = query.names, function(y, n) lapply(y, function(x, ds_name = n){
        x.names <- names(x)
        x.char <- as.character(x)
        
        df <- rbind.data.frame(x.char, stringsAsFactors = F)
        colnames(df) <- x.names
        
        # Make sf polygon filed from spatialFootprint
        spf.sub <- grep("spatialFoot", x.names)
        spf <- unlist(x[spf.sub])
        spf <- as.numeric(spf[grep("coordinates", names(spf))])
        df[,spf.sub] <- st_as_text(.check_aoi(cbind(spf[seq(1, length(spf), by = 2)], spf[seq(2, length(spf), by = 2)]), type = "sf", quiet = T))
        
        df <- cbind.data.frame(df, ds_name, stringsAsFactors = F)
        colnames(df)[ncol(df)] <- "product"
        return(df)
      }), SIMPLIFY = F), recursive = F)
      
      ## Read out meta data
      out("Reading meta data of search results from USGS EarthExplorer...", msg = T)
      meta <- lapply(sapply(query.df, function(x) x$metadataUrl, USE.NAMES = F), function(x) gSD.get(x))
      meta.list <- lapply(meta, function(x) as_list(xml_contents(xml_contents(content(x))[1])))
      meta.val <- lapply(meta.list, function(x) sapply(x, function(y){
        z <- try(y$metadataValue[[1]], silent = T)
        if(inherits(z, "try-error")) NULL else z
      }, USE.NAMES = F))
      meta.name <- lapply(meta.list, function(x) sapply(x, function(y) attributes(y)$name))
      
      ## Define meta fields that are usefull for the query output
      if(is.null(meta.fields)) meta.fields <- unique(unlist(meta.name))
      meta.subs <- lapply(meta.name, function(mnames, mf = meta.fields) unlist(lapply(mf, function(x, mn = mnames) which(x == mn))))
      meta.df <- mapply(FUN = function(v, n, i){
        x <- v[i]
        x <- lapply(x, function(x) if(is.null(x)) "" else x)
        x <- rbind.data.frame(x, stringsAsFactors = F)
        colnames(x) <- gsub(" ", "", n[i])
        return(x)
      }, v = meta.val, n = meta.name, i = meta.subs, SIMPLIFY = F)
      
      query.df <- mapply(q = query.df, m = meta.df, FUN = function(q, m){
        ## apply meaningful order and replace startTime and endTime with meta outputs
        x <- cbind.data.frame(q$acquisitionDate, m, q[,-(1:3)], stringsAsFactors = F)
        colnames(x)[1] <- colnames(q)[1]
        return(x)
      }, SIMPLIFY = F)
      
      return.names <- unique(unlist(lapply(query.df, colnames)))
      return.df <- as.data.frame(stats::setNames(replicate(length(return.names),numeric(0), simplify = F), return.names), stringsAsFactors = F)
      return.df <-  do.call(rbind.data.frame, lapply(query.df, function(x, rn = return.names,  rdf = return.df){
        rdf[1, match(colnames(x), rn)] <- x
        return(rdf)
      }))
      return(return.df)
    } else{
      return(NULL)
    }
  }
}


#' convert MODIS product names # used????
#'
#' @param names names
#' @keywords internal
#' @noRd
.convMODIS_names <- function(names){
  sapply(names, function(x){
    y <- strsplit(x, "_")[[1]]
    y <- y[2:length(y)]
    if(length(y) > 1) y <- paste0(y[1:(length(y)-1)], collapse = "_")
    return(y)
  }, USE.NAMES = F)
}


#' USGS ESPA ordering functon
#'
#' @param id id
#' @param level level
#' @param username username
#' @param password password
#' @param format format
#' @keywords internal
#' @importFrom httr content
#' @noRd
.ESPA_order <- function(id, level = "sr", username, password, format = "gtiff", verbose){
  
  ## check query and abort, if not available
  out("Ordering requested items from ESPA...")
  checked <- lapply(id , function(x, v = verbose){
    r <- gSD.get(paste0(getOption("gSD.api")$espa, "available-products/", x), getOption("gSD.usgs_user"), getOption("gSD.usgs_pass"))
    if(names(content(r)) == "not_implemented") out(paste0("'", x, "': This ID is invalid, as it cannot be found in the ESPA database. Please remove it from input and reexecute."), type = 3)
    list(x, r)
  })
  
  ## group request by collection (single or multi order)
  req.data <- lapply(checked, function(x) c(names(content(x[[2]])), x[[1]]))
  coll <- sapply(req.data, function(x) x[[1]][[1]], USE.NAMES=F)
  coll.uni <- unique(coll)
  out(paste0("Collecting from ", toString(length(coll.uni)), " collection(s) [", paste0(coll.uni, collapse = ", "), "], resulting in ", toString(length(coll.uni)), " order(s)..."))
  req.coll <- lapply(coll.uni, function(x, c = coll, rd = req.data) rd[which(c == x)])
  
  reqlevel <- sapply(strsplit(level, '[, ]+'), function(x) toString(paste0('\"', x, '\"')))
  reqlevel <- paste0(reqlevel, collapse=",")
  
  ## build request
  req.body <- lapply(req.coll, function(x, p = reqlevel, f = format){
    i <- paste0(sapply(x, function(y) y[2], USE.NAMES = F), collapse = '", "')
    paste0('{"', x[[1]][1], '": { "inputs": ["', i, '"], "products": [', p, ']}, "format": "', f, '"}')
  })
  
  ## order
  order <- lapply(req.body, function(x, user = username, pass = password) gSD.post(url = paste0(getOption("gSD.api")$espa, "order/"), username = user, password = pass, body = x))
  order.list <- sapply(order, function(x) content(x)[[1]], USE.NAMES = F)
  out(paste0("Products '", paste0(id, collapse = "', '"), "' have been ordered successfully:"))
  out(paste0("[level = '", level, "', format = '", format, "', order ID(s) '", paste0(order.list, collapse = "', '"), "']."))
  return(order.list)
}


#' USGS ESPA downloading functon
#'
#' @param order.list order.list
#' @param username username
#' @param password password
#' @param file.down file.down
#' @param delay delay
#'
#' @importFrom utils head tail
#'
#' @keywords internal
#' @noRd
.ESPA_download <- function(records, username, password, dir_out, delay = 10, wait_for_espa = NULL, n.retry = 3){
  
  records$download_success <- F
  records$download_attempts <- NA
  continue <- T
  while(!all(records$download_success) & isTRUE(continue)){
    
    ## get tiems
    items <- unlist(unlist(lapply(unique(records$ESPA_orderID), function(x){
      content(gSD.get(paste0(getOption("gSD.api")$espa, "item-status/", x), username, password))
    }), recursive = F), recursive = F)
    records$md5_url <- records$dataset_url <- NA
    
    ## get tiems
    records <- do.call(rbind, lapply(1:nrow(records), function(i){
      x <- records[i,]
      x.item <- items[[grep(x$displayId, items)]]
      x$ESPA_status <- x.item$status
      
      if(x$ESPA_status == "complete"){
        x$md5_url <- x.item$cksum_download_url
        x$md5_checksum <- sapply(x$md5_url, function(url) strsplit(content(gSD.get(x$md5_url), as = "text", encoding = "UTF-8"), " ")[[1]][1], USE.NAMES = F)
        x$dataset_url <- x.item$product_dload_url
      }
      return(x)
    }))
    
    ## download available records
    if(nrow(records[records$ESPA_status == "complete" & records$download_success == F,]) > 0){
      records[records$ESPA_status == "complete" & records$download_success == F,] <- gSD.retry(
        records[records$ESPA_status == "complete" & records$download_success == F,], gSD.download,
        names = colnames(records), prog = getOption("gSD.verbose"), force = T, n.retry = n.retry)
    }
    
    if(!all(records$download_success)){
      if(is.null(wait_for_espa)){
        out("Some datasets are not ready to download from ESPA yet.", msg = T)
        out("If getLandsat_data should not continue checking ESPA, it will return the records of all requested datasets (including their ESPA order IDs). Use the returned records to call getLandsat_data again later to continue checking/downloading from ESPA.")
        wait_for_espa <- readline("Should getLandsat_data wait for all datasets to be ready for download? [y/n]: ")
        if(wait_for_espa == "n") wait_for_espa <- F else wait_for_espa <- T
      }
      if(isTRUE(wait_for_espa)){
        out(paste0("Waiting for dataset(s) '", paste0(records[records$ESPA_status == "complete" & records$download_success == F,]$dataset_name, collapse = "', "), "' to be ready for download from ESPA (this may take a while)..."))
        Sys.sleep(delay) #wait before reconnecting to ESPA to recheck status
      } else{
        continue <- FALSE
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
  
  # set-up column name dictionary
  dict <- rbind.data.frame(c("product", NA, NA, NA),
                           c("product_group", NA, NA, NA),
                           c("record_id", "title", "displayId", "displayId"),
                           c("entity_id", "uuid", "entityId", "entityId"),
                           c("dataset_url", "url", NA, NA),
                           c("md5_url", "url.alt", NA, NA),
                           c("preview_url", "url.icon", "browseUrl", "browseUrl"),
                           c("meta_url", NA, "metadataUrl", "metadataUrl"),
                           c("meta_url_fgdc", NA, "fgdcMetadataUrl", "fgdcMetadataUrl"),
                           c("summary", "summary", "summary", "summary"),
                           c("date_acquisition", NA, "acquisitionDate", "acquisitionDate"),
                           c("start_time", "beginposition", "StartTime", "AcquisitionStartDate"),
                           c("stop_time", "endposition", "StopTime", "AcquisitionEndDate"),
                           c("date_ingestion", "ingestiondate", NA, NA),
                           c("date_modified", NA, "modifiedDate", "modifiedDate"),
                           c("tile_number_horizontal", NA, "WRSPath", "HorizontalTileNumber"),
                           c("tile_number_vertical", NA, "WRSRow", "VerticalTileNumber"),
                           c("tile_id", "tileid", NA, NA),
                           c("cloudcov", "cloudcoverpercentage", "SceneCloudCover", NA),
                           c("sensor_id", "instrumentshortname", "SensorIdentifier", NA),
                           c("sensor", "instrumentname", NA, NA),
                           c("platform", "platformname", NA, NA),
                           c("platform_serial", "platformserialidentifier", NA, NA),
                           c("platform_id", "platformidentifier", NA, NA),
                           c("level", "processinglevel", NA, NA),
                           c("levels_available", NA, "levels_available", NA),
                           c("footprint", "footprint", "footprint", "footprint"), stringsAsFactors = F)
  colnames(dict) <- c("gSD", "Sentinel", "Landsat", "MODIS")
  which.col <- sapply(tolower(colnames(dict)), grepl, tolower(product_name), USE.NAMES = F)
  which.valid <- !is.na(dict[,which.col])
  
  # translate column names
  colnames(records) <- sapply(colnames(records), function(x){
    i <- which(x == dict[,which.col])
    if(length(i) > 0) dict$gSD[i] else x
  }, USE.NAMES = F)
  
  # fill group
  records$product_group <- colnames(dict)[which.col]
  
  # address product-specific cases
  records$product <- product_name
  if(which(which.col) == 2){ # special cases for Sentinel
    records$date_acquisition <- sapply(strsplit(records$start_time, "T"), '[', 1)
    records$tile_id[is.na(records$tile_id)] <- sapply(strsplit(records$record_id[is.na(records$tile_id)], "_"), function(x){
      gsub("T", "", x[nchar(x) == 6 & substr(x, 1, 1) == "T"])
    })
  }
  
  if(which(which.col) > 2){
    records <- records[,-sapply(c("ordered", "bulkOrdered", "orderUrl", "dataAccessUrl", "downloadUrl", "cloudCover"), function(x) which(x == colnames(records)), USE.NAMES = F)]
  }
  # fill up undefined default columns
  #records[dict$gSD[sapply(dict$gSD, function(x) !(x %in% colnames(records)), USE.NAMES = F)]] <- NA
  
  # sort columns
  records[,c(na.omit(match(dict$gSD, colnames(records))), which(!(colnames(records) %in% dict$gSD)))]
}

#' converts everything in a data.frame that is not of class c("character","numeric","integer","logical")
#' to character
#' @param x data.frame.
#' @return data.frame as x but only with columns of classes c("character","numeric","integer","logical").
#' @keywords internal
#' @noRd
.df_dates_to_chars <- function(x) {
  to_char <- sapply(names(x), function(name) {
    if(isFALSE(inherits(x[[name]], c("character", "numeric", "integer", "logical", "sfc")))) {
      x[name] <- as.character(x[,name])
    }
  })
  return(x)
}

#' On package startup
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname){
  
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
    gSD.dhus_user = FALSE,
    gSD.dhus_pass = FALSE,
    gSD.dhus_set = FALSE,
    gSD.usgs_user = FALSE,
    gSD.usgs_pass = FALSE,
    gSD.usgs_set = FALSE,
    gSD.usgs_apikey = FALSE,
    gSD.archive = FALSE,
    gSD.archive_set = FALSE,
    gSD.aoi = FALSE,
    gSD.aoi_set = FALSE
  )
  toset <- !(names(op.gSD) %in% names(op))
  if(any(toset)) options(op.gSD[toset])
  ## allocate gdal on load
  gdalUtils::gdal_setInstallation(rescan = T)
  
  invisible()
}

#' On package unload (logouts)
#' @keywords internal
#' @noRd
.onUnload <- function(libname, pkgname) {
  
  ## logout from USGS
  if(isTRUE(getOption("gSD.usgs_set"))) .ERS_logout(getOption("gSD.usgs_apikey"))
}

# -------------------------------------------------------------
# data.frame utils
# -------------------------------------------------------------

#' unlists all columns of a data.frame
#' @param records data.frame.
#' @param records data.frame with all columns unlisted
#' @keywords internal
#' @noRd
.unlist_df <- function(records) {
  for (i in 1:NCOL(records)) {
    if (inherits(records[,i], "list")) {
      records[,i] <- unlist(records[,i])
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
#' @importFrom raster area
#' @importFrom methods as
#' @keywords internal
#' @noRd
.calc_aoi_area <- function(aoi) {
  
  spoly <- "SpatialPolygons"
  if (class(aoi)[1] != spoly) {
    aoi_sp <- as(aoi, spoly)
  } else {aoi_sp <- aoi}
  aoi_area <- raster::area(aoi_sp) / 1000000
  return(aoi_area)
  
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
  
  if (is.null(aoi_ncell)) aoi_ncell <- .calc_aoi_corr_vals(aoi,x)
  
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
#' @importFrom raster extent raster res crs values<- mask
#' @keywords internal
#' @noRd
.calc_aoi_corr_vals <- function(aoi,x) {
  
  # calculate aoi number of cells (calculation is suitable for large areas)
  e <- extent(aoi)
  # calculate area of aoi in order to get a suitable resolution for percentage cells computations
  r <- raster(xmn=e[1],xmx=e[2],ymn=e[3],ymx=e[4],crs=crs(x),resolution=res(x))
  values(r) <- as.integer(1)
  r <- mask(r,aoi)
  r_vals <- getValues(r)
  aoi_npixels <- length(which(r_vals == 1))
  return(aoi_npixels)
  
}

#' checks if records data.frame has SAR records (Sentinel-1) and if all records are SAR
#' @param sensor character vector of all sensors in records.
#' @return \code{has_SAR} numeric 1 for TRUE, 2 for FALSE, 100 for "all".
#' @keywords internal
#' @noRd
.has_SAR <- function(sensor) {
  
  sentinel1 <- name_product_sentinel1()
  if (sentinel1 %in% sensor) {
    has_SAR <- ifelse(all(sensor == sentinel1),100,1)
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
    
  TILEID <- name_tile_id()
  
  # first, try using the horizontal / vertical columns
  tileid_not_given <- is.na(records$tile_id)
  vertical_given <- !is.na(records$tile_number_vertical)
  horizontal_given <- !is.na(records$tile_number_horizontal)
  use_tile_num <- which((tileid_not_given * vertical_given * horizontal_given) == 1) 
  horizontal <- records$tile_number_horizontal[use_tile_num]
  vertical <- records$tile_number_vertical[use_tile_num]
  records[use_tile_num, TILEID] <- paste0(horizontal, vertical)

  # in many cases now value is given in horizontal / vertical
  # ensure that this is given in all cases, sensor-specifically
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
          footprint <- footprint[[1]][[1]]
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

#' creates a character date column in the format "YYYY-MM-DD" for Sentinel records
#' @param records data.frame.
#' @param date_col_orig character name of the original date colum.
#' @param date_col_name character name of the added date column.
#' @return \code{records} data.frame with added column "date_clear".
#' @keywords internal
#' @noRd
.extract_clear_date <- function(records, date_col_orig, date_col_name) {
  
  records[[date_col_name]] <- records[[date_col_orig]]
  sent_recs <- which(records$product_group==name_product_group_sentinel())
  for (i in sent_recs) {
    records[i,date_col_name] <- as.character(substr(records[i,date_col_name],1,10))
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
  period <- c(dates_sorted[1],tail(dates_sorted,1))
  return(period)
}

#' calculates the number of days between two dates
#' @param period character vector of start and end date.
#' @return \code{days} numeric number of days between.
#' @keywords internal
#' @noRd
.period_days <- function(period) {
  days <- as.integer(as.Date(period[2]) - as.Date(period[1]))
  return(days)
}

# -------------------------------------------------------------
# raster utils
# -------------------------------------------------------------

#' mask the edges of Landsat preview raster
#' @param preview raster.
#' @return \code{preview_masked} masked preview
#' @importFrom methods as slot slot<-
#' @importFrom raster mask crs extent crs<-
#' @keywords internal
#' @noRd
.landsat_preview_mask_edges <- function(preview) {
  
  polygons <- "polygons"
  COORDS_SLOT <- "coords"
  ext <- try(extent(preview))
  if (inherits(ext,"try-error")) return (preview)
  poly <- as(ext,"SpatialPolygons")
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
  preview_masked <- mask(preview,poly)
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
#' @param mode character specifies the mode of calculation.
#' @param custom numeric vector with two values: [1] are e.g. cloud values [2] are e.g. non-cloud values. Only if mode == "custom".
#' @param aoi aoi.
#' @param aoi_ncell integer number of cells in aoi.
#' @return \code{percent} numeric percentage
#' @keywords internal
#' @importFrom raster as.matrix extent res crs
#' @noRd
.raster_percent <- function(x, mode = "na", custom = NULL, aoi = NULL, aoi_ncell = NULL) {
  
  if (mode == "na") {
    na_mask <- is.na(x)
    x <- mask(na_mask,aoi)
    x_mat <- as.integer(as.matrix(x))
    # clouds = 1 and clear = 0 now
    percent <- (length(which(x_mat == 1)) / length(which(!is.na(x_mat)))) * 100
  } else if (mode == "custom") {
    x_mat <- as.integer(as.matrix(x))
    val1 <- length(which(x_mat == custom[[1]]))
    val2 <- length(which(x_mat == custom[[2]]))
    percent <- (val1 / sum(val1,val2)) * 100
  } else if (mode == "aoi") {
    percent <- .calc_aoi_coverage(x,aoi,aoi_ncell)
  }
  # due to the calculation based on pixel values it might happen that percent 
  # exceeds 100 slightly. In these cases use 100
  percent <- ifelse(percent > 100,100,percent)
  
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
  # do not reduce the resolution to the equivalent of double the Sentinel-2 preview resolution
  if (target_res > 0.0042) target_res <- 0.004 
  adj <- target_res / res_ref
  adj <- ifelse(adj < 2 && adj > 1, 2, adj)
  if (adj > 1) {
    x_adj <- sapply(1:length(x),function(i) {
      r_save_path <- file.path(dir_out,paste0(x_names[i],"_aggr.tif"))
      if (file.exists(r_save_path)) return(r_save_path)
      r_load <- stack(x[[i]])
      r_aggr <- aggregate(r_load,adj)
      writeRaster(r_aggr,r_save_path,overwrite=T,datatype=dataType(r_load))
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
                                seperate=F,overwrite=T,datatype=datatype))

  if (inherits(write_mos,"try-error")) {
    return(NA)
  } else {
    mos <- raster(save_path)
    if (mode == "rgb") return(mos) else mos <- mos==1
  }
  
}

#' wrapper for reading a raster brick via raster::brick(). Mainly for unit tests.
#' @param character absolute file_path
#' @return RasterBrick
#' @importFrom raster brick
#' @keywords internal
#' @noRd
.read_brick <- function(file_path) {
  return(brick(file_path))
}

#' wrapper for subsetting a raster brick to band 1 and band 3. For unit test.
#' @param RasterBrick or RasterStack b
#' @return RasterBrick with band 1 and band 3
#' @importFrom raster brick
#' @keywords internal
#' @noRd
.subset_brick <- function(b) {
  return(brick(b[[1]], b[[3]]))
}

#' calculates the variance along z dimension
#' @param RasterBrick or RasterStack
#' @return RasterLayer variance along z dimension
#' @keywords internal
#' @noRd
.calc_ndim_var <- function(array) {
  ndim <- nlayers(array)
  squared_sum <- array[[1]]^2
  for (i in 2:ndim) {
    squared_sum <- squared_sum + array[[i]]^2
  }
  sum <- array[[1]]
  for (i in 2:ndim) {
    sum <- sum + array[[i]]
  }
  var <- (squared_sum / (sum / ndim))^2
  return(var)
}

#' calculates the standard deviation of an RGB raster stack at z dimension
#' @param x RasterStack
#' @return sd RasterLayer
#' @keywords internal
#' @noRd
.ndim_sd <- function(x) {
  xmean <- (x[[1]] + x[[2]] + x[[3]]) / 3
  stdev <- sqrt(((preview[[1]] - xmean)^2 + (preview[[2]] - xmean)^2 + (preview[[3]] - xmean)^2) / 3)
  return(stdev)
}

#' calculates the normalized difference of two rasters
#' @param x RasterLayer
#' @param y RasterLayer
#' @return raster layer normalized difference of x and y
#' @keywords internal
#' @noRd
.normalized_difference <- function(x, y) {
  a <- x - y
  b <- x + y
  return(a / b)
}

#' rescales raster to 0-100
#' @param x raster layer
#' @return raster layer rescaled
#' @importFrom raster minValue maxValue
#' @keywords internal
#' @noRd
.rescale_raster <- function(x) {
  return((x - minValue(x)) / (maxValue(x) - minValue(x)) * 100)
}

#' creates NA mask of preview including checks if observations (in aoi) given
#' @param preview raster stack
#' @return preview raster stack
#' @keywords internal
#' @noRd
.mask_preview_na <- function(preview, aoi) {
  
  MIN_DN <- 5 # for NA masking
  MIN_BROKEN <- 20 # for checks if no observations with DN >= 20
  
  # Check if preview is broken
  is_broken <- .check_preview(preview)
  
  # Check for valid observations in aoi
  if (!is_broken) {
    prevMasked <- mask(preview,aoi)
    maxValPrevMasked <- maxValue(prevMasked)
    # if max value smaller 20: no valid observations
    not_valid_in_aoi <- maxValPrevMasked[1] < MIN_BROKEN || is.na(maxValPrevMasked[1])
  }
  
  # if preview is broken or nor valid observations in aoi return NA
  if (isTRUE(any(c(is_broken,not_valid_in_aoi)))) {
    return(NA)
  } else {
    # mask NA values in preview (considered as RGB DN < 5 here)
    NA_mask <- (preview[[1]] > MIN_DN) * (preview[[2]] > MIN_DN) * (preview[[3]] > MIN_DN)
    preview <- mask(preview, NA_mask, maskvalue=0)
    return(preview)
  }
  
}

# -------------------------------------------------------------
# vector utils
# -------------------------------------------------------------

#' wrapper for reading shapefile via sf::read_sf(). Mainly for unit tests.
#' @param Character absolute file_path to shp including extension (".shp")
#' @return SpatialPolygons shp
#' @importFrom sf read_sf as_Spatial
#' @importFrom methods as
#' @keywords internal
#' @noRd
.read_shp <- function(file_path) {
  shp <- as(as_Spatial(read_sf(file_path)), "SpatialPolygons")
  return(shp)
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
#' @param x list data.frame.
#' @return x list without NULLs and NAs.
#' @keywords internal
#' @noRd
.gsd_compact <- function(x) {
  
  if (inherits(x, "list")) {
    not_na <- sapply(x,function(y) {return((!is.na(y) && !is.null(y)))})
    x <- x[not_na]
  }
  return(x)
  
}

#' returns TRUE if a vector or list has length 0/is.null() or is.na()
#' @param x vector of any type
#' @return logical
#' @keywords internal
#' @noRd
.is_empty_array <- function(x) {
  return(length(x) == 0 || is.null(x) || is.na(x))
}

#' evaluate records footprints after csv read (they get wasted when writing to csv)
#' @param records data.frame read from csv
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
    is_sfc <- inherits(f, "sfc")
    if (is_sfc) {
      footprints[[i]] <- f[[1]]
    } else {
      footprint_eval <- unlist(eval(parse(text = footprint)))
      ncol <- 2
      nrow <- length(footprint_eval) / ncol
      m <- matrix(data = footprint_eval, nrow = nrow, ncol = ncol)
      footprints[[i]] <- st_multipolygon(list(list(m)))
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


