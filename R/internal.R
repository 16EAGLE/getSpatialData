#' Outputs errors, warnings and messages
#'
#' @param input character
#' @param type numeric, 1 = message/cat, 2 = warning, 3 = error and stop
#' @param msg logical. If \code{TRUE}, \code{message} is used instead of \code{cat}. Default is \code{FALSE}.
#' @param sign character. Defines the prefix string.
#'
#' @importFrom utils flush.console
#' @keywords internal
#' @noRd

out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", flush = FALSE, verbose = getOption("gSD.verbose")){
  if(isTRUE(flush)) flush.console()
  if(is.null(ll)) if(isTRUE(verbose)) ll <- 1 else ll <- 2
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input, call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){ cat(paste0(sign,input), sep = ifelse(isTRUE(flush), " ", "\n"))
    } else{message(paste0(sign,input))}}}}
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
#' @noRd
.ERS_login <- function(username, password){
  x <- POST(paste0(getOption("gSD.api")$ee, 'login?jsonRequest={"username":"', username, '","password":"', password, '","authType":"EROS","catalogId":"EE"}'))
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
#' @param name name
#' @param api.key api.key
#' @param meta.fields meta.fields
#'
#' @importFrom sf st_bbox st_as_text
#' @importFrom xml2 as_list
#'
#' @keywords internal
#' @noRd
.EE_query <- function(aoi, time_range, name, api.key, meta.fields = NULL){
  
  spatialFilter <- paste0('"spatialFilter":{"filterType":"mbr","lowerLeft":{"latitude":', st_bbox(aoi)$ymin, ',"longitude":', st_bbox(aoi)$xmin, '},"upperRight":{"latitude":', st_bbox(aoi)$ymax, ',"longitude":', st_bbox(aoi)$xmax, '}}')
  temporalFilter <- paste0('"temporalFilter":{"startDate":"', time_range[1], '","endDate":"', time_range[2], '"}')
  
  out(paste0("Searching records for product name '", name, "'..."))
  query <- lapply(name, function(x, ak = api.key, sf = spatialFilter, tf = temporalFilter) gSD.get(paste0(getOption("gSD.api")$ee, 'search?jsonRequest={"apiKey":"', ak,'","datasetName":"', x,'",',sf,',', tf, ',"startingNumber":1,"sortOrder":"ASC","maxResults":50000}')))
  query.cont <- lapply(query, content)
  if(length(name) == 1) if(query.cont[[1]]$error != "") out("Invalid query. This dataset seems to be not available for the specified time range.", type = 3)
  query.use <- sapply(query.cont, function(x) if(x$error == "" & length(x$data$results) != 0) T else F, USE.NAMES = F)
  query.cont <- query.cont[query.use]
  query.names <- name[query.use]
  
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
#' @param name product name 
#' @keywords internal
#' @noRd
.translate_records <- function(records, name){
  
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
  which.col <- sapply(tolower(colnames(dict)), grepl, tolower(name), USE.NAMES = F)
  which.valid <- !is.na(dict[,which.col])
  
  # translate column names
  colnames(records) <- sapply(colnames(records), function(x){
    i <- which(x == dict[,which.col])
    if(length(i) > 0) dict$gSD[i] else x
  }, USE.NAMES = F)
  
  # fill group
  records$product_group <- colnames(dict)[which.col]
  
  # address product-specific cases
  records$product <- name
  if(which(which.col) == 2){ # special cases for Sentinel
    records$date_aquistion <- sapply(strsplit(records$start_time, "T"), '[', 1)
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


#' unlists all columns of a data.frame
#' @param records data.frame.
#' @param records data.frame with all columns unlisted
#' @keywords internal
#' @noRd
.unlist_df <- function(records) {
  for (i in 1:NCOL(records)) records[,i] <- unlist(records[,i])
}

#' rbind different dfs
#' @param x list of dfs
#' @keywords internal
#' @noRd
rbind.different <- function(x) {
  
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

#' get column names added in calc_hot_cloudcov
#' @return list of character column names.
#' @keywords internal
#' @noRd
.cloudcov_colnames <- function() {
  
  cols <- list(cloud_mask_path="cloud_mask_file",
               aoi_hot_cc_percent="aoi_HOT_cloudcov_percent",
               scene_hot_cc_percent="scene_HOT_cloudcov_percent")

}


#' calc processing time
#' 
#' @param numRecords numRecords
#' @param i record number in the loop
#' @param processingTime processingTime
#' @param previewSize previewSize
#' @keywords internal
#' @importFrom utils object.size
#' @noRd
.calcHOTProcTime <- function(numRecords,i,processingTime,previewSize) {
  meanProcessingTime <- mean(processingTime)
  meanPreviewSize <- mean(previewSize) / 1000000
  stillToGoFor <- numRecords - 5
  sumProcessingTime <- meanProcessingTime * stillToGoFor
  if (sumProcessingTime < 1) {
    sumProcessingTime <- "less than 1 minute"
  } else if (round(sumProcessingTime) == 1) {
    sumProcessingTime <- "1 minute"
  } else {
    sumProcessingTime <- paste0(round(as.numeric(sumProcessingTime))," minutes")
  }
  sumDataDownload <- meanPreviewSize * stillToGoFor
  out(paste0("\n5 records are processed.\nProcessing time for all remaining records, in sum approx.: ",sumProcessingTime,"\nData amount to be downloaded approx.: ",sumDataDownload," MB\n"))
}

#' fills the record data.frame aoi cloud cover columns with NA cases if cc calculation failed or SAR is given
#' @param record data.frame with one row.
#' @param is_SAR logical if the record is a SAR acquisition. Default is FALSE.
#' @return record data.frame with one row but added columns.
#' @keywords internal
#' @noRd
.handle_cc_skip <- function(record, is_SAR = FALSE, dir_out = NULL) {
  
  if (!is.null(dir_out)) record["cloud_mask_file"] <- "NONE"
  record["aoi_HOT_cloudcov_percent"] <- ifelse(is_SAR,NA,100)
  record["scene_HOT_cloudcov_percent"] <- ifelse(is_SAR,NA,9999)
  return(record)
  
}

#' creates new columns and fills a one line records data.frame with calc_hot_cloudcov results
#' finalizes the cloud mask and saves it
#' @param record data.frame.
#' @param aoi aoi.
#' @param cMask raster cloud mask.
#' @param HOT raster HOT cloud probabilitiy layer.
#' @param scene_cPercent numeric calculated HOT scene cloud cover.
#' @param maskFilename character file path where to save the cloud mask.
#' @param cols list of character column names.
#' @param dir_given logical if a dir_out is given as argument.
#' @return record data.frame with additional columns.
#' @importFrom raster cellStats writeRaster mask
#' @keywords internal
#' @noRd
.record_cloudcov_finish <- function(record, aoi, cMask, HOT, scene_cPercent,
                                    mask_path, cols, dir_given, reload=F) {
  
  aoi_cPercent <- .raster_percent(cMask,aoi=aoi) # calculate the absolute HOT cloud cover in aoi
  if (is.null(HOT)) {
    aoi_cProb <- 9999
  } else {
    HOT_masked <- mask(HOT,aoi)
    aoi_cProb <- raster::cellStats(HOT_masked,mean) # calculate the mean HOT cloud probability in aoi
  }
  if (isFALSE(reload)) {
    cMask <- mask(cMask,aoi)
    cMask[cMask==0] <- NA
  }
  if (dir_given) { # save cloud mask if desired
    if (!file.exists(mask_path)) writeRaster(cMask,mask_path,overwrite=T)
    record[cols$cloud_mask_path] <- mask_path
  }
  
  ##### Add scene, aoi cloud cover percentage and mean aoi cloud cover probability to data.frame
  record[cols$aoi_hot_cc_percent] <- as.numeric(aoi_cPercent)
  record[cols$scene_hot_cc_percent] <- as.numeric(scene_cPercent)
  return(record)
  
}

#' mask the edges of Landsat preview raster
#' @param preview raster.
#' @return \code{preview_masked} masked preview
#' @importFrom methods as slot slot<-
#' @importFrom raster mask crs extent crs<-
.preview_mask_edges <- function(preview) {
  
  ext <- try(extent(preview))
  if (inherits(ext,"try-error")) return (preview)
  poly <- as(ext,"SpatialPolygons")
  crs(poly) <- crs(preview)
  # get the vertices of the extent and modify them
  coords <- slot(slot(slot(poly, "polygons")[[1]], "Polygons")[[1]], "coords")
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
  slot(slot(slot(poly, "polygons")[[1]], "Polygons")[[1]], "coords") <- coords
  preview_masked <- mask(preview,poly)
  return(preview_masked)
  
}

#' handles the aoi input and converts it to sf object if needed
#' @param aoi aoi.
#' @param crs crs.
#' @return \code{aoi}
#' @keywords internal
#' @importFrom sf st_as_sf st_transform
#' @noRd
.handle_aoi <- function(aoi, crs) {
  
  aoi_class <- class(aoi)
  error <- "try-error"
  if (class(aoi)[1] != "sf") aoi <- try(st_as_sf(aoi))
  if (inherits(aoi,error)) out(paste0("Aoi of class '",aoi_class,"' could not be converted to 'sf' object"),3)
  if (as.character(crs(aoi)) != as.character(crs)) aoi <- try(st_transform(aoi,crs))
  if (inherits(aoi,error)) out("Aoi reprojection failed",3)
  return(aoi)
  
}

#' creates a temp dir (tmp_dir) and/or deletes it
#' @param dir_out character directory as parent dir.
#' @param action numeric, 1 for create.
#' @keywords internal
#' @noRd
.tmp_dir <- function(dir_out, action = 2) {
  
  tmp_dir <- file.path(dir_out,"tmp")
  if (dir.exists(tmp_dir)) {unlink(tmp_dir,recursive=T)}
  if (action == 1) {dir.create(tmp_dir)}
  return(tmp_dir)
  
}

#' calculates percentage of a value in a raster or polygon with different modes.
#' @param x raster.
#' @param mode character specifies the mode of calculation.
#' @param custom numeric vector with two values: [[1]] are cloud values [[2]] are non-cloud values. Only if mode == "custom".
#' @param aoi aoi.
#' @return \code{percent} numeric percentage
#' @keywords internal
#' @importFrom raster as.matrix extent res crs
#' @noRd
.raster_percent <- function(x, mode = "na", custom = NULL, aoi = NULL) {
  
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
    percent <- .calc_aoi_coverage(x,aoi)
  }
  # due to the calculation based on pixel values it might happen that 'percent' exceeds 100 slightly. In these cases use 100
  percent <- ifelse(percent > 100,100,percent)

}

#' calculates area in aoi in km2
#' @param aoi aoi.
#' @return aoi_area numeric
#' @importFrom raster area
#' @importFrom methods as
#' @keywords internal
#' @noRd
.calc_aoi_area <- function(aoi) {
  
  if (class(aoi) != "SpatialPolygons") {
    aoi_sp <- as(aoi,"Spatial")
  } else {aoi_sp <- aoi}
  aoi_area <- raster::area(aoi_sp) / 1000000
  
}

#' calculates the number of cells of value 1 covering the aoi
#' @param x raster with the resolution.
#' @param aoi aoi.
#' @return \code{percent} numeric percentage of value 1 covering the aoi
#' @importFrom raster extent raster res mask ncell values<- area aggregate extract
#' @keywords internal
#' @noRd
.calc_aoi_coverage <- function(x,aoi) {
  
  # calculate aoi number of cells (calculation is suitable for large areas)
  e <- extent(aoi)
  # calculate area of aoi in order to get a suitable resolution for percentage cells computations
  aoi_area <- .calc_aoi_area(aoi)
  correction <- aoi_area / 100000 # correction for resolution
  correction_small <- correction < 1
  correction <- ifelse(correction_small,1,correction)
  r <- raster(xmn=e[1],xmx=e[2],ymn=e[3],ymx=e[4],crs=crs(x),resolution=(res(x)*correction))
  values(r) <- as.integer(1)
  aoi_npixels <- length(extract(r,aoi)[[1]])
  aoi_ncell <- aoi_npixels * (correction^2)
  if (correction_small) {
    x_aggr <- x
  } else {
    x_aggr <- aggregate(x,correction)
  }
  # calculate number of cells with value 1 in aoi
  x_aggr_npixels <- extract(x_aggr,aoi)[[1]]
  x_aggr_valid <- length(x_aggr_npixels[!is.na(x_aggr_npixels)])
  x_ncell <- ifelse(correction_small,x_aggr_valid,x_aggr_valid * (correction^2))
  
  # calculate percentage of pixels with value 1 in aoi
  percent <- (x_ncell / aoi_ncell) * 100
  
}

#' disaggregates a Landsat or MODIS preview or preview cloud mask to the resolution of Landsat or Sentinel-2
#' @param x raster layer to be disaggregated
#' @param x_sensor character name of sensor to be disaggregated. Can be "Landsat" or "MODIS".
#' @param y_sensor character name of sensor to which x shall be disaggregated.
#' @return \code{x_dis} raster layer disaggregated.
#' @importFrom raster disaggregate
#' @keywords internal
#' @noRd
.disaggr_raster <- function(x, x_sensor, y_sensor) {
  
  options <- list(s2="Sentinel-2",s3="Sentinel-3",l="Landsat",m="MODIS")
  if (x_sensor == options$l && y_sensor == options$s2) {
    adj <- 3.028734
  } else if (x_sensor == options$m && y_sensor == options$s2) {
    adj <- 3.028734 # adjust it still
  } else if  (x_sensor == options$s3 && y_sensor == options$s2) {
    adj <- 3.028734 # adjust it still
  } else if (x_sensor == options$s3 && y_sensor == options$l) {
    adj <- 3.028734 # adjust it still 
  }
  x_dis <- raster::disaggregate(x,adj)
  
}

#### CHECKS that are not input checks

#' checks if records data.frame has SAR records (Sentinel-1) and if all records are SAR
#' @param sensor character vector of all sensors in records.
#' @return \code{has_SAR} numeric 1 for TRUE, 2 for FALSE, 100 for "all".
#' @keywords internal
#' @noRd
.has_SAR <- function(sensor) {
  
  if ("Sentinel-1" %in% sensor) {
    has_SAR <- ifelse(all(sensor == "Sentinel-1"),100,1)
  } else {
    has_SAR <- 0
  }
  
}

#' catches the case where the records data.frame of a sub-period is empty.
#' @param records data.frame.
#' @param ts numeric which timestamp.
#' @return nothing. Console communication.
#' @keywords internal
#' @noRd
.catch_empty_records <- function(records, ts) {
  
  if (NROW(records) == 0) {
    out(paste0("No records at timestamp: ",ts,". You could e.g.:\n
               - decrease 'num_timestamps',
               - decrease 'min_distance',
               - increase 'max_period',
               - add another sensor.\n"),2)
  }
  
}

#### HELPERS

#' creates a character date column in the format "YYYY-MM-DD" for Sentinel records
#' @param records data.frame.
#' @param date_col_orig character name of the original date colum.
#' @param date_col_name character name of the added date column.
#' @return \code{records} data.frame with added column "date_clear".
#' @keywords internal
#' @noRd
.extract_clear_date <- function(records, date_col_orig, date_col_name) {
  
  records[[date_col_name]] <- records[[date_col_orig]]
  sent_recs <- which(records$sensor_group=="Sentinel")
  for (i in sent_recs) {
    records[i,date_col_name] <- as.character(substr(records[i,date_col_name],1,10))
  }
  return(records)
  
}

#' returns the smallest and largest date of a character vector of dates.
#' @param dates character vector of dates ("2019-01-01").
#' @return \code{period} character vector of two dates
#' @keywords internal
#' @noRd
.identify_period <- function(dates) {
  
  dates_sorted <- sort(dates)
  period <- c(dates_sorted[1],tail(dates_sorted,1))
  
}

#' calculates the number of days between two dates
#' @param period character vector of start and end date.
#' @return \code{days} numeric number of days between.
#' @keywords internal
#' @noRd
.period_days <- function(period) {
  days <- as.numeric(as.Date(period[2]) - as.Date(period[1]))
}

#' creates clean tile ids
#' @param records data.frame.
#' @param identifier numeric identifier column.
#' @return \code{records} data.frame with an added column: 'tile_id'
#' @keywords internal
#' @noRd
.make_tileid <- function(records, identifier) {
  
  sensor_groups <- unique(records$product_group)
  for (s in sensor_groups) {
    if (s == sensor_groups[1]) {
      titles <- records[[identifier]][which(records$product_group==s)]
      tile_id <- sapply(titles,function(x) {return(substr(x,39,44))})
    } else if (s %in% sensor_groups[2:3]) {
      tile_id <- paste0(records$WRSPath,records$WRSRow)
    }
  }
  records[["tile_id"]] <- unlist(tile_id)
  return(records)
  
}

#' checks if an new coverage percentage exceeds the min_improvement argument
#' @param min_improvement numeric.
#' @param cov_init numeric.
#' @param cov_aft numeric.
#' @return exceeds logical.
#' @keywords internal
#' @noRd
.exceeds_min_improvement <- function(min_improvement, cov_init, cov_aft) {
  return(cov_aft >= (cov_init + (((100 - cov_init) / 100) * min_improvement)))
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

#### RASTER HANDLING

#' masks a raster with masks of 1 and NA (remain with pixels where mask == 1)
#' @param x raster to be masked.
#' @param mask raster mask with 1 for valid pixels and NA for pixels to be set NA.
#' @return \code{x} masked raster
#' @keywords internal
#' @noRd
.mask_raster <- function(x, mask) {
  x[is.na(mask)] <- NA
  return(x)
}

#' aggregates a raster according to the aoi area size.
#' @param x character vector of paths to rasters to check on. All have to have
#' the same resolution.
#' @param x_names character vector of names refering to x.
#' @param aoi aoi.
#' @param factor numeric adjustment for aoi_area resulting in an adjustment 
#' @param dir_out character directory where to save adjusted rasters if necessary
#' @return x_adj or x (if nothing modified in data) characer vector of paths to (aggregated) rasters.
#' @importFrom raster raster aggregate
#' @keywords internal
#' @noRd
.aggr_raster <- function(x, x_names, aoi, factor = 500000, dir_out) {
  
  aoi_area <- .calc_aoi_area(aoi)
  adj <- aoi_area / factor
  res_ref <- raster(x[[1]]) # check the resolution and modify adjustment according to it
  target_res <- 0.00023 * adj # the Sentinel-2 preview resolution * adj is the target res 
  adj <- target_res / res_ref
  if (adj > 1) {
    x_adj <- sapply(1:length(x),function(i) {
      curr <- x[[i]]
      r_load <- raster(curr)
      r_aggr <- aggregate(r_load,adj)
      r_save_path <- file.path(dir_out,paste0(x_names[i],"_aggr.tif"))
      writeRaster(r_aggr,r_save_path,overwrite=T)
      return(r_save_path)
    })
  } else {
    return(x)
  }
  return(x_adj)
  
}

#### SELECT PREP

#' returns internal params used in select_*
#' @param records data.frame.
#' @param mode character which mode is used: "TS", "BT" or "UT".
#' @return \code{par} list of characters.
#' @keywords internal
#' @noRd
.select_params <- function(records,mode) {
  
  modes <- list("TS"="timeseries","BT"="bitemporal","UT"="unitemporal")
  par <- list(selected_col=paste0("selected_for_",modes[[mode]]), # logical column if a record is selected at all
              pmos_col="rgb_mosaic_file", # path to the RGB mosaic tif where record is included
              cmos_col="cmask_mosaic_file", # path to the cloud mask mosaic tif where record is included
              timestamp_col="selected_for_timestamp", # the timestamp number for which the record is selected
              aoi_cc_col="aoi_HOT_cloudcov_percent",
              tileid_col="tile_id",
              preview_col="preview_file",
              cloud_mask_col="cloud_mask_file",
              date_col="date_acquisition",
              identifier="record_id")
  par$sensor_group <- unique(records$sensor_group)
  par$sensor <- unique(records$sensor)
  #par$date_col_orig <- "date_acquisition"
  par$tileids <- unique(records[[par$tileid_col]])
  par$sep <- sep()
  return(par)
  
}

#' seperator
#' @return character
#' @keywords internal
#' @noRd
sep <- function() {
  sep <- "\n----------------------------------------------------------------"
}

#' prep process of a selection process
#' @param records data.frame.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param par list holding everything inserted into this parameter list in the calling select function (8 parameters).
#' @return records data.frame 
#' @keywords internal
#' @noRd
.select_prep <- function(records, num_timestamps, par) {
  
  #records <- .extract_clear_date(records,par$date_col_orig,par$date_col)
  records <- .make_tileid(records,par$identifier)
  records[[par$date_col]] <- sapply(records[[par$date_col]],as.character)
  period <- .identify_period(records[[par$date_col]])
  # calculates the sub_period column
  records <- .select_sub_periods(records,period,num_timestamps,par$date_col)
  
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
  
  par <- .select_params(records,mode)
  records <- .select_prep(records,num_timestamps,par)
  par$period <- .identify_period(records[[par$date_col]])
  has_SAR <- .has_SAR(par$sensor) # check if SAR records in records (1 for TRUE, 0 for FALSE or 100 for "all"). If 100 selection is done only for SAR
  prep <- list(records=records,
               par=par,
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
  
}

#### SELECT MOSAICKING FUNCTIONS

#' bridge to .make_mosaic
#' @param paths character paths to rasters to be mosaicked.
#' @param aoi aoi.
#' @param save_path save_path (should end with '.tif').
#' @return mos_base_crop.
#' @importFrom raster mask crop
#' @keywords internal
#' @noRd
.select_bridge_mosaic <- function(paths, aoi, save_path) {
  
  mos_base <- .make_mosaic(paths,save_path)
  mos_base_mask <- mask(mos_base,aoi)
  mos_base_crop <- crop(mos_base_mask,aoi)
  writeRaster(mos_base_crop,save_path,overwrite=T)
  return(mos_base_crop)
  
}

#' create mosaic
#' @description The rasters from \code{x} will be mosaicked in a stupid way: everything is mosaicked that is in this list.
#' @param x list of paths to raster files.
#' @param save_path character, full path where to save the mosaic (has to end with '.tif').
#' @param mode character, optional. If mode == "rgb" no masking of the raster to only 1 values is done. 
#' If mode == "mask" the raster will be returned with 1 and NA. Default is mode == "mask".
#' @return \code{mos} raster mosaic
#' @keywords internal
#' @importFrom gdalUtils gdalbuildvrt
#' @noMd
.make_mosaic <- function(x, save_path, mode = "mask") {
  
  write_mos <- gdalbuildvrt(x,save_path,resolution="highest",srcnodata=c("-3.3999999521443642e+38"),vrtnodata="0",
                            seperate=F,overwrite=T)
  mos <- raster(save_path)
  if (mode == "rgb") {
    return(mos)
  } else {
    mos <- mos==1
  }
  
}

#' creates a mosaic of all used cloud masks and writes it as .tif
#' @param s list 'selected' of a timestamp holding everything inserted in select_*().
#' @param aoi aoi.
#' @param dir_out character directory.
#' @return \code{save_path_cmos} character path where cloud mask mosaic is saved
#' @keywords internal
#' @noRd
.select_cmask_mos <- function(s, aoi, dir_out) {
  
  save_path_cmos <- file.path(dir_out,paste0("cloud_mask_mosaic_timestamp",s$timestamp,".tif"))
  cMask_mosaic <- .select_bridge_mosaic(s$cMask_paths,aoi,save_path_cmos)
  return(save_path_cmos)
  
}

#' creates a cloud-masked preview RGB mosaic and writes it as .tif
#' @param records data.frame.
#' @param s list 'selected' of a timestamp holding everything inserted in select_*().
#' @param aoi aoi.
#' @param i numeric index in the loop.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @param dir_out character directory below which to save intermediate product in tmp.
#' @param cloud_mask_col character name of cloud mask path column.
#' @param preview_col character name of the preview path column.
#' @return \code{save_pmos_final} character path where preview RGB mosaic is saved
#' @importFrom raster writeRaster stack mask
#' @keywords internal
#' @noRd
#' @author Henrik Fisser
.select_preview_mos <- function(records, s, aoi, i, identifier, dir_out, cloud_mask_col, preview_col) {
  
  id_sel <- sapply(s$ids,function(x) which(records[,identifier]==x))
  save_pmos <- file.path(dir_out,paste0("preview_mosaic_timestamp",s$timestamp))
  # mask preview tiles by cloud masks
  layers <- c("red","green","blue")
  sensors_given <- unique(records$sensor_group)
  sensors_ref <- c("Sentinel-2","Landsat","Sentinel-3","MODIS")
  tmp_dir <- .tmp_dir(dir_out,action=1)
  preview_paths <- lapply(id_sel,function(i) {
    p_path <- records[i,preview_col] # preview
    if (is.na(p_path) || !file.exists(p_path)) return(NA)
    cMask <- raster(records[i,cloud_mask_col]) # cloud mask
    preview <- stack(p_path)
    # disaggregate preview to the resolution of other sensor in records that has higher resolution
    sensors_adj <- c("landsat","modis","sentinel-3")
    cond <- grepl(tolower(records[i,"sensor_group"]),sensors_adj)
    if (isTRUE(any(cond)) && length(sensors_given) > 1) {
      y_sensor <- sensors_ref[which(sensors_given %in% sensors_ref)]
      x_sensor <- firstup(sensors_adj[which(cond==T)])
      preview_adj <- try(.disaggr_raster(preview,x_sensor=x_sensor,y_sensor=y_sensor))
      #if (inherits(preview_adj,"try-error")) preview_adj <- preview
    }
    preview_aoi <- mask(preview_adj,aoi)
    preview_masked <- .mask_raster(preview_aoi,cMask)
    preview_save <- file.path(tmp_dir,paste0(records[i,identifier],"_cloud_masked"))
    paths_sep <- sapply(1:nlayers(preview_masked),function(j) {
      layer_save <- paste0(preview_save,"_",i,"_",layers[j],".tif")
      writeRaster(preview_masked[[j]],layer_save,overwrite=T)
      return(layer_save)
    })
  })
  preview_paths <- .gsd_compact(preview_paths)
  preview_mos <- lapply(1:length(layers),function(j) {
    curr_layers <- lapply(preview_paths,function(x) path <- x[j])
    save_path_pmos <- paste0(save_pmos,"_",layers[j],".tif")
    pmos <- .select_bridge_mosaic(curr_layers,aoi,save_path_pmos)
  })
  preview_mos_stack <- stack(preview_mos)
  save_pmos_final <- paste0(save_pmos,"_",i,"_rgb.tif")
  writeRaster(preview_mos_stack,save_pmos_final,overwrite=T)
  .tmp_dir(dir_out,action=2)
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
#' @param ts numeric of the current timestamp.
#' @param dir_out character directory where to save intermediate product.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @return selected list of [[1]] character ids of selected records, [[2]] percentage of valid pixels in mosaic.
#' Side effect: creates a dir_tmp, writes into it, deletes dir_tmp with all files.
#' @importFrom raster minValue maxValue writeRaster raster crs crs<-
#' @keywords internal
#' @noRd
#' @author Henrik Fisser
.select_calc_mosaic <- function(records, base_records, aoi, sub, cloud_mask_col, 
                                min_improvement,
                                ts, dir_out, identifier) {
  
  dir_tmp <- .tmp_dir(dir_out,1)
  le_first_order <- length(sub[[1]])
  sub <- unlist(.gsd_compact(sub))
  collection <- sapply(sub,function(x) cmask_path <- as.character(records[[cloud_mask_col]][x])) # get paths to cloud masks. This is the queue for processing
  # this vector are all orders ordered from best records (lowest aoi cc) 
  # to worst records (highest aoi cc) in a queue
  names(collection) <- sapply(sub,function(x) return(records[x,identifier]))
  collection <- collection[which(collection != "NONE")]
  # start mosaicking
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
  # aggregate raster adjusted to aoi area size in order to speed up process
  names <- names(base_records)
  base_records <- .aggr_raster(base_records,names,aoi=aoi,dir_out=dir_tmp)
  names(base_records) <- names
  # this base mosaic will be updated with each added record after check if valid cover is increased
  base_mos_path <- file.path(dir_tmp,"base_mosaic_tmp.tif")
  base_mos <- .select_bridge_mosaic(base_records,aoi,base_mos_path)
  writeRaster(base_mos,base_mos_path,overwrite=T)
  rm(base_mos)
  base_coverage <- -1000
  # add next cloud mask consecutively and check if it decreases the cloud coverage
  for (i in start:length(collection)) {
    if (i == start) {out("Current coverage of valid pixels")}
    x <- collection[i] # do it this way in order to keep id
    # before calculating the next mosaic, 
    # check if record tile is within the area of non-covered pixels at all
    base_mos <- raster(base_mos_path) # current mosaic
    name_x <- names(x)
    x <- .aggr_raster(x,name_x,aoi=aoi,dir_out=dir_tmp)
    names(x) <- name_x
    next_record <- raster(x) # record to be added if it supports the mosaic
    curr_base_mos_crop <- crop(base_mos,next_record) # crop base mosaic to tile area of next
    aoi_subset <- as(extent(next_record),"SpatialPolygons")
    crs(aoi_subset) <- crs(next_record)
    cov_init <- .raster_percent(curr_base_mos_crop,mode="aoi",aoi=aoi_subset)
    crop_p <- file.path(dir_tmp,"crop_tmp.tif")
    curr_mos_tmp_p <- file.path(dir_tmp,"curr_mos_tmp.tif")
    writeRaster(curr_base_mos_crop,crop_p,overwrite=T)
    base_tmp <- c(crop_p,x)
    curr_mos_tmp <- .select_bridge_mosaic(base_tmp,aoi,curr_mos_tmp_p) # in this tile add next_record
    cov_aft <- .raster_percent(curr_mos_tmp,mode="aoi",aoi=aoi_subset) # check new coverage
    rm(next_record,base_mos,curr_base_mos_crop,curr_mos_tmp)
    # calculate by how much valid coverage is improved when adding the record to the tile area
    has_potential <- cov_aft > cov_init # only check for tile
    base_mos_path_new <- file.path(dir_tmp,paste0("base_mosaic_",i,"_tmp.tif")) # do not overwrite base_mos directly
    if (has_potential) {
      base_mos <- .select_bridge_mosaic(base_tmp,aoi,base_mos_path_new) # mosaic with added record
      base_cov_tmp <- .raster_percent(base_mos,mode="aoi",aoi=aoi)
      add_it <- .exceeds_min_improvement(min_improvement,base_coverage,base_cov_tmp)
      if (add_it) {
        base_records <- c(base_records,x) # add save path of current mosaic
        writeRaster(base_mos,base_mos_path,overwrite=T) # overwrite base mosaic
        unlink(base_mos_path_new)
        base_coverage <- base_cov_tmp
        rm(base_mos)
        cov <- as.character(round(base_coverage,2))
        cov <- ifelse(nchar(cov)==5,cov,paste0(cov,"0"))
        out(paste0("\r", "-      ",cov,"  %"), flush = T)
      }
    }
    if (round(base_coverage) == 100) {
      break
    }
  }
  
  # return ids of selected records and percentage of valid pixels of final mosaic
  selected <- list(ids=names(base_records),
                   cMask_paths=base_records,
                   valid_pixels=base_coverage)
  
  del <- .tmp_dir(dir_out)
  return(selected)
}

#' calculates the final cloud mask and preview RGB mosaic per timestamp
#' @param records data.frame.
#' @param selected list of lists, each of the list is one timestamp and holds the ids, timestamp numbers and cloud mask paths.
#' @param aoi aoi.
#' @param selected_col character name of the logical column name in records data.frame (selected TRUE/FALSE).
#' @param pmos_col character name of the column where to enter the paths to the preview mosaics.
#' @param cmos_col character name of the column where to enter the paths to the cloud mask mosaics.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @param dir_out character directory.
#' @return records with four additional columns: selected_col, timestamp_col, pmos_col, cmos_col. Cloud mask and preview mosaics are saved in dir_out.
#' @keywords internal
#' @noRd
#' @author Henrik Fisser
.select_save_mosaics <- function(records, selected, aoi, 
                                 par, dir_out) {
  
  console_info <- list()
  cols <- c(par$selected_col,par$timestamp_col,par$pmos_col,par$cmos_col)
  records <- .select_prep_cols(records, cols, par$selected_col)
  for (i in 1:length(selected)) {
    s <- selected[[i]]
    id_sel <- s$ids
    #A cloud mask mosaic
    save_path_cmos <- .select_cmask_mos(s,aoi,dir_out)
    #B preview mosaic
    save_path_pmos <- .select_preview_mos(records,s,aoi,i,par$identifier,dir_out,
                                          cloud_mask_col=par$cloud_mask_col,preview_col=par$preview_col)
    #C add columns to records
    insert <- c(TRUE,s$timestamp,save_path_pmos,save_path_cmos)
    for (j in 1:length(cols)) {
      records[id_sel,cols[j]] <- insert[j]      
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
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @param par list holding everything inserted into this parameter list in .select_params().
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
                         prio_sensors,
                         par,
                         dir_out,
                         cols_initial) {
  
  # if all are SAR records
  if (has_SAR == 100) {
    records <- .select_all_SAR(records, max_sub_period,
                               min_distance, num_timestamps, par)
    records <- .column_summary(records,cols_initial)
    return(records)
  }
  
  #### Start Process for optical data selection
  selected <- list() # list to be filled by all selected 'record_id' ids, the valid coverage percentage per timestamp and the cloud mask paths
  sub_periods <- unique(records$sub_period)
  # select per sub-period (=timestamp) best mosaic. The sub-periods are adjusted dynamically according to min_distance, max_sub_period
  for (t in 1:length(sub_periods[!is.na(sub_periods)])) {
    selected_ts <- .select_process(records,aoi,
                                   timestamp=t,
                                   min_distance=min_distance, 
                                   max_sub_period=max_sub_period,max_cloudcov_tile=max_cloudcov_tile, 
                                   min_improvement=min_improvement,
                                   par=par,dir_out=dir_out)
    selected_ts$timestamp <- t
    selected[[t]] <- selected_ts # insert 'selected_ts' list into selected list
  }
  
  # if some are SAR records
  if (has_SAR == 1) {
    selected <- .select_some_SAR(records, selected, max_sub_period,
                                 min_distance, num_timestamps, par)
  }
  
  # create resulting mosaics and add columns to records
  #A Create and save final cloud mask mosaic
  #B Create and save final RGB preview mosaic
  #C Add 3 columns to records data.frame:
  #1 logical column if a record is selected at all
  #2 path to the RGB mosaic tif where record is included
  #3 the timestamp number for which the record is selected
  out(paste0(par$sep,"\nSelection Process Summary per timestamp",par$sep))
  # create final mosaics for each timestamp and summary message per timestamp
  records <- .select_save_mosaics(records,selected=selected,aoi=aoi,
                                  par=par,dir_out=dir_out)
  # create optional warning(s) and overall summary message
  csw <- .select_summary_ts(selected)
  w <- csw[2:3] # warnings
  w <- w[which(w!="NULL")]
  summary <- .out_vector(csw[[1]])
  if (length(w) > 0) to_console <- sapply(w,function(x) .out_vector(x,type=2))
  
  records <- subset(records,select=-sub_period) # remove sub-period column
  records <- subset(records,select=-tile_id)
  
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
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @param par list holding everything inserted into this parameter list in .select_params().
#' @param dir_out character directory where to save intermediate product.
#' @param timestamp numeric of the current timestamp.
#' @return \code{selected_ts} list of selected records with all items returned by .select_process_sub
#' @keywords internal
#' @noRd
.select_process <- function(records, aoi,
                            timestamp,
                            min_distance, max_sub_period, max_cloudcov_tile, 
                            min_improvement, prio_sensors = NULL,
                            par, dir_out) {
  
  period_new <- c()
  base_records <- c()
  ids <- c()
  valid_pixels <- 0
  prio_sensors <- ifelse(is.null(prio_sensors) || length(prio_sensors) == 1,1,prio_sensors)
  le_prio_is_one <- length(prio_sensors) == 1
  for (s in prio_sensors){
    if (le_prio_is_one) {
      # in case prio_sensors is not given process all sensors together
      s_match <- which(!is.na(records$product))  
    } else {
      # in case prio_sensors is given process sensors in this order
      s_match <- which(records$product==s)
    }
    sensor_match <- intersect(which(records$sub_period==timestamp),s_match)
    if (length(sensor_match) == 0) { # no records for sensors s at timestamp
      if (le_prio_is_one) .catch_empty_records(data.frame()) else break
    } 
    tstamp <- new.env()
    tstamp$records <- records[sensor_match,]
    tstamp$records <- tstamp$records[which(!is.na(records[[par$preview_col]])),]
    .catch_empty_records(tstamp$records,ts=timestamp)
    tstamp$period <- .identify_period(tstamp$records[[par$date_col]])
    if (timestamp > 1) {
      # enforce to min_distance from previous timestamp
      tstamp$first_date <- .select_force_distance(period_new,min_distance)
      tstamp$period <- .select_handle_next_sub(first_date=tstamp$first_date,
                                               period_initial=tstamp$period,
                                               min_distance,max_sub_period)
      tstamp$records <- .select_within_period(records,tstamp$period,par$date_col) # subset to records in period
    }
    # run the selection process
    selected <- .select_process_sub(tstamp$records,
                                    aoi,
                                    tstamp$period,
                                    period_new=period_new,
                                    base_records=base_records,
                                    max_sub_period,
                                    max_cloudcov_tile,
                                    min_improvement,
                                    par,
                                    dir_out,
                                    ts=timestamp)
    if (isFALSE(le_prio_is_one)) {
      period_new <- selected$period # for selection of records from next sensor in prio_sensors
      base_records <- c(base_records,selected$cmask_paths) # for base mosaic for selection from next sensor
      ids <- c(ids,selected$ids) # ids of selected records
      valid_pixels <- selected$valid_pixels # percentage of valid pixels in aoi
    } else {
      return(selected)
    }
  }
  
  if (length(ids) == 0) .catch_empty_records(data.frame())
  
  selected_ts <- list(ids=ids,
                      cmask_paths=base_records,
                      valid_pixels=valid_pixels)
  return(selected_ts)

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
#' @param par list holding everything inserted into this parameter list in the calling select function (8 parameters).
#' @param dir_out character directory where to save intermediate product.
#' @param ts numeric of the current timestamp.
#' @return \code{selected} list of [[ids]] character vector of selected ids, [[cMask_paths]] character vector to cloud masks, 
#' [[valid_pixels]] percentage of valid pixels in mosaic with the given selection, [[period]] charater vector of two dates.
#' @keywords internal
#' @noRd
.select_process_sub <- function(records, aoi,
                                period, period_new = c(), base_records = NULL,
                                max_sub_period, max_cloudcov_tile, min_improvement, 
                                par, dir_out, ts) {
  
  # the sub is an ordering of all available records per tile according to aoi cloud cover
  # this is also the step where max_cloudcov_tile is ensured
  sub <- .select_sub(records=records,
                     tiles=par$tileids,
                     max_cloudcov_tile=max_cloudcov_tile,
                     aoi_cc_col=par$aoi_cc_col,
                     tileid_col=par$tileid_col,
                     date_col=par$date_col,
                     identifier=par$identifier)
  # this step enforces max_sub_period
  sub_within <- .select_force_period(records,sub,period,max_sub_period,period_new=period_new,
                                     date_col=par$date_col,aoi_cc_col=par$aoi_cc_col)
  # make best mosaic of cloud masks for first timestamp
  out(par$sep)
  out(paste0("Calculating best mosaic for timestamp: ",ts))
  selected <- .select_calc_mosaic(records,
                                  base_records=base_records,
                                  aoi,
                                  sub_within,
                                  par$cloud_mask_col,
                                  min_improvement=min_improvement,
                                  ts=ts,
                                  dir_out,
                                  par$identifier)
  selected$period <- .identify_period(records[records[[par$identifier]]==selected$ids])
  out(paste0("\nCompleted selection process for timestamp: ",ts,"\n"))
  return(selected)
  
}

#' select timestamps for SAR data according to num_timestamps and min_distance. This
#' can be the fundament for an optical selection or if has_SAR == 100 (only SAR in records) it is the only selection
#' @param records data.frame.
#' @param period_new list of character vectors of two dates, one vector for each timestamp.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param max_sub_period numeric maximum length of sub-period.
#' @param par list holding everything inserted into this parameter list in the calling select function.
#' @return \code{SAR_selected} list of [[ids]] character vector of selected ids per timestamp, [[period]] character vector
#' of two dates and [[sub-period]] numeric the sub-period number
#' character vector of two dates (start and end date of SAR sub-period)
#' @keywords internal
#' @noRd
.select_SAR <- function(records, period_new = NULL, 
                        max_sub_period, min_distance, num_timestamps, par) {
  
  subperiods <- unique(records$sub_period)
  # if the length of the current sub-period is longer than max_sub_period grade all dates in sub_period
  # according to the number of the given records for each date and calculate the distance of each record
  # from this date. Exclude records consecutively until max_sub_period is reached
  selected_SAR <- list() # to be filled with the selected lists of selected ids and sub-periods
  for (s in subperiods) {
    records_in_s <- records[which(records$sub_period==s),]
    if (s > 1) {
      # enforce min_distance
      previous_period <- selected_SAR[[s-1]][["period"]] # get the previous selected sub-period
      period_initial <- .identify_period(records_in_s[[par$date_col]])
      # earliest date of next sub-period adjusted
      first_date <- .select_force_distance(previous_period,min_distance)
      period_s <- .select_handle_next_sub(first_date,period_initial,
                                          min_distance,max_sub_period)
      records_in_s <- .select_within_period(records_in_s,period_s) # subset to records within period_s
    }
    tiles_s <- records_in_s[[par$tileid_col]]
    dates_s <- sapply(records_in_s[[par$date_col]],as.Date)
    if (!is.null(period_new)) {
      period_new_dates <- sapply(period_new,as.Date)
      dates_combined <- c(dates_s,period_new_dates)
      dates_s <- min(dates_combined,max(dates_combined))
    }
    min_dates_s <- min(dates_s)
    max_dates_s <- max(dates_s)
    sub_period_le <- length(max_dates_s-min_dates_s)
    # grade dates and exclude consecutively
    if (sub_period_le > max_sub_period) {
      dates_seq <- min_dates_s:max_dates_s
      date_grade <- sapply(dates_seq,function(date) {
        records_match <- which(dates_s==date)
        # include one record per tile for the grading as for SAR several records on one date = no benefit
        grade <- length(unique(records[records_match,par$tileid_col]))
      })
      # calculate best_period based on date_grade, in combination with period_new
      # and while ensuring max_sub_period
      best_period <- .select_best_period(date_grade = date_grade, dates_seq = dates_seq, 
                                         min_date = min(dates_seq), max_date = max(dates_seq),
                                         period_new = period_new,
                                         max_sub_period = max_sub_period)
      if (!is.na(best_period)) {
        incl <- .select_subset_to_best_period(dates_s, best_period)
        ids <- records_in_s[incl,par$identifier] # ids of selected records in upated sub-period
      }
    } else {
      ids <- records_in_s[[par$identifier]] # all ids of records in sub-period
    }
    dates_sel <- records_in_s[which(ids %in% records[[par$identifier]]),par$date_col]
    period <- .identify_period(dates_sel)
    selected_SAR[[s]] <- list("ids"=ids,"period"=period,"sub-period"=s)
  }
  
  return(selected_SAR)

}

#' selection process when only SAR given in records
#' @param records data.frame.
#' @param max_sub_period numeric max_sub_period.
#' @param min_distance numeric min_distance.
#' @param num_timestamps numeric num_timestamps.
#' @param par list par.
#' @return records data.frame ready for return to user
#' @keywords internal
#' @noRd
.select_all_SAR <- function(records, max_sub_period,
                                 min_distance, num_timestamps,
                                 par) {
  
  selected_SAR <- .select_SAR(records,period_new = NULL,max_sub_period,min_distance,
                              num_timestamps,par)
  records <- .select_finish_SAR(records, selected_SAR, num_timestamps, par)
  return(records)
  
}

#' selection process for SAR when some SAR given in records
#' @param records data.frame.
#' @param selected list selected.
#' @param max_sub_period numeric max_sub_period.
#' @param min_distance numeric min_distance.
#' @param num_timestamps numeric num_timestamps.
#' @param par list par.
#' @return selected list of lists where each list is a selected list of one timestamp
#' @keywords internal
#' @noRd
.select_some_SAR <- function(records, selected, max_sub_period,
                                  min_distance, num_timestamps, par) {
  
  # SAR records shall be searched within max_sub_period combined
  # with the periods selected for optical 
  period_new_all <- lapply(selected,function(x) return(x[["period"]]))
  selected_SAR <- .select_SAR(records,period_new_all,max_sub_period,min_distance,
                              num_timestamps,par)
  # add selected ids to selected list of optical records
  for (ts in 1:length(selected_SAR)) {
    ts_SAR <- selected_SAR[[ts]]
    optical_ids <- selected[[ts]][["ids"]]
    selected[[ts]][["ids"]] <- append(optical_ids,ts_SAR[["ids"]])
  }
  return(selected)
  
}

#' finishs a SAR selection where only SAR records were given. Fills the records data.frame
#' return columns and creates final console summary + optional warning.
#' @param records data.frame.
#' @param selected_SAR list of [[ids]] character vector of selected ids per timestamp, [[period]] character vector
#' of two dates and [[sub-period]] numeric the sub-period number
#' @param par list holding everything inserted into this parameter list in the calling select function.
#' @return records data.frame with two added columns, ready for return to user.
#' @keywords internal
#' @noRd
.select_finish_SAR <- function(records, selected_SAR, num_timestamps, par) {
  
  csw_SAR <- .select_SAR_summary(records,selected_SAR,num_timestamps,par)
  summary <- .out_vector(csw_SAR[[1]]) # SAR selection summary
  w <- csw_SAR[[2]]
  if (!is.null(w)) out(w,type=2) # warning
  ids <- sapply(selected_SAR,function(x) {return(x[["ids"]])})
  # add columns to records
  cols <- c(par$selected_col,par$timestamp_col)
  records <- .select_prep_cols(records,cols)
  for (ts in 1:length(ids)) {
    ids_match <- match(ids[ts],records[[par$identifier]])
    records[ids_match,par$selected_col] <- TRUE # is record selected at all
    records[ids_match,par$timestamp_col] <- ts # timestamp for which record is selected
  }
  return(records)
  
}

#### SELECT SUB-PROCESSES

#' selects initial records for the first sub-period while ensuring max_cloudcov_tile
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
  sub <- .gsd_compact(sub)
  max_num_sel <- max(sapply(sub,length))
  orders <- sapply(1:max_num_sel,function(i) unlist(sapply(sub,function(x) return(x[i])))) # to matrix
  orders <- data.frame(orders)
  sub_within <- list()
  for (i in 1:NCOL(orders)) {
    print(i)
    x <- orders[,i]
    # first try to use all records of this order
    order <- x[!is.na(x)]
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
      order_within <- .select_remove_dates(order, records, period_new, max_sub_period, date_col, aoi_cc_col)
      if (!is.na(order_within)) {
        period_new <- c(period_new,.select_bridge_period(records[order_within,date_col],period_new))
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
  best_period <- .select_best_period(date_grade = date_grade, dates_seq = dates_seq, 
                                     min_date = min_date, max_date = max_date,
                                     period_new = period_new,
                                     max_sub_period = max_sub_period)
  if (is.na(best_period)) {
    return(NA)
  } else {
    incl <- .select_subset_to_best_period(dates, best_period)
    order <- x[incl]
    return(order)
  }
  
}

#' removes NULLs and NAs from list.
#' @param x list.
#' @return x list without NULLs and NAs.
#' @keywords internal
#' @noRd
.gsd_compact <- function(x) {
  
  not_na <- sapply(x,function(y) {return((!is.na(y) && !is.null(y)))})
  x <- x[not_na]
  return(x)
  
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
.select_sub_periods <- function(records, period, num_timestamps, date_col) {
  
  period <- sapply(period,as.Date)
  days <- as.numeric(diff(period))
  le_subperiods <- days / num_timestamps
  dates <- sapply(0:num_timestamps,function(i) date <- period[1] + (i * le_subperiods))
  l <- length(dates)
  dates[l] <- dates[l] + 1
  date_col_mirr <- sapply(records[[date_col]],as.Date) # mirror of the date column as days since 1970-01-01
  for (i in 1:num_timestamps) {
    within <- intersect(which(date_col_mirr >= dates[i]),which(date_col_mirr < dates[i+1]))
    records[within,"sub_period"] <- i
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
  
  sep <- "\n----------------------------------------------------------------"
  coverages <- sapply(selected,function(x) {x$valid_pixels})
  num_timestamps <- length(selected)
  min_cov <- round(min(coverages))
  mean_cov <- round(mean(coverages))
  p <- " %"
  header <- paste0("\n-- Selection Process Summary Overall --")
  cov_pixels <- "overage of valid pixels "
  info1 <- paste0("\n- Number of timestamps: ",num_timestamps)
  info2 <- paste0("\n- C",cov_pixels,"in timestamp-wise mosaics of selected records: ")
  info3 <- paste0("\n-    Mean:     ",mean_cov,p)
  info4 <- paste0("\n-    Lowest:   ",min_cov,p)
  info5 <- paste0("\n-    Highest:  ",round(max(coverages)),p)
  console_summary <- paste0(sep,sep,header,sep,info1,info2,info3,info4,info5,sep,"\n")
  
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
#' @param par list holding everything inserted into this parameter list in the calling select function.
#' @return \code{console_summary_warning} list of two character vectors holding the messages:
#' [[1]] Summary info
#' [[2]] Warning if minimum coverage of tiles does not reach number of tiles.
#' @keywords internal
#' @noRd
.select_SAR_summary <- function(records, selected_SAR, num_timestamps, par) {
  
  sep <- "\n----------------------------------------------------------------"
  covered_tiles_ts_wise <- sapply(selected_SAR,function(s) {
    num_tiles <- length(unique(records[match(s[["ids"]],records[[par$identifier]]),par$tileid_col]))
  })
  header <- paste0("\n-- Selection Process Summary Overall --")
  info1 <- paste0("\n- Number of timestamps: ",num_timestamps)
  info2 <- paste0("\n- Number of covered tiles in timestamp-wise SAR mosaics of selected records: ")
  info3 <- c()
  for (i in 1:length(covered_tiles_ts_wise)) {
    num_tiles <- covered_tiles_ts_wise[i]
    info3[i] <- paste0("\n    Timestamp ",i," covers:",num_tiles)
  }
  console_summary <- paste0(sep,sep,header,sep,info1,info2,info3,sep,"\n")
  # check if for all timestamps all tiles are covered
  check_tile_cov <- which(covered_tiles_ts_wise != length(par$tileids))
  # return warning if check_tile_cov has length > 0
  le <- length(check_tile_cov)
  char <- c()
  for (x in check_tile_cov) char <- ifelse(le == 1,x,paste0(char,x,", "))
  warning <- ifelse(le == 0,NULL,paste0("\nFor timestamps: \n   ",char,
                                        "\nnot all tiles could be covered with the given parameters. You could e.g.:\n
                                        - decrease 'num_timestamps',
                                        - decrease 'min_distance',
                                        - increase 'max_period'.")) 
  
  console_summary_warning <- list(console_summary,warning)
}

#' prints character vectors in console combined into one message in out()
#' @param x list of character vectors.
#' @param type numeric as in out().
#' @param msg logical as in out().
#' @return nothing. Console print
#' @keywords internal
#' @noRd
.out_vector <- function(x,type=1,msg=FALSE) {
  
  shout_out <- sapply(x,function(vec) {
    print_out <- sapply(vec,function(v) out(v,type=type,msg=msg))
  })
  
}

#' run silent
#' @param expr an expression
#' @return nothing. runs expression
#' @keywords internal
#' @noRd
quiet <- function(expr){
  return(suppressWarnings(suppressMessages(expr)))
}
