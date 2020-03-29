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

#' get column names needed for running calc_hot_cloudcov
#' @return character vector needed_cols
#' @keywords internal
#' @noRd
.get_needed_cols_calc_cloudcov <- function() {
  return(c("product", "product_group", "record_id", "sensor", "cloudcov", "preview_url"))
}

#' get column names needed for running select_*
#' @return character vector needed_cols
#' @keywords internal
#' @noRd
.get_needed_cols_select <- function() {
  needed_cols_cloudcov <- .get_needed_cols_calc_cloudcov()
  needed_cols_cloudcov <- append(needed_cols_cloudcov, .cloudcov_colnames())
  return(append(needed_cols_cloudcov, c("preview_file")))
}


#' unlists all columns of a data.frame
#' @param records data.frame.
#' @param records data.frame with all columns unlisted
#' @keywords internal
#' @noRd
.unlist_df <- function(records) {
  
  is_list <- which(sapply(1:NCOL(records),function(i) {
    return(class(records[,i]))
  }) == "list")
  if (length(is_list) > 0) {
    for (i in is_list) {
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

#' converts everything in a data.frame that is not of class c("character","numeric","integer","logical")
#' to character
#' @param x data.frame.
#' @return data.frame as x but only with columns of classes c("character","numeric","integer","logical").
#' @keywords internal
#' @noRd
.df_dates_to_chars <- function(x) {
  
  classes <- sapply(x,class)
  to_char <- which(!classes %in% c("character","numeric","integer","logical"))
  for (i in to_char) {
    x[,i] <- as.character(x[,i])
  }
  
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
  sumDataDownload <- round(meanPreviewSize * stillToGoFor, 2)
  out(paste0(sep(),"\n\n10 records are processed.\nTime for remaining records, approx.: ",
             sumProcessingTime,"\nData amount approx.: ",sumDataDownload," MB\n",sep(),"\n"))
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
    aoi_cProb <- cellStats(HOT_masked,mean) # calculate the mean HOT cloud probability in aoi
  }
  if (isFALSE(reload)) {
    cMask <- mask(cMask,aoi)
    cMask[cMask==0] <- NA
  }
  if (dir_given) { # save cloud mask if desired
    mask_path <- mask_path
    if (!file.exists(mask_path)) writeRaster(cMask,mask_path,overwrite=T,
                                             datatype="INT2S")
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

#' calculates area in aoi in km2
#' @param aoi aoi.
#' @return aoi_area numeric
#' @importFrom raster area
#' @importFrom methods as
#' @keywords internal
#' @noRd
.calc_aoi_area <- function(aoi) {
  
  if (class(aoi)[1] != "SpatialPolygons") {
    aoi_sp <- as(aoi,"SpatialPolygons")
  } else {aoi_sp <- aoi}
  aoi_area <- raster::area(aoi_sp) / 1000000
  
}

#' calculates the number of cells of value 1 covering the aoi
#' @param x raster for which the percentage of value 1 in aoi shall be calculated.
#' @param aoi aoi.
#' @param aoi_ncell list of numerics if the needed values have been calculated already they can
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
#' @importFrom raster extent raster res crs values<- extract mask
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

#' disaggregates a Landsat or MODIS preview or preview cloud mask to the resolution of Landsat or Sentinel-2
#' @param x raster layer to be disaggregated
#' @param x_sensor character name of sensor to be disaggregated. Can be "Landsat" or "MODIS" or "Sentinel-3".
#' @param y_sensor character name of sensor to which x shall be disaggregated.
#' @return \code{x_dis} raster layer disaggregated.
#' @importFrom raster disaggregate res
#' @keywords internal
#' @noRd
.disaggr_raster <- function(x, x_sensor, y_sensor, aoi) {
  
  x_sensor <- tolower(x_sensor)
  
  # disaggregation parameters
  res <- mean(res(raster(x[[1]]))) # check the resolution and modify adjustment according to it
  target_res <- 0.0019 # the Sentinel-2 preview resolution
  # do not reduce the resolution to the equivalent of more than double the Sentinel-2 preview resolution
  adj <- res / target_res
  adj <- ifelse(adj < 1,1,adj)
  x_dis <- try(disaggregate(x,adj))
  
  if (inherits(x_dis,"try-error")) return(x) else return(x_dis)
  
}

#### CHECKS that are not input checks

#' checks if a record is supported by calc_cloudcov() or not
#' @param record with one row
#' @return \code{is_supported} logical
#' @keywords internal
#' @noRd
.cloudcov_supported <- function(record) {
  record_id <- tolower(record$record_id)
  product_id <- tolower(record$product)
  supported_modis <- tolower(c("MCD18A1.006", "MCD18A2.006", "MCD19A1.006", "MOD09A1.006", "MOD09CMG.006", 
                               "MOD09GA.006", "MOD09GQ.006", "MOD09Q1.006", "MODOCGA.006", "MYD09A1.006", 
                               "MYD09CMG.006", "MYD09GA.006", "MYD09GQ.006", "MYD09Q1.006", "MYDOCGA.006"))
  supported_modis <- tolower(paste0("MODIS_", supported_modis))
  if (startsWith(product_id, "modis")){
    return(any(startsWith(supported_modis, substr(product_id, 1, 13))))
  } else if (startsWith(product_id, "landsat") || product_id == "sentinel-2") {
    return(TRUE)
  } else if (product_id == "sentinel-3") {
    return(strsplit(record_id, "_")[[1]][2] == "ol")
  } else {
    return(FALSE)
  }
}

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

#' creates a tileid from row/path
#' @param records data.frame.
#' @return \code{records} data.frame with a completely filled tile_id column.
#' @keywords internal
#' @noRd
.make_tileid <- function(records) {
  
  TILEID <- "tile_id"
  RECORDID <- "record_id"
  SENTINEL1 <- "S1"
  SENTINEL3 <- "S3"
  tileid_not_given <- is.na(records$tile_id)
  vertical_given <- !is.na(records$tile_number_vertical)
  horizontal_given <- !is.na(records$tile_number_horizontal)
  use_tile_num <- which((tileid_not_given * vertical_given * horizontal_given) == 1) 
  horizontal <- records$tile_number_horizontal[use_tile_num]
  vertical <- records$tile_number_vertical[use_tile_num]
  records[use_tile_num, TILEID] <- paste0(vertical, horizontal)
  # create a tileid from the record_id in case of Sentinel-1
  
  #is_sentinel1 <- which(startsWith(records$record_id, SENTINEL1))
  #tileids <- sapply(records[is_sentinel1, RECORDID], function(x) {
  #  splitted <- strsplit(x, "_")[[1]]
  #  id <- paste0(splitted[8], splitted[9])
  #})
  #records[is_sentinel1, TILEID] <- tileids
  is_sentinel3 <- which(startsWith(records$record_id, SENTINEL3))
  tileids <- sapply(records[is_sentinel3, RECORDID], function(x){
    sep <- "______"
    if (grepl(sep, x)) {
      splitted <- strsplit(x, sep)[[1]][1]
      splitted1 <- strsplit(splitted, "_")[[1]]
      len <- length(splitted1)
      id <- paste0(splitted1[len-1], splitted1[len])
    } else {
      splitted <- strsplit(x, "_LN1")[[1]]
      splitted1 <- strsplit(splitted, "_")[[1]]
      len <- length(splitted1)
      id <- paste0(splitted1[len-2], splitted1[len-1])
    }
  })
  records[is_sentinel3, TILEID] <- tileids
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
  sent_recs <- which(records$product_group=="Sentinel")
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
  days <- as.integer(as.Date(period[2]) - as.Date(period[1]))
}

#### RASTER HANDLING

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
  
  aoi_area <- .calc_aoi_area(aoi)
  adj <- aoi_area / factor
  res_ref <- mean(res(raster(x[[1]]))) # check the resolution and modify adjustment according to it
  target_res <- 0.0019 * adj # the Sentinel-2 preview resolution * adj is the target res also for Landsat, MODIS
  # do not reduce the resolution to the equivalent of double the Sentinel-2 preview resolution
  if (target_res > 0.0042) target_res <- 0.004 
  adj <- target_res / res_ref
  adj <- ifelse(adj < 2 && adj > 1,2,adj)
  if (adj > 1) {
    x_adj <- sapply(1:length(x),function(i) {
      r_save_path <- normalizePath(file.path(dir_out,paste0(x_names[i],"_aggr.tif")))
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

#' seperator
#' @return character
#' @keywords internal
#' @noRd
sep <- function() {
  sep <- "\n----------------------------------------------------------------"
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
