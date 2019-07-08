#' Outputs errors, warnings and messages
#'
#' @param input character
#' @param type numeric, 1 = message/cat, 2 = warning, 3 = error and stop
#' @param msg logical. If \code{TRUE}, \code{message} is used instead of \code{cat}. Default is \code{FALSE}.
#' @param sign character. Defines the prefix string.
#'
#' @keywords internal
#' @noRd

out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", verbose = getOption("gSD.verbose")){
  if(is.null(ll)) if(isTRUE(verbose)) ll <- 1 else ll <- 2
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input, call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){ cat(paste0(sign,input),sep="\n")
    } else{message(paste0(sign,input))}}}}
}

#' first character to upper
#' 
#' @param x character
#' 
#' #' @keywords internal
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

#' Checks, if specific command is available
#'
#' @param cmd command
#' @importFrom devtools system_check
#' @keywords internal
#' @noRd
check.cmd <- function(cmd){
  sc <- try(system_check(cmd, quiet = TRUE),silent = TRUE)
  if(class(sc) == "try-error"){return(FALSE)}else{return(TRUE)}
}


#' gSD.get
#' @param url url
#' @param username user
#' @param password pass
#' @param dir.file output file path
#' @param prog show or not show progress console
#' @importFrom httr GET stop_for_status warn_for_status message_for_status progress
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
#' @importFrom httr POST stop_for_status warn_for_status message_for_status progress
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
gSD.download <- function(x, names, prog = T, force = F){
  
  x <- as.list(x)
  names(x) <- names
  
  if(file.exists(x$dataset_file) & !isTRUE(force)){
    out(paste0(x$gSD.head, "Skipping download of '", x$dataset_name, "', since '", x$dataset_file, "' already exists..."), msg = T)
    return(TRUE)
  } else{
  
    out(paste0(x$gSD.head, "Downloading '", x$dataset_name, "' to '", x$dataset_file, "'..."), msg = T)
    file.tmp <- tempfile(tmpdir = paste0(head(strsplit(x$dataset_file, "/")[[1]], n=-1), collapse = "/")) #, fileext = ".tar.gz")
    gSD.get(x$dataset_url, dir.file = file.tmp, prog = prog)
    
    if(!is.null(x$md5_checksum)){
      if(as.character(md5sum(file.tmp)) == tolower(x$md5_checksum)){ out("Successfull download, MD5 check sums match.", msg = T)
      } else{
        out(paste0("Download failed, MD5 check sums do not match."), type = 2)
        file.remove(file.tmp)
        return(FALSE)
      }
    }
  }
  
  file.rename(file.tmp, x$dataset_file)
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

#' download summary 
#'
#' @param records df
#' @param records.names character
#'
#' @keywords internal
#' @noRd
.download_summary <- function(records, records.names){
  
  # remove internal columns
  records <- records[,-grep("gSD", colnames(records))]
  
  if(!is.null(records$download_success)){
    if(any(!records$download_success)){
      out(paste0("Some downloads have not been succesfull after ", max(records$download_attempts), " attempt(s) (see column 'download_success'). Please retry later."), type = 2)
    } else{
      out(paste0("All downloads have been succesfull after ", max(records$download_attempts), " attempt(s)."), msg = T)
    }
  }
  out(paste0("Columns added to records: '", paste0(setdiff(colnames(records), records.names), collapse = "', '"), "'"))
  
  return(records)
}

#' check if url
#'
#' @param url a url
#' @keywords internal
#' @noRd
is.url <- function(url) grepl("www.|http:|https:", url)

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
  
  out(paste0("Searching records for product name '", name, "'..."), msg = T)
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
      df[,spf.sub] <- st_as_text(.make_aoi(cbind(spf[seq(1, length(spf), by = 2)], spf[seq(2, length(spf), by = 2)]), type = "sf", quiet = T))
      
      df <- cbind.data.frame(df, ds_name, stringsAsFactors = F)
      colnames(df)[ncol(df)] <- "product"
      return(df)
    }), SIMPLIFY = F), recursive = F)
    
    ## Read out meta data
    out("Reading meta data of search results from USGS EarthExplorer...")
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


#' preview EE record
#'
#' @param record record
#' @param preview_crs preview_crs
#' @param on_map on_map
#' @param show_aoi show_aoi
#' @param verbose verbose
#'
#' @importFrom getPass getPass
#' @importFrom httr GET write_disk authenticate
#' @importFrom raster stack plotRGB crs crs<- extent extent<- NAvalue
#' @importFrom sf st_as_sfc st_crs as_Spatial st_transform st_coordinates
#' @importFrom mapview viewRGB
#' @importFrom leafem addFeatures
#'
#' @keywords internal
#' @noRd
.EE_preview <- function(record, preview_crs = NULL, on_map = TRUE, show_aoi = TRUE, verbose = TRUE){
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  
  ## Intercept false inputs and get inputs
  url.icon <- record$browseUrl
  if(is.na(url.icon)){out("Argument 'record' is invalid or no preview is available.", type=3)}
  if(length(url.icon) > 1){out("Argument 'record' must contain only a single record, represented by a single row data.frame.")}
  char_args <- list(url.icon = url.icon)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  
  if(length(grep("https", url.icon)) == 0){
    out("No preview available for this record or product.", msg = T)
  } else{
    ## Recieve preview
    file_dir <- paste0(tempfile(),".jpg")
    gSD.get(url.icon, dir.file = file_dir)
    r.prev <- stack(file_dir)
    
    if(isTRUE(on_map)){
      
      ## create footprint
      footprint <- record$footprint
      if(!is.null(preview_crs)) footprint <- st_transform(footprint, st_crs(preview_crs))
      
      crs(r.prev) <- crs(as_Spatial(footprint))
      footprint <- st_coordinates(footprint)
      
      extent(r.prev) <- extent(min(footprint[,1]), max(footprint[,1]), min(footprint[,2]), max(footprint[,2])) #extent(footprint)
      
      ## create map
      map <- suppressWarnings(viewRGB(r.prev, r=1, g=2, b=3))
      
      if(isTRUE(show_aoi)){
        if(isFALSE(getOption("gSD.aoi_set"))){
          out("Preview without AOI, since no AOI has been set yet (use 'set_aoi()' to define an AOI).", type = 2)
        } else{
          aoi.sf <- getOption("gSD.aoi")
          #aoi.sf <- .make_aoi(aoi.m, type = "sf", quiet = T)
          map <- addFeatures(map, aoi.sf)
        }
      }
      print(map) # display mapview or leaflet output
    } else{
      
      ## create simple RGB plot
      plotRGB(r.prev)
    }
  }
}


#' convert MODIS product names
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



#' make aoi
#'
#' @param aoi aoi
#' @keywords internal
#' @importFrom sp SpatialPolygons
#' @importFrom sf st_sfc st_polygon st_crs st_as_sf st_coordinates st_transform st_crs<- as_Spatial
#' @noRd
.make_aoi <- function(aoi, type = "matrix", quiet = F){
  
  ## if not sfc, convert to sfc
  if(!inherits(aoi, c("Spatial", "sfc", "matrix"))) out("Argument 'aoi' needs to be a 'SpatialPolygons' or 'sfc_POLYGON' or 'matrix' object.", type = 3)
  if(inherits(aoi, "matrix")){
    if(!all(aoi[1,] == aoi[length(aoi[,1]),])) aoi <- rbind(aoi, aoi[1,])
    aoi <- st_sfc(st_polygon(list(aoi)), crs = 4326)
    if(isFALSE(quiet)) out(paste0("Argument 'aoi' is a matrix, assuming '", st_crs(aoi)$proj4string, "' projection."), type = 2)
  }
  if(inherits(aoi, "Spatial")) aoi <- st_as_sf(aoi)
  
  ## check projection
  if(is.na(st_crs(aoi))){
    st_crs(aoi) <- 4326
    if(isFALSE(quiet)) out(paste0("Argument 'aoi' has no projection, assuming '", st_crs(aoi)$proj4string, "' projection."), type = 2)
  }
  if(length(grep("WGS84", grep("longlat", st_crs(aoi)$proj4string, value = T), value = T)) != 1){
    aoi <- st_transform(aoi, 4326)
  }
  
  ## get coordinates
  aoi.m <- st_coordinates(aoi)[,c(1,2)]
  aoi.sf <- st_sfc(st_polygon(list(aoi.m)), crs = 4326)
  aoi.sp <- as_Spatial(aoi.sf)
  
  if(type == "matrix") return(aoi.m)
  if(type == "sf") return(aoi.sf)
  if(type == "sp") return(aoi.sp)
}


#' translate records column names to gSD standard
#' @param records df as returned by client
#' @param name product name 
#' @keywords internal
#' @noRd
.translate_records <- function(records, name){
  
  # set-up column name dictionary
  dict <- rbind.data.frame(c("product", NA, NA, NA),
                           c("record_id", "title", "displayId", "displayId"),
                           c("entity_id", "uuid", "entityId", "entityId"),
                           c("dataset_url", "url", NA, NA),
                           c("md5_url", "url.alt", NA, NA),
                           c("preview_url", "url.icon", "browseUrl", "browseUrl"),
                           c("meta_url", NA, "metadataUrl", "metadataUrl"),
                           c("meta_url_fgdc", NA, "fgdcMetadataUrl", "fgdcMetadataUrl"),
                           c("summary", "summary", "summary", "summary"),
                           c("date_aquistion", NA, "acquisitionDate", "acquisitionDate"),
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
