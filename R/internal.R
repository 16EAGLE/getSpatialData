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
is.FALSE <- function(evaluate){if(evaluate == FALSE){return(TRUE)}else{return(FALSE)}}


#' Simplifies check of variables being TRUE
#'
#' @param evaluate variable or expression to be evaluated
#'
#' @keywords internal
#' @noRd
is.TRUE <- function(evaluate){if(evaluate == TRUE){return(TRUE)}else{return(FALSE)}}


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
#' @param name name
#' @param url url
#' @param file file
#' @importFrom tools md5sum
#' @keywords internal
#' @noRd
gSD.download <- function(name, url.file, file, url.checksum = NULL, head.out = NULL, prog = T){
  
  out(paste0(if(!is.null(head.out)) head.out else "", "Downloading '", name, "' to '", file, "'..."), msg = T)
  file.tmp <- tempfile(tmpdir = paste0(head(strsplit(file, "/")[[1]], n=-1), collapse = "/")) #, fileext = ".tar.gz")
  gSD.get(url.file, dir.file = file.tmp, prog = prog)
  
  if(!is.null(url.checksum)){
    md5 <- strsplit(content(gSD.get(url.checksum), as = "text", encoding = "UTF-8"), " ")[[1]][1]
    if(as.character(md5sum(file.tmp)) == tolower(md5)){ out("Successfull download, MD5 check sums match.", msg = T)
    } else{
      out(paste0("Download failed, MD5 check sums do not match. Will retry."), type = 2)
      file.remove(file.tmp)
      return(FALSE)
    }
  } #else out("Download finished. MD5 check sums not available (file integrity could not be checked).", msg = T)
  
  file.rename(file.tmp, file)
  return(TRUE)
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
      if(p == "Sentinel-1" | p == "Sentinel-2") x <- "dhus"
      if(p == "Sentinel-3") x <- "s3"
      if(p == "Sentinel-5P" | p == "Sentinel-5 Precursor") x <- "s5p"
      if(p == "GNSS") x <- "gnss"
    }
    if(x == "dhus"){url <- getOption("gSD.api")$dhus}
    if(x == "s3"){
      url <- getOption("gSD.api")$s3
      user <- "s3guest"
      pw <- "s3guest"
    }
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
  
  out("Searching USGS EarthExplorer for available products...")
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
      
      # Make sf polygon filed from spatialFootprint
      spf.sub <- grep("spatialFoot", x.names)
      spf <- unlist(x[spf.sub])
      spf <- as.numeric(spf[grep("coordinates", names(spf))])
      spf.sf <- .make_aoi(cbind(spf[seq(1, length(spf), by = 2)], spf[seq(2, length(spf), by = 2)]), type = "sf", quiet = T)
      
      df <- rbind.data.frame(x.char, stringsAsFactors = F)
      colnames(df) <- x.names
      df[,spf.sub] <- st_as_text(spf.sf)
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
#' @param return_preview return_preview
#' @param verbose verbose
#'
#' @importFrom getPass getPass
#' @importFrom httr GET write_disk authenticate
#' @importFrom raster stack plotRGB crs crs<- extent extent<- NAvalue
#' @importFrom sf st_as_sfc st_crs as_Spatial st_transform st_coordinates
#' @importFrom mapview viewRGB addFeatures
#'
#' @keywords internal
#' @noRd
.EE_preview <- function(record, preview_crs = NULL, on_map = TRUE, show_aoi = TRUE, return_preview = FALSE, verbose = TRUE){
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
    
    if(isTRUE(on_map) | isTRUE(return_preview)){
      
      ## create footprint
      footprint <- st_as_sfc(list(record$spatialFootprint), crs = 4326)
      if(!is.null(preview_crs)) footprint <- st_transform(footprint, st_crs(preview_crs))
      
      crs(r.prev) <- crs(as_Spatial(footprint))
      footprint <- st_coordinates(footprint)
      
      extent(r.prev) <- extent(min(footprint[,1]), max(footprint[,1]), min(footprint[,2]), max(footprint[,2])) #extent(footprint)
      
      if (isTRUE(on_map)) {
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
        map # display mapview or leaflet output
      }
      
      if (isTRUE(return_preview)) {
        return(r.prev)
      }
      
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
## check order(s)
.ESPA_download <- function(order.list, username, password, file.down, delay = 10, dir_out, items.order = NULL, n.item = NULL){
  
  remain.active = TRUE; ini = TRUE; show.status = TRUE
  while(remain.active){
    
    ## get tiems
    items <- lapply(order.list, function(x, user = username, pass = password){
      content(gSD.get(paste0(getOption("gSD.api")$espa, "item-status/", x), user, pass))
    })
    
    ## get items content
    items <- lapply(items, function(x) lapply(x[[1]], function(y){
      r <- unlist(y)
      names(r) <- names(y)
      return(r)
    }))
    
    ## make items data.frame containing recieve status
    items <- data.frame(do.call(rbind, lapply(items, function(x) do.call(rbind, lapply(x, function(y) rbind(y))))), row.names = NULL, check.names = F, fix.empty.names = F, stringsAsFactors = F)
    names.required <- sapply(file.down, function(x) head(strsplit(tail(strsplit(x, "/")[[1]], n=1), "_LEVEL_")[[1]], n=1), USE.NAMES = F) #paste0(head(strsplit(tail(strsplit(x, "/")[[1]], n=1), "_")[[1]], n=-1), collapse = "_"), USE.NAMES = F)
    items <- items[sapply(names.required, function(x, y = items$name) which(y == x), USE.NAMES = F),]
    items <- cbind(items, items$status == "complete")
    items <- cbind.data.frame(items, file.down, stringsAsFactors = F) #sapply(as.character(items$name), function(x, l = level) paste0(dir_out, "/", x, "_", toupper(level), ".tar.gz"), USE.NAMES = F), stringsAsFactors = F)
    colnames(items)[(ncol(items)-1):ncol(items)] <- c("available", "file")
    
    if(ini){
      items.df <- cbind.data.frame(items, rep(FALSE, length(items$status)), stringsAsFactors = F)
      colnames(items.df)[ncol(items.df)] <- "recieved"
      ini <- FALSE
    } else{
      items.df <- cbind.data.frame(items, items.df$recieved, stringsAsFactors = F)
    }
    if(isTRUE(force)) emp <- sapply(items.df$file, function(x) if(file.exists(x)) file.remove(x), USE.NAMES = F)
    items.df$recieved <- sapply(items.df$file, file.exists, USE.NAMES = F)
    
    ## Items to download
    if(all(items.df$available) & all(items.df$recieved)){
      remain.active <- FALSE
    } else{
      
      ## Download or wait for status
      sub.download <- intersect(which(items.df$available == T), which(items.df$recieved == F))
      if(length(sub.download) > 0){
        
        items.get <- items.df[sub.download,]
        out(paste0("Starting download of product(s) '", paste0(items.get$name, collapse = "', "), "'."), msg = T)
        #items.df$recieved[sub.download] <- apply(items.get, MARGIN = 1, function(x, d = dir_out){
        items.df$recieved[sub.download] <- mapply(name = items.get$name, uf = items.get$product_dload_url, uc = items.get$cksum_download_url,
                                                  file = items.get$file, i.item = items.order, function(name, uf, uc, file, i.item){
                                                    
                                                    # create console index of current item                                                    
                                                    head.out <- paste0("[", i.item, "/", n.item, "] ")
                                                    gSD.download(name = name, url.file = uf, url.checksum = uc, file = file, head.out = head.out)
                                                  })
        show.status <- TRUE
      } else{
        if(isTRUE(show.status)){
          out(paste0("Waiting for product(s) '", paste0(items.df$name[items.df$available == F], collapse = "', "), "' to be ready for download from ESPA (this may take a while)..."))
          out("Note: It is also possible to terminate the function and call it again later by providing the displayed order ID(s) as input to 'espa_order'.", msg = T)
        }
        show.status <- FALSE
      }
    }
    Sys.sleep(delay) #wait before reconnecting to ESPA to recheck status
  }
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

#' On package startup
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname){
  
  op <- options()
  op.gSD <- list(
    gSD.api = list(dhus = 'https://scihub.copernicus.eu/dhus/',
                   s3 = 'https://scihub.copernicus.eu/s3/',
                   s5p = 'https://s5phub.copernicus.eu/',
                   gnss = 'https://scihub.copernicus.eu/gnss/',
                   espa = 'https://espa.cr.usgs.gov/api/v1/',
                   ee = 'https://earthexplorer.usgs.gov/inventory/json/v/1.4.0/',
                   aws.l8 = 'https://landsat-pds.s3.amazonaws.com/c1/L8/',
                   aws.l8.sl = 'https://landsat-pds.s3.amazonaws.com/c1/L8/scene_list.gz',
                   laads = 'https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/'),
    gSD.api.names = list(dhus = "ESA Copernicus Open Hub",
                         s3 = "ESA Copernicus S3 Hub",
                         s5p = "ESA Copernicus S5P Hub",
                         gnss = "ESA Copernicus GNSS Hub",
                         espa = "USGS-EROS ESPA",
                         ee = "USGS EarthExplorer",
                         aws.l8 = "AWS Landsat 8",
                         laads = "NASA DAAC LAADS"),
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



#' bridge between sensor and calc HOT
#' 
#' @inheritParams calcSentinel_aoi_cloudcov
#' @param sensor sensor
#' @param sceneCloudCoverCol sceneCloudCoverCol
#'
#' @keywords internal
#' 
#' @importFrom utils object.size
#' @noRd

.hotBridge <- function(sensor = NULL, sceneCloudCoverCol = NULL, records, aoi = NULL,  maxDeviation = 20,
                       cloudPrbThreshold = 40, slopeDefault = 1.4, interceptDefault = -10, 
                       dir_out = NULL, username = NULL, password = NULL, verbose = TRUE) {
  
  ## Check input
  aoiClass <- class(aoi)
  if (aoiClass[1] != "sf" && aoiClass != "sp" && aoiClass != "matrix") {out(paste0("Aoi has to be of class 'sp' or 'sf' or 'matrix' but is of class:\n",aoiClass),type=3)}

  numRecords <- NROW(records)
  quarterNumRecords <- round(numRecords/4)
  processingTime <- c()
  previewSize <- c()
  ## Do HOT cloud cover assessment consecutively
  records <- do.call(rbind,lapply(1:numRecords,function(i) {
    startTime <- Sys.time()
    # get preview of current record
    currRecord <- records[i,]
    
    if (sensor == "Sentinel-2") {
      preview <- getSentinel_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                                     username=username,password=password,verbose=verbose)
    } else if (sensor == "Landsat") {
      preview <- getLandsat_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                                    username=username,password=password,verbose=verbose)
    } else if (sensor == "MODIS") {
      preview <- getMODIS_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                                  username=username,password=password,verbose=verbose)
    }
    previewSize <- c(previewSize,object.size(preview))
    # pass preview to HOT function
    currRecCloudCover <- calc_hot_cloudcov(records=records,preview=preview,aoi=aoi,maxDeviation=maxDeviation,sceneCloudCoverCol=sceneCloudCoverCol,
                                           slopeDefault=slopeDefault,interceptDefault=interceptDefault,dir_out=dir_out,verbose=verbose)
    endTime <- Sys.time()
    if (i <= 5) {elapsed <- round(as.numeric(difftime(endTime,startTime,units="mins")))}
    processingTime <- c(processingTime,elapsed)
    if (numRecords >= 10 && i == 5) {
      .calcProcTime(numRecords=numRecords,quarterNumRecords=quarterNumRecords,i=i,processingTime=processingTime,previewSize=previewSize)
    }
    return(currRecCloudCover)
  }))
  return(records)
}



#' calc processing time
#' 
#' @param numRecords numRecords
#' @param quarterNumRecords quarterNumRecords
#' @param i record number in the loop
#' @param processingTime processingTime
#' @param previewSize previewSize
#' @keywords internal
#' @importFrom utils object.size
#' @noRd
.calcProcTime <- function(numRecords,quarterNumRecords,i,processingTime,previewSize) {
  processedUpdate <- "records are processed "
  meanProcessingTime <- mean(processingTime)
  meanPreviewSize <- mean(previewSize)
  stillToGoFor <- numRecords - 5
  sumProcessingTime <- meanProcessingTime * (stillToGoFor)
  sumDataDownload <- meanPreviewSize * (stillToGoFor)
  out(paste0("5 records are processed.\nProcessing time for all remaining records in sum:",sumProcessingTime,"\nData amount to be downloaded:",sumDataDownload))
  if (i == quarterNumRecords) {out(paste0(i,processedUpdate,"(approx. 25 % of all records)"))}
  if (i == quarterNumRecords * 2) {out(paste0(i,processedUpdate,"(approx. 50 % of all records"))}
  if (i == quarterNumRecords * 3) {out(paste0(i,processedUpdate,"(approx. 75 % of all records"))}
}

