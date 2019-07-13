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
      if(p == "Sentinel-1" | p == "Sentinel-2" | p == "Sentinel-3") x <- "dhus"
      #if(p == "Sentinel-3") x <- "s3" # decommisioned
      if(p == "Sentinel-5P" | p == "Sentinel-5 Precursor") x <- "s5p"
      if(p == "GNSS") x <- "gnss"
    }
    if(x == "dhus"){url <- getOption("gSD.api")$dhus}
    # if(x == "s3"){
    #   url <- getOption("gSD.api")$s3
    #   user <- "s3guest"
    #   pw <- "s3guest"
    # }
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
      footprint <- st_as_sfc(record$spatialFootprint, crs = 4326)
      if(!is.null(preview_crs)) footprint <- st_transform(footprint, st_crs(preview_crs))
      
      crs(r.prev) <- crs(as_Spatial(footprint))
      footprint <- st_coordinates(footprint)
      
      extent(r.prev) <- extent(min(footprint[,1]), max(footprint[,1]), min(footprint[,2]), max(footprint[,2])) #extent(footprint)
      #crs(r.prev) <- preview_crs

      if(isTRUE(on_map)) {
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

#' bridge between sensor and a cloud cover calculator
#' 
#' @inheritParams calc_cloudcov
#' @param sensor sensor
#' @param sceneCloudCoverCol sceneCloudCoverCol
#'
#' @keywords internal
#' 
#' @importFrom utils object.size
#' @noRd
.cloudcov_bridge <- function(sensor = NULL, sceneCloudCoverCol = NULL, records, aoi = NULL,  maxDeviation = 20,
                       cloudPrbThreshold = 40, slopeDefault = 1.4, interceptDefault = -10, 
                       dir_out = NULL, username = NULL, password = NULL, verbose = TRUE) {
  
  ## Check input
  aoiClass <- class(aoi)
  classNumErr <-  "has to be of class 'numeric'. But is: "
  if (aoiClass[1] != "sf" && aoiClass != "SpatialPolygonsDataFrame" && aoiClass != "SpatialPolygons" && aoiClass != "matrix") {out(paste0("Aoi has to be of class 'sp' or 'sf' or 'matrix' but is of class:\n",aoiClass),type=3)}
  if (!class(records) == "data.frame") {out(paste0("Records has to be of class 'data.frame' in the format as returned within the getSpatialData package. But is of class: ",class(records)),type=3)}
  params <- list("cloudPrbThreshold"=cloudPrbThreshold,"slopeDefault"=slopeDefault,"interceptDefault"=interceptDefault)
  check_num <- sapply(1:length(params),function(i) {
    if (!is.numeric(params[[i]])) {out(paste0(names(params)[[i]],classNumErr,class(params[[i]])),type=3)}
  })
  
  numRecords <- nrow(records)
  out(paste0("\n",numRecords," records will be processed...\nStarting HOT..."))
  processingTime <- c()
  previewSize <- c()
  ## Do HOT cloud cover assessment consecutively
  records <- do.call(rbind,lapply(1:numRecords,function(i) {
    startTime <- Sys.time()
    # get preview of current record
    currRecord <- records[i,]
    
    if (sensor == "Sentinel-2" || sensor == "Sentinel-3") {
      preview <- getSentinel_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                                     username=username,password=password,verbose=verbose)
      identifier <- 1
    } else if (sensor == "Landsat") {
      preview <- getLandsat_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                                    verbose=verbose)
      identifier <- 15
    } else if (sensor == "MODIS") {
      preview <- getMODIS_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                                  verbose=verbose)
      identifier <- 15
    }
    previewSize <- c(previewSize,object.size(preview))
    # pass preview to HOT function
    currRecCloudCover <- calc_hot_cloudcov(record=currRecord,preview=preview,aoi=aoi,identifier=identifier,maxDeviation=maxDeviation,sceneCloudCoverCol=sceneCloudCoverCol,
                                           slopeDefault=slopeDefault,interceptDefault=interceptDefault,dir_out=dir_out,verbose=verbose)
    endTime <- Sys.time()
    if (i <= 5) {
      elapsed <- as.numeric(difftime(endTime,startTime,units="mins"))
      processingTime <- c(processingTime,elapsed)
    }
    if (numRecords >= 10 && i == 5) {
      .calcHOTProcTime(numRecords=numRecords,i=i,processingTime=processingTime,previewSize=previewSize)
    }
    return(currRecCloudCover)
  }))
  return(records)
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
  } else if (sumProcessingTime == 1) {
    sumProcessingTime <- "1 minute"
  } else {
    sumProcessingTime <- paste0(round(as.numeric(sumProcessingTime))," minutes")
  }
  sumDataDownload <- meanPreviewSize * stillToGoFor
  out(paste0("\n5 records are processed.\nProcessing time for all remaining records, in sum approx.: ",sumProcessingTime,"\nData amount to be downloaded approx.: ",sumDataDownload," MB\n"))
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
#' @param aoi aoi. Only if mode == "aoi".
#' @return \code{percent} numeric percentage
#' @keywords internal
#' @importFrom raster as.matrix extract xmin xmax ymin ymax resolution crs rasterize
#' @noRd
.raster_percent <- function(x, mode = "na", custom = NULL, aoi = NULL) {
  
  if (mode != "aoi") {x_mat <- as.matrix(x)}
  if (mode == "na") {
    percent <- (length(which(x_mat == 0)) / length(which(!is.na(x_mat)))) * 100
  } else if (mode == "custom") {
    val1 <- length(which(x_mat == custom[[1]]))
    val2 <- length(which(x_mat == custom[[2]]))
    percent <- (val1 / sum(val1,val2)) * 100
  } else if (mode == "aoi") {
    r <- raster(xmn=xmin(aoi),xmx=xmax(aoi),ymn=ymin(aoi),ymx=ymax(aoi),crs=crs(mos),resolution=res(mos))
    aoi_r <- rasterize(aoi,r) # rasterize aoi in order to calculate number of pixels with valid pixels in whole aoi
    aoi_r_extr <- extract(aoi_r,aoi)[[1]]
    valid <- extract(x,aoi)[[1]]
    percent <- (length(valid[!is.na(valid)]) / length(aoi_r_extr[!is.na(aoi_r_extr)])) * 100
  }

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
#' @param s list 'selected' of a timestamp holding everything inserted in select_*().
#' @param aoi aoi.
#' @param i numeric index in the loop.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @param dir_out character directory below which to save intermediate product in tmp.
#' @param cloud_mask_col character name of cloud mask path column.
#' @param preview_col character name of the preview path column.
#' @return \code{save_pmos_final} character path where preview RGB mosaic is saved 
#' @keywords internal
#' @noRd
.select_preview_mos <- function(s, aoi, i, identifier, dir_out, cloud_mask_col, preview_col) {
  
  id_sel <- sapply(s$ids,function(x) {which(records[,identifier]==x)})
  save_pmos <- file.path(dir_out,paste0("preview_mosaic_timestamp",s$timestamp))
  # mask preview tiles by cloud masks
  layers <- c("red","green","blue")
  tmp_dir <- .tmp_dir(dir_out,action=1)
  preview_paths <- lapply(id_sel,function(i) {
    p_path <- records[i,preview_col]
    preview <- brick(p_path)
    preview_aoi <- mask(preview,aoi)
    cMask <- raster(records[i,cloud_mask_col])
    preview_masked <- .mask_raster(preview_aoi,cMask)
    preview_save <- file.path(tmp_dir,paste0(records[i,identifier],"_cloud_masked"))
    paths_sep <- sapply(1:nlayers(preview_masked),function(j) {
      layer_save <- paste0(preview_save,"_",i,"_",layer[j],".tif")
      writeRaster(preview_masked[[j]],layer_save,overwrite=T)
      return(layer_save)
    })
  })
  preview_mos <- lapply(1:length(layers),function(j) {
    curr_layer <- lapply(preview_paths,function(x) {
      path <- x[j]
    })
    save_path_pmos <- paste0(save_pmos,"_",layers[j],".tif")
    mos <- .select_bridge_mosaic(curr_layer,aoi,save_path_pmos,mode="rgb")
  })
  
  save_pmos_final <- paste0(save_pmos,"_",i,"_rgb.tif")
  preview_mos_stack <- stack(preview_mos)
  writeRaster(preview_mos_stack,save_pmos_final,overwrite=T)
  .tmp_dir(dir_out,action=2)
  return(save_pmos_final)
  
}

#' create mosaic consecutively in the order of ordered records (according to aoi cloud cover)
#' Important: the cloud masks have to have NA where clouds or no data.
#' @param records data.frame that contains all records within the sub-period but will be subsetted to \code{sub}.
#' @param aoi aoi.
#' @param sub list of numeric vectors. Each vector represents one tile id and indexes records in \code{records}.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @param cloud_mask_col character name of cloud mask path column.
#' @param dir_out character directory where to save intermediate product.
#' @return selected list of [[1]] character ids of selected records, [[2]] percentage of valid pixels in mosaic.
#' Side effect: creates a dir_tmp, writes into it, deletes dir_tmp with all files.
#' @importFrom plyr compact
#' @importFrom raster minValue maxValue
#' @keywords internal
#' @noRd
.select_calc_mosaic <- function(records, aoi, sub, cloud_mask_col, dir_out, identifier) {
  
  dir_tmp <- .tmp_dir(dir_out,1)
  sub <- unlist(compact(sub))
  collection <- sapply(sub,function(x) { # get paths to cloud masks
    cmask_path <- records[[cloud_mask_col]][x]
  })
  # this vector are all orders in an order from best records (lowest aoi cc) to worst records (highest aoi cc) in a queue
  names(collection) <- sapply(sub,function(x) {return(records[x,identifier])})
  # start mosaicking
  base_coverage <- -1000
  base_order <- c()
  # add next cloud mask consecutively and check if it decreases the cloud coverage
  for (i in 1:length(collection)) {
    x <- collection[i] # do it this way in order to keep id
    save_path <- file.path(dir_tmp,paste0("mosaic_tmp_",names(x),".tif"))
    added_order <- c(base_order,x)
    mos <- .select_bridge_mosaic(added_order,aoi,save_path)
    next_coverage <- .raster_percent(mos,mode="aoi",aoi=aoi)
    # the coverage value has to become larger since base_coverage refers to percentage covered with valid pixels
    add_it <- next_coverage > base_coverage
    if (add_it) {
      base_order <- added_order
      base_coverage <- next_coverage
    }  
  }
  
  # return ids of selected records and percentage of valid pixels of final mosaic
  selected <- list(ids=names(base_order),
                   cMask_paths=base_order,
                   valid_pixels=base_coverage)
  
  del <- .tmp_dir(dir_out)
  return(selected)
}

#' sets NAs in cloud masks 0 and crops to the aoi then
#' @param cMask.
#' @param aoi.
#' @return cMask with 0 where no coverage
#' @importFrom raster mask
#' @keywords internal
#' @noRd
.cMask_NA_to_0 <- function(cMask, aoi) {
  
  cMask[is.na(cMask)] <- 0
  cMask <- mask(cMask,aoi)
  
}

#' bridge to .make_mosaic
#' @param paths character paths to rasters to be mosaicked.
#' @param aoi aoi.
#' @param save_path save_path (should end with '.tif').
#' @importFrom raster mask crop
#' @keywords internal
#' @noRd
.select_bridge_mosaic <- function(paths, aoi, save_path, mode = "mask") {
  
  mos_base <- .make_mosaic(paths,save_path,mode)
  mos_base_mask <- mask(mos_base,aoi)
  mos_base_crop <- crop(mos_base_mask,aoi)
  
}

#' masks a raster with masks of 1 and NA (remain with pixels where mask == 1)
#' @param x raster to be masked.
#' @param mask raster mask with 1 for valid pixels and NA for pixels to be set NA.
#' @return \code{x} masked raster
#' @keywords internal
#' @importFrom raster
#' @noRd
.mask_raster <- function(x, mask) {
  x[is.na(mask)] <- NA
  return(x)
}

#' handles the aoi input and converts it to sf object if needed
#' @param aoi aoi.
#' @param crs crs.
#' @return \code{aoi}
#' @keywords internal
#' @importFrom sf st_as_sf st_transform
#' @noRd
.handle_aoi <- function(aoi, crs) {
  
  error <- "try-error"
  if (class(aoi)[1] != "sf") aoi <- try(st_as_sf(aoi))
  if (inherits(aoi,error)) {out(paste0("Aoi of class '",class(aoi),"' could not be converted to 'sf' object"),3)}
  if (as.character(crs(aoi)) != as.character(crs)) {
    aoi <- try(st_transform(aoi,crs))
  }
  if (inherits(aoi,error)) {out("Aoi reprojection failed",3)}
  return(aoi)
  
}

#' creates an error if requested coverage is higher than sensor revisit time
#' @param sensor character name of sensor.
#' @param period character vector of start and end date.
#' @param num_timestamps numeric number of timestamps. 
#' @return nothing. Console communication
#' @keywords internal
#' @noRd
.select_handle_revisit <- function(sensor, period, num_timestamps) {
  
  revisit_times <- list("Landsat"=8,"Sentinel-2"=5,"Sentinel-3"=2,"MODIS"=2)
  r <- min(sapply(sensor,function(x) {revisit_times[[x]]}))
  sub_period <- (as.numeric(as.Date(period[2]) - as.Date(period[1]))) / num_timestamps
  info <- paste0("The selected number of timestamps (",num_timestamps)
  s <- ifelse(length(sensor)==1,paste0("\nSensor:"),paste0("\nSensors:"))
  out(cat("Number of timestamps selected:",num_timestamps,s,sensor))
  if (sub_period < r) {
    out(paste0(info,") results in shorter coverage frequency than sensor revisit time (",r,"). Decrease 'num_timestamps'"),3)
  } else if (sub_period == r) {
    out(paste0(info,") results in equal coverage frequency as revisit time (",r,"). It is unlikely to get cloud-free coverage this often"),1)
  }
  
}

#' handle Landsat case in select
#' @param records data.frame.
#' @return \code{records} data.frame
#' @keywords internal
#' @noRd
.select_handle_landsat <- function(records, sensor) {
  
  if (sensor %in% c("Landsat","MODIS")) {
    records <- .make_Landsat_tileid(records)
    identifier <- 15
    return(list(records,identifier))
  }
  
}

#' selects from a numeric data.frame column the i lowest value and checks if it is lower than a provided numeric
#' @param records data.frame.
#' @param max numeric maximum allowed value.
#' @param i numeric.
#' @param column character name of the column to be ordered.
#' @return \code{chosen} numeric index to the matching data.frame row.
#' @keywords internal
#' @noRd
.df_get_lowest <- function(records, max, i, column) {
  
  records_ord <- sort(records[[column]])
  if (i > NROW(records_ord)) return(NA)
  chosen <- which(records[[column]]==records_ord[[i]])[1]
  val <- records[chosen,column]
  if (val <= max) return(chosen) else return(NA)
  
}

#' selects initial records for the first sub-period
#' @param records data.frame subsetted to a sub-period.
#' @param tiles character vector of the tile ids.
#' @param period character vector of start and end date.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param aoi_cc_col character name of aoi cloud cover column.
#' @param tileid_col character name of tile id column.
#' @param date_col character name of the date column.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @return \code{sub} list of numeric vectors, each number is an index to a record in \code{records}
#' @keywords internal
#' @noRd
.select_sub <- function(records, tiles, max_sub_period, 
                        aoi_cc_col, tileid_col, date_col, max_cloudcov_tile, 
                        identifier) {
  
  sub <- lapply(tiles,function(x) {
    rec_tile_sub <- records[which(records[[tileid_col]]==x),]
    i <- 0
    lwst_cc <- 0
    selected <- c()
    while(!is.na(lwst_cc)) {
      i <- i+1
      lwst_cc <- .df_get_lowest(rec_tile_sub,max=max_cloudcov_tile,i,aoi_cc_col)
      if (!is.na(lwst_cc)) {
        selected[i] <- which(records[,identifier] == rec_tile_sub[lwst_cc,identifier])
      }
    }
    return(selected)
  })
  return(sub)
}

#' calls the different steps of selection for a sub-period
#' @param records data.frame subsetted to a sub-period.
#' @param period character vector of start and end date.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param par list holding everything inserted into this parameter list in the calling select function (8 parameters).
#' @param dir_out character directory where to save intermediate product.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @return \code{selected} list of [[ids]] character vector of selected ids, [[cMask_paths]] character vector to cloud masks, 
#' [[valid_pixels]] percentage of valid pixels in mosaic with the given selection. 
#' @keywords internal
#' @noRd
.select_process_sub <- function(records, period, max_sub_period, max_cloudcov_tile, par, dir_out, identifier) {
  
  tiles <- unique(records[[par$tileid_col]])
  # the sub is an ordering process of all available records per tile according to aoi cloud cover
  sub <- .select_sub(records=records,tiles=tiles,
                     aoi_cc_col=par$aoi_HOT_col,tileid_col=par$tileid_col,
                     date_col=par$date_col, max_cloudcov_tile=max_cloudcov_tile,
                     identifier=identifier)
  sub_within <- .select_force_period(records,sub,period,max_sub_period,par$date_col)
  # make best mosaic of cloud masks for first timestamp
  out("Calculating best mosaic for timestamp..",msg=T)
  selected <- .select_calc_mosaic(records,aoi,sub_within,par$cloud_mask_col,dir_out,identifier)
  
}

#' returns the indices of records within a max_sub_period
#' @param records data.frame.
#' @param sub list of numeric vectors each pointing to a record.
#' @param period character vector of start and end date.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param date_col character name of the date column.
#' @return \code{sub_within} list of numeric vectors, each number is an index to a record in \code{records}
#' @importFrom plyr compact
#' @keywords internal
#' @noRd
.select_force_period <- function(records, sub, period, max_sub_period, date_col) {
  
  # check if covered period of timestamp is within max_sub_period and re-calculate period consecutively with record of next-lowest cloud cover
  max_num_sel <- max(sapply(sub,length))
  orders <- sapply(1:max_num_sel,function(i) {unlist(sapply(sub,function(x) {return(x[i])}))}) # to matrix
  orders <- data.frame(orders) 
  period_new <- c()
  sub_within <- list()
  for (i in 1:NCOL(orders)) {
    x <- orders[,i]
    # first try to use all records of this order
    order <- x[!is.na(x)]
    period_tmp <- .select_period_bridge(records,order,period_new,date_col)
    period_tmp_le <- .calc_days_period(period_tmp)
    if (period_tmp_le <= max_sub_period) { # the case where all records from current order x are within period_new
      period_new <- period_tmp
      sub_within[[i]] <- order
    } else {
      # for the case where at least one of record of order x is not within period_new
      # try with all values in all possible combinations (not orders). Might be that 
      # from 0 to all records except one are within period
      order <- .select_remove_dates(x, records, period_new, max_sub_period, date_col)
      # remain with the try that is within the temporal max_sub_period with previous orders and
      # offers the highest number of records and lowest aoi cloud cover
      if (length(order) > 0 && isFALSE(all(is.na(test)))) {
        subset_true <- compact(order)
        check_cc <- sapply(subset_true,function(x) {
          cc <- mean(records[x,aoi_cc_col]) # mean aoi cloud cover
        })
        subset_le <- sapply(subset_true,length)
        max_le <- which(subset_le == max(subset_le))
        subset_max_rec <- subset_true[max_le]
        subset_cc <- check_cc[max_le]
        # maybe several subsets have the highest number of used records within the period
        # in this case take the choice with lowest aoi cc cover
        min_cc_max_le <- which(subset_cc == min(subset_cc))[1]
        sub_within[[i]] <- subset_max_rec[[min_cc_max_le]]
      }
    }
  }
  return(sub_within)
  
}

#' find optimal collection of dates to remove from a records collection in a sub-period.
#' @param x numeric vector.
#' @param records data.frame.
#' @param period_new period_new.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param date_col date_col.
#' @return \code{order} list of numeric vectors where all values are an index to a record in records
#' @keywords internal
#' @noRd
.select_remove_dates <- function(x, records, period_new, max_sub_period, date_col) {
  
  vec <- .help_vec_comb(x)
  order <- list()
  n <- 0
  for (order_curr in vec) {
    period_tmp <- .select_period_bridge(records,order_curr,period_new,date_col)
    period_tmp_le <- .calc_days_period(period_tmp)
    if (period_tmp_le <= max_sub_period) {
      n <- n+1
      period_new <- period_tmp
      order[[n]] <- order_curr
    }
  }
  order <- unique(order)
}

#' helper for creating a list of numeric vectors with all possible combinations of numerics (not orders, just combinations)
#' @param x numeric vector.
#' @return \code{vecs} list of numeric vectors
#' @importFrom combinat permn
#' @keywords internal
#' @noRd
.help_vec_comb <- function(x) {
  
  vseq <- 1:length(x)
  vals <- list()
  n <- 0
  all_comb <- combinat::permn(x)
  for (c in all_comb) {
    for (i in vseq) {
      n <- n+1
      vals[[n]] <- c[1:i]
    }
  }
  vals_sort <- lapply(vals,sort)
  vec <- unique(vals_sort)

}

#' bridge function to the period identifier \link{.identify_period}. Enables to calculate from
#' a given period with added dates a new period.
#' @param records data.frame.
#' @param order numeric vector pointing to elements of records.
#' @return period_new character holding a period of dates.
#' @param date_col character name of the date column.
#' @keywords internal
#' @noRd
.select_period_bridge <- function(records, order, period_new, date_col) {
  
  dates_tmp <- records[order,date_col]
  period_curr <- .identify_period(dates_tmp)
  period_new <- .identify_period(c(period_new,period_curr))
  
}

#' calculates the number of days between two dates
#' @param period character vector of start and end date.
#' @return \code{days} numeric number of days between.
#' @keywords internal
#' @noRd
.calc_days_period <- function(period) {
  
  days <- as.numeric(as.Date(period[2]) - as.Date(period[1]))
  
}

#' shortens the subset of a vector !is.na(). For example df[vec[!is.na(vec)],] is not handy
#' @param vec vector.
#' @return \code{vec_checked} vector check: vec[!is.na(vec)]
#' @keywords internal
#' @noRd

vNA <- function(vec) {
  return(vec[!is.na(vec)])
}

#' creates a tile id for Landsat data from WRSRow and WRSPath
#' @param records data.frame of Landsat data.
#' @return \code{records} data.frame with an added column: 'row_path_id'
#' @keywords internal
#' @noRd
.make_Landsat_tileid <- function(records) {
  
  records[["tile_id"]] <- paste0(records$WRSPath,records$WRSRow)
  return(records)
  
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
  dates <- sapply(0:num_timestamps,function(i) {next_date <- period[1] + (i * le_subperiods)})
  dates[length(dates)] <- dates[length(dates)] + 1
  date_col_mirr <- sapply(records[,date_col][1],as.Date) # mirror of the date column as days since 1970-01-01
  for (i in 1:num_timestamps) {
    within <- intersect(which(date_col_mirr >= dates[i]),which(date_col_mirr < dates[i+1]))
    records[within,"Sub_period"] <- i
  }
  return(records)
}

#' identifies which are date columns in a records data.frame
#' @param records.
#' @return \code{is_date} logical vector indicating which of the columns have a date
#' @keywords internal
#' @noRd
.identify_date_col <- function(records) {
  
  is_date <- sapply(1:NCOL(records),function(i) {
    check <- try(as.Date(records[1,i]),silent=T)
    if (inherits(check,"try-error")) FALSE else TRUE
  })
  
}

#' returns the smallest and largest date of a date column.
#' @param dates character vector of dates ("2019-01-01").
#' @return \code{period} character vector of two dates
#' @keywords internal
#' @noRd
.identify_period <- function(dates) {
  
  dates_sorted <- sort(dates)
  period <- c(dates_sorted[1],tail(dates_sorted,1))
  
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
               these values disable the creation of a consistent time-series. Modify the values (most likely decrease (some of) them."),3)  
  }
  
}

#' checks which records are within a period of time
#' @param records data.frame.
#' @param period character vector of dates. Last is the end date.
#' @param date_col character name of the date column.
#' @return \code{records} data.frame reduced to matching records.
#' @keywords internal
#' @noRd
.within_period <- function(records, period, date_col) {
  
  dates <- as.Date(records[[date_col]])
  cond <- intersect(which(dates >= period[1]),which(dates <= period[2]))
  records <- records[cond,]
  
}

#' constructs a console message to be given at the end of a selection process
#' @param selected_i list 'selected' holding for one timestamp: 'ids', 'cMask_paths', 'valid_pixels', 'timestamp'.
#' @return \code{console_info} character vector holding the message
#' @keywords internal
#' @noRd
.select_final_info <- function(selected_i) {

  sep <- "\n----------------------------------------------------------------"
  ts <- selected_i$timestamp
  n_records <- length(selected_i$cMask_paths)
  header <- paste0("\n- Timestamp: ",ts)
  coverage_info <- paste0("\n- Coverage of valid pixels in mosaic of selected records: ",round(selected_i$valid_pixels)," %")
  num_selected_info <- paste0("\n- Number of selected records: ",n_records)
  console_info <- c(sep,header,coverage_info,num_selected_info,sep)
  
}

#' constructs a summary of the console info of \code{.select_final_info}.
#' In addition: if the minimum coverage amongst the timestamps is below 60 \% return a warning message. 
#' In addition: if the mean coverage amongst the timestamps is below 80 \% return a warning message.
#' These warning messages are returned as NULL if the respective condition is not TRUE.
#' @param selected list of lists 'selected' each holding for a timestamp: 'ids', 'cMask_paths', 'valid_pixels', 'timestamp'
#' @return \code{console_summary_warning} list of character vectors holding the messages:
#' [[1]] Summary info
#' [[2]] Warning if minimum coverage is below 60 \% ele NULL.
#' [[3]] Warning if mean coverage is below 80 \% else NULL.
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
  header <- paste0("\n-- Selection Process Summary --")
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
                                      "\n",warn_str,"the lowest coverage amongst all timestamps is below: ",min_thresh,p,"\n"),NULL)
  warn_mean <- ifelse(check_mean,paste0("The mean c",cov_pixels,in_ts,"is ",mean_cov,warn_help,
                                        "\n",warn_str,"the mean coverage is below: ",mean_thresh,p,"\n"),NULL)
  console_summary_warning <- list(console_summary,warn_min,warn_mean)
    
}

#' prints a character vector in console combined into one message in out()
#' @param x character vector.
#' @param type numeric as in out().
#' @param msg logical as in out().
#' @return nothing. Console print
#' @keywords internal
#' @noRd
.out_vector <- function(x,type=1,msg=FALSE) {
  
  shout_out <- sapply(x,function(vec) {
    print_out <- sapply(vec,function(v) {out(v,type=type,msg=msg)})
  })
  
}








  