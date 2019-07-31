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
#' @importFrom readr read_csv write_csv
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
    is_SAR <- sensor == "Sentinel-1"
    # get preview of current record
    currRecord <- records[i,]
    identifier <- ifelse(grepl("Sentinel",sensor),1,15)
    csv_path <- file.path(dir_out,paste0(currRecord[,identifier],".csv"))
    if (file.exists(csv_path)) {
      out(paste0("Loading because already processed: ",recorrds[i,identifier]),msg=T)
      currRecCloudCover <- as.data.frame(readr::read_csv(csv_path))
      currRecCloudCover <- currRecCloudCover[,2:NCOL(currRecCloudCover)]
      return(currRecCloudCover)
    }
    if (sensor == "Sentinel-2" || sensor == "Sentinel-3") {
      preview <- try(getSentinel_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                                     username=username,password=password,verbose=verbose))
      identifier <- 1
    } else if (sensor == "Landsat") {
      preview <- try(getLandsat_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                                    verbose=verbose))
      identifier <- 15
    } else if (sensor == "MODIS") {
      preview <- try(getMODIS_preview(record=currRecord,on_map=FALSE,show_aoi=FALSE,return_preview=TRUE,
                                  verbose=verbose))
      identifier <- 15
    } else if (is_SAR) {
      preview <- NULL
    }
    # pass preview to HOT function
    cond <- !is.null(preview) && !inherits(preview,"try-error")
    if (cond) {
      currRecCloudCover <- try(calc_hot_cloudcov(record=currRecord,preview=preview,aoi=aoi,identifier=identifier,cloudPrbThreshold=cloudPrbThreshold,maxDeviation=maxDeviation,sceneCloudCoverCol=sceneCloudCoverCol,
                                                 slopeDefault=slopeDefault,interceptDefault=interceptDefault,dir_out=dir_out,verbose=verbose))
    }
    if (isFALSE(cond) || class(currRecCloudCover) != "data.frame") {
      currRecCloudCover <- .handle_cc_skip(currRecord,is_SAR,dir_out)
    } else {
      previewSize <- c(previewSize,object.size(preview))
    }
    endTime <- Sys.time()
    if (i <= 5) {
      elapsed <- as.numeric(difftime(endTime,startTime,units="mins"))
      processingTime <- c(processingTime,elapsed)
    }
    if (numRecords >= 10 && i == 5) {
      .calcHOTProcTime(numRecords=numRecords,i=i,processingTime=processingTime,previewSize=previewSize)
    }
    if (!is.null(dir_out)) readr::write_csv(currRecCloudCover,csv_path)
    
    if (NCOL(currRecCloudCover) != 48) {
      View(currRecCloudCover)
      cat("problematic: ",currRecord[,1])
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

#' fills the record data.frame aoi cloud cover columns with NA cases if cc calculation failed or SAR is given
#' @param record data.frame with one row.
#' @param is_SAR logical if the record is a SAR acquisition. Default is FALSE.
#' @return record data.frame with one row but added columns.
#' @keywords internal
#' @noRd
.handle_cc_skip <- function(record, is_SAR = FALSE, dir_out = NULL) {
  
  if (!is.null(dir_out)) record[["cloud_mask_file"]] <- "NONE"
  record[["aoi_HOT_cloudcov_percent"]] <- ifelse(is_SAR,NA,100)
  record[["aoi_HOT_mean_probability"]] <- ifelse(is_SAR,NA,100)
  record[["scene_HOT_cloudcov_percent"]] <- ifelse(is_SAR,NA,9999)
  return(record)
  
}

#' mask the edges of Landsat preview raster
#' @param preview.
#' @return \code{preview_masked} masked preview
#' @importFrom sf as
#' @importFrom raster mask crs extent
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
#' @importFrom raster as.matrix extent resolution crs
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
    percent <- .calc_aoi_coverage(aoi,x)
  }
  # due to the calculation based on pixel values it might happen that 'percent' exceeds 100 slightly. In these cases use 100
  percent <- ifelse(percent > 100,100,percent)

}

#' calculates area in aoi in km2
#' @param aoi aoi.
#' @return aoi_area numeric
#' @importFrom raster area
#' @importFrom sf as
#' @keywords internal
#' @noRd
.calc_aoi_area <- function(aoi) {
  
  if (class(aoi) != "SpatialPolygons") {
    aoi_sp <- as(aoi,"Spatial")
  } else {aoi_sp <- aoi}
  aoi_area <- raster::area(aoi_sp) / 1000000
  
}

#' calculates the number of cells of value 1 covering the aoi
#' @param aoi aoi.
#' @param x raster with the resolution.
#' @return \code{percent} numeric percentage of value 1 covering the aoi
#' @importFrom raster extent raster res mask ncell values area aggregate
#' @keywords internal
#' @noRd
.calc_aoi_coverage <- function(aoi,x) {
  
  # calculate aoi number of cells (calculation is suitable for large areas)
  e <- extent(aoi)
  # calculate area of aoi in order to get a suitable resolution for percentage cells computations
  aoi_area <- .calc_aoi_area(aoi)
  correction <- aoi_area / 100000 # correction for resolution
  correction_small <- correction < 1
  correction <- ifelse(correction_small,1,correction)
  r <- raster(xmn=e[1],xmx=e[2],ymn=e[3],ymx=e[4],crs=crs(x),resolution=(res(x)*correction))
  values(r) <- 1
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

#' returns internal params used in select_*
#' @param mode character which mode is used: "TS", "BT" or "UT".
#' @param records data.frame.
#' @return \code{par} list of characters.
#' @keywords internal
#' @noRd
.select_params <- function(mode,records) {
  
  modes <- list("TS"="timeseries","BT"="bitemporal","UT"="unitemporal")
  par <- list(selected_col=paste0("selected_for_",modes[[mode]]), # logical column if a record is selected at all
              pmos_col="rgb_mosaic_file", # path to the RGB mosaic tif where record is included
              cmos_col="cmask_mosaic_file", # path to the cloud mask mosaic tif where record is included
              timestamp_col="selected_for_timestamp", # the timestamp number for which the record is selected
              aoi_cc_col="aoi_HOT_cloudcov_percent",
              tileid_col="tile_id",
              preview_col="preview_file",
              cloud_mask_col="cloud_mask_file",
              aoi_cc_prb_col="aoi_HOT_mean_probability",
              cc_index_col="cc_index",
              date_col="date_clear")
  par$sensor_group <- unique(records$sensor_group)[1]
  par$sensor <- unique(records$sensor)[1]
  par$date_col_orig <- ifelse(par$sensor_group == "Landsat","acquisitionDate","beginposition")
  par$id_col <- ifelse(par$sensor_group == "Landsat","entityId","title")
  par$tileids <- unique(records[[par$tileid_col]])
  par$sep <- "\n----------------------------------------------------------------"
  return(par)
  
}

#' creates a character date column in the format "YYYY-MM-DD" for Sentinel records
#' @param records data.frame.
#' @param date_col_orig character name of the original date colum.
#' @param date_col_name character name of the added date column.
#' @return \code{records} data.frame with added column "date_clear".
#' @keywords internal
#' @noRd
.extract_clear_date <- function(records, date_col_orig, date_col_name) {
  
  records[[par$date_col]] <- as.character(records[[par$date_col_orig]])
  sent_recs <- which(records$sensor_group=="Sentinel")
  for (i in sent_recs) {
    records[i,par$date_col] <- as.character(substr(records[i,par$date_col],1,10))
  }
  return(records)
  
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
#' @importFrom plyr compact
#' @keywords internal
#' @noRd
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
  preview_paths <- compact(preview_paths)
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
#' @importFrom plyr compact
#' @importFrom utils flush.console
#' @importFrom raster minValue maxValue writeRaster raster crs
#' @keywords internal
#' @noRd
.select_calc_mosaic <- function(records, base_records, aoi, sub, cloud_mask_col, 
                                min_improvement,
                                ts, dir_out, identifier) {
  
  dir_tmp <- .tmp_dir(dir_out,1)
  le_first_order <- length(sub[[1]])
  sub <- unlist(compact(sub))
  collection <- sapply(sub,function(x) cmask_path <- as.character(records[[cloud_mask_col]][x])) # get paths to cloud masks. This is the queue for processing
  # this vector are all orders ordered from best records (lowest aoi cc) to worst records (highest aoi cc) in a queue
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
  needs_addition <- TRUE # starting with TRUE because at the beginning it is always TRUE
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
    writeRaster(curr_base_mos_crop,p,overwrite=T)
    base_tmp <- c(crop_p,x)
    curr_mos_tmp <- .select_bridge_mosaic(base_tmp,aoi,curr_mos_tmp_p) # in this tile add next_record
    cov_aft <- .raster_percent(curr_mos_tmp,mode="aoi",aoi=aoi_subset) # check new coverage
    rm(next_record,base_mos,curr_base_mos_crop,curr_mos_tmp)
    # calculate by how much valid coverage is improved when adding the record to the tile area
    has_potential <- cov_aft > cov_init # only check for tile
    if (has_potential) {
      base_mos <- .select_bridge_mosaic(base_tmp,aoi,base_mos_path_new) # mosaic with added record
      base_cov_tmp <- .raster_percent(base_mos,mode="aoi",aoi=aoi)
      add_it <- .exceeds_min_improvement(min_improvement,base_coverage,base_cov_tmp)
      if (add_it) {
        base_records <- c(base_records,x) # add save path of current mosaic
        base_mos_path_new <- file.path(dir_tmp,paste0("base_mosaic_",i,"_tmp.tif")) # do not overwrite base_mos directly
        writeRaster(base_mos,base_mos_path,overwrite=T) # overwrite base mosaic
        unlink(base_mos_path_new)
        base_coverage <- base_cov_tmp
        rm(base_mos)
        cov <- as.character(round(base_coverage,2))
        cov <- ifelse(nchar(cov)==5,cov,paste0(cov,"0"))
        flush.console()
        cat("\r","-      ",cov,"  %")
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

#' checks if an new coverage percentage exceeds the min_improvement argument
#' @param min_improvement numeric.
#' @param cov_init numeric.
#' @param cov_aft numeric.
#' @return exceeds logical.
#' @keywords internal
#' @noRd
.exceeds_min_improvement <- function(min_improvement, cov_init, cov_aft) {
  exceeds <- covered_aft >= (covered_init + (((100 - covered_init) / 100) * min_improvement))
}

#' aggregates a raster according to the aoi area size.
#' @param x character vector of paths to rasters to check on.
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
  
  aoi_class <- class(aoi)
  error <- "try-error"
  if (class(aoi)[1] != "sf") aoi <- try(st_as_sf(aoi))
  if (inherits(aoi,error)) out(paste0("Aoi of class '",aoi_class,"' could not be converted to 'sf' object"),3)
  if (as.character(crs(aoi)) != as.character(crs)) aoi <- try(st_transform(aoi,crs))
  if (inherits(aoi,error)) out("Aoi reprojection failed",3)
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
  info <- paste0("Selected number of timestamps (",num_timestamps)
  s <- ifelse(length(sensor)==1,paste0("\n- Sensor: ",sensor),paste0("\nSensors: ",sensor))
  out(cat("- Number of timestamps selected:",num_timestamps,s))
  if (sub_period < r) {
    out(paste0(info,") results in shorter coverage frequency than sensor revisit time (",r,"). Decrease 'num_timestamps'"),3)
  } else if (sub_period == r) {
    out(paste0(info,") results in equal coverage frequency as revisit time (",r,"). It is unlikely to get cloud-free coverage this frequent"),1)
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

#' selects from a numeric data.frame column the i lowest value and checks if it is lower than a provided numeric
#' @param records data.frame.
#' @param max numeric maximum allowed value.
#' @param i numeric.
#' @param column character name of the column to be ordered.
#' @param max_column character name of the column for which max shall be checked. Can be equal to column.
#' @return \code{chosen} numeric index to the matching data.frame row.
#' @keywords internal
#' @noRd
.df_get_lowest <- function(records, max, i, column, max_column) {
  
  records_ord <- records[order(records[[column]]),]
  if (i > NROW(records_ord)) return(NA)
  j <- 1
  j <- ifelse(length(unique(records[[column]])) < i,i,1)
  chosen <- which(records[[column]]==records_ord[i,column])[j]
  val <- records[chosen,max_column]
  if (as.numeric(val) <= max) return(chosen) else return(NA)
  
}

#' selects initial records for the first sub-period
#' @param records data.frame subsetted to a sub-period.
#' @param tiles character vector of the tile ids.
#' @param period character vector of start and end date.
#' @param aoi_cc_col character name of aoi cloud cover column.
#' @param cc_index_col character name of the cloud cover index column.
#' @param tileid_col character name of tile id column.
#' @param date_col character name of the date column.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @return \code{sub} list of numeric vectors, each number is an index to a record in \code{records}
#' @keywords internal
#' @noRd
.select_sub <- function(records, tiles, max_cloudcov_tile,
                        aoi_cc_col, cc_index_col, tileid_col, date_col, 
                        identifier) {
  
  sub <- lapply(tiles,function(x) {
    rec_tile_sub <- records[which(records[[tileid_col]]==x),]
    i <- 0
    lwst_cc <- 0
    selected <- c()
    while(!is.na(lwst_cc)) {
      i <- i+1
      lwst_cc <- .df_get_lowest(rec_tile_sub,max=max_cloudcov_tile,i,
                                column=cc_index_col, max_column=aoi_cc_col)
      if (!is.na(lwst_cc)) {
        selected[i] <- which(records[,identifier] == rec_tile_sub[lwst_cc,identifier])
      }
    }
    return(unique(selected))
  })
  names(sub) <- tiles
  return(sub)
}

#' prep process of a selection process
#' @param records data.frame.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param par list holding everything inserted into this parameter list in the calling select function (8 parameters).
#' @param identifier numeric identifier column.
#' @return records data.frame with 
#' @keywords internal
#' @noRd
.select_prep <- function(records, num_timestamps, par, identifier) {
  
  has_error <- .catch_missing_columns(records,cols=c(par$aoi_cc_col,par$aoi_cc_prb_col,
                                                     par$preview_col,par$cloud_mask_col))
  if (has_error) out("Argument 'records' cannot be processed as it lacks relevant columns/values",3)
  records <- .extract_clear_date(records,par$date_col_orig,par$date_col)
  records <- .make_tileid(records,identifier)
  period <- .identify_period(records[[par$date_col]])
  # calculates the sub_period column
  records <- .select_sub_periods(records,period,num_timestamps,par$date_col) 

}

#' main process of a selection process
#' @param records data.frame.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @param par list holding everything inserted into this parameter list in the calling select function (8 parameters).
#' @param dir_out character directory where to save intermediate product.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @param timestamp numeric of the current timestamp.
#' @return \code{selected} list of selected records with all items returned by .select_process_sub
#' @keywords internal
#' @noRd
.select_main <- function(records, 
                         min_distance, max_sub_period, max_cloudcov_tile, 
                         min_improvement, prio_sensors = NULL,
                         par, dir_out, identifier, timestamp) {
  
  period_new <- c()
  base_records <- c()
  ids <- c()
  valid_pixels <- 0
  if (is.null(prio_sensors)) prio_sensors <- 1
  le_prio_is_one <- length(prio_sensors) == 1
  for (s in prio_sensors){
    if (le_prio_is_one) {
      # in case prio_sensors is not given process all sensors together
      s_match <- which(!is.na(records$sensor))  
    } else {
      # in case prio_sensors is given process sensors in this order
      s_match <- which(records$sensor==s)
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
      tstamp$records <- .within_period(records,tstamp$period,par$date_col) # subset to records in period
    }
    # run the selection process
    selected <- .select_process_sub(tstamp$records,tstamp$period,
                                    period_new=period_new,base_records=base_records,
                                    max_sub_period,max_cloudcov_tile=max_cloudcov_tile,
                                    min_improvement=min_improvement,
                                    par=par,dir_out=dir_out,identifier=identifier,ts=timestamp)
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

}

#' checks if all files in a vector of paths are on disk
#' @param paths character vector of paths to files to check on.
#' @param item_name character name of the type of files checking on. E.g. "preview_file".
#' @return In case all are given: NA. Else: it throws a warning
#' @keywords internal
#' @noRd
.select_catch_files <- function(paths, item_name) {
  
  paths <- paths[intersect(which(!is.na(paths)),which(paths != "NONE"))]
  exist <- sapply(paths,function(p) file.exists(p))
  all_on_disk <- isTRUE(all(exist))
  if (all_on_disk) {
    return(NA)
  } else {
    number_not_found <- which(exist == FALSE)
    out(
      paste0("All files in '",item_name,"' have to be saved at the location as indicated
in the paths. Out of ",length(paths)," files ",number_not_found," cannot be located"),2)
  }
   
}

#' checks the prio_sensors argument
#' @param prio_sensors character vector of sensors ordered by preference (first highest priority, selected first).
#' @return nothing. In case of problematic input in prio_sensors: error.
#' @keywords internal
#' @noRd
.select_check_prio_sensors <- function(prio_sensors) {
  
  if (class(prio_sensors) != "character") out("Argument 'prio_sensors' has to be of class character (vector)",3)
  optical_sensors <- c("Sentinel-2","Sentinel-3","Landsat-5","Landsat-7","Landsat-8","MODIS")
  some_wrong <- isFALSE(any(sapply(prio_sensors,function(x) check <- x %in% optical_sensors)))
  if (some_wrong) {
    out("Argument 'prio_sensors' has to be provided with sensor names in the same format as returned by get_names()",3)
  }
  
}

#' select timestamps for SAR data according to num_timestamps and min_distance. This
#' can be the fundament for an optical selection or if has_SAR == 100 (only SAR in records) it is the only selection
#' @param records data.frame.
#' @param period_new list of character vectors of two dates, one vector for each timestamp.
#' @param min_distance numeric the minimum number of days between two used acquisitions for distinguished timestamps.
#' @param num_timestamps numeric the number of timestamps the timeseries shall cover.
#' @param max_sub_period
#' @param par list holding everything inserted into this parameter list in the calling select function.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @return \code{SAR_selected} list of [[ids]] character vector of selected ids per timestamp, [[period]] character vector
#' of two dates and [[sub-period]] numeric the sub-period number
#' character vector of two dates (start and end date of SAR sub-period)
#' @keywords internal
#' @noRd
.select_SAR <- function(records, period_new = NULL, 
                        max_sub_period, min_distance, num_timestamps, par, identifier) {
  
  subperiods <- unique(records$sub_period)
  # if the length of the current sub-period is longer than max_sub_period grade all dates in sub_period
  # according to the number of the given records for each date and calculate the distance of each record
  # from this date. Exclude records consecutively until max_sub_period is reached
  selected_SAR <- list() # to be filled with the selected lists of selected ids and sub-periods
  for (s in subperiods) {
    records_in_s <- records[which(records$sub_period==s),]
    if (s > 1) {
      # enforce min_distance
      previous_period <- selected[[i-1]][["period"]] # get the previous selected sub-period
      period_initial <- .identify_period(records_in_s[[par$date_col]])
      # earliest date of next sub-period adjusted
      first_date <- .select_force_distance(previous_period,min_distance)
      period_s <- .select_handle_next_sub(first_date,period_initial,
                              min_distance,max_sub_period)
      records_in_s <- .within_period(records_in_s,period_s) # subset to records within period_s
    }
    tiles_s <- records_in_s[[par$tileid_col]]
    dates_s <- sapply(records_in_s[[par$date_col]],as.Date)
    sub_period_le <- length(max(dates_s)-min(dates_s))
    # grade dates and exclude consecutively
    if (sub_period_le > max_sub_period) {
      dates_seq <- min(dates_s):max(dates_s)
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
      incl <- .select_subset_to_best_period(dates_s, best_period)
      ids <- records_in_s[incl,identifier] # ids of selected records in upated sub-period
    } else {
      ids <- records_in_s[[identifier]] # all ids of records in sub-period
    }
    dates_sel <- records_in_s[which(ids %in% records[[identifier]]),par$date_col]
    period <- .identify_period(dates_sel)
    selected_SAR[[s]] <- list("ids"=ids,"period"=period,"sub-period"=s)
  }
  
  return(selected_SAR)

}


#' calls the different steps of selection for a sub-period
#' @param records data.frame subsetted to a sub-period.
#' @param period character vector of start and end date.
#' @param period_new character vector an existing period for the timestamp. Default is c().
#' @param base_records character vector of paths to cloud masks that create a base mosaic. Default is NULL.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param max_cloudcov_tile numeric maximum cloud cover per tile.
#' @param min_improvement numeric the minimum increase of valid pixels percentage in mosaic when adding record.
#' The value is the percentage of not yet covered area that shall be covered additionally when adding the record.
#' @param par list holding everything inserted into this parameter list in the calling select function (8 parameters).
#' @param dir_out character directory where to save intermediate product.
#' @param identifier numeric indicating a unique identifier in the records data.frame.
#' @param ts numeric of the current timestamp.
#' @return \code{selected} list of [[ids]] character vector of selected ids, [[cMask_paths]] character vector to cloud masks, 
#' [[valid_pixels]] percentage of valid pixels in mosaic with the given selection, [[period]] charater vector of two dates.
#' @keywords internal
#' @noRd
.select_process_sub <- function(records, period, period_new = c(), base_records = NULL,
                                max_sub_period, max_cloudcov_tile, min_improvement, 
                                par, dir_out, identifier, ts) {
  
  tiles <- unique(records[[par$tileid_col]])
  tiles <- tiles[!is.na(tiles)]
  # the sub is an ordering of all available records per tile according to aoi cloud cover and aoi cloud cover probability
  sub <- .select_sub(records=records,tiles=tiles,max_cloudcov_tile=max_cloudcov_tile,
                     aoi_cc_col=par$aoi_cc_col,cc_index_col=par$cc_index_col,tileid_col=par$tileid_col,
                     date_col=par$date_col,
                     identifier=identifier)
  sub_within <- .select_force_period(records,sub,period,max_sub_period,period_new=period_new,
                                     date_col=par$date_col,aoi_cc_col=par$aoi_cc_col,
                                     cc_index_col=par$cc_index_col)
  # make best mosaic of cloud masks for first timestamp
  out(par$sep)
  out(paste0("Calculating best mosaic for timestamp: ",ts))
  selected <- .select_calc_mosaic(records,base_records=base_records,
                                  aoi,sub_within,par$cloud_mask_col,
                                  min_improvement=min_improvement,
                                  ts=ts,dir_out,identifier)
  selected$period <- .identify_period(records[records[[identifier]]==selected$ids])
  out(paste0("\nCompleted selection process for timestamp: ",ts,"\n"))
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
.select_finish_SAR <- function(records, selected_SAR, par) {
  
  csw_SAR <- .select_SAR_summary(selected_SAR,records,par)
  summary <- .out_vector(csw_SAR[[1]]) # SAR selection summary
  w <- cws_SAR[[2]]
  if (!is.null(w)) out(w,type=2) # warning
  ids <- sapply(selected_SAR,function(x) {return(x[["ids"]])})
  # add columns to records
  cols <- c(par$selected_col,par$timestamp_col)
  records <- .select_prep_cols(records,cols)
  for (ts in 1:length(ids)) {
    ids_match <- match(ids[i],records[[identifier]])
    records[ids_match,par$selected_col] <- TRUE # is record selected at all
    records[ids_match,par$timestamp_col] <- ts # timestamp for which record is selected
  }
  return(records)
  
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
.select_save_mosaics <- function(records, selected, aoi, selected_col, pmos_col, cmos_col, identifier, dir_out) {
  
  console_info <- list()
  cols <- c(selected_col,timestamp_col,pmos_col,cmos_col)
  records <- .select_prep_cols(records, cols)
  for (i in 1:length(selected)) {
    s <- selected[[i]]
    id_sel <- s$ids
    #A cloud mask mosaic
    save_path_cmos <- .select_cmask_mos(s,aoi,dir_out)
    #B preview mosaic
    save_path_pmos <- .select_preview_mos(records,s,aoi,i,identifier,dir_out,
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

#' creates new columns for selection completion and fills with NAs or FALSE
#' @param records data.frame.
#' @param cols character vector of the column names.
#' @return records data.frame with new columns.
#' @keywords internal
#' @noRd
.select_prep_cols <- function(records, cols) {
  
  for (j in 1:length(cols)) {
    col <- cols[j]
    val <- ifelse(col == selected_col,FALSE,NA)
    records[[col]] <- val
  }
  
}

#' returns a sub list of indices pointing to records within max_sub_period, returned in orders according to aoi cloud cover 
#' @param records data.frame.
#' @param sub list of numeric vectors each pointing to a record.
#' @param period character vector of start and end date.
#' @param max_sub_period numeric maximum number of days to use for creating a mosaic per timestamp if mosaicking is needed.
#' @param period_new character vector of an existing period for this timestamp.
#' @param date_col character name of the date column.
#' @param aoi_cc_col character name of aoi cloud cover column.
#' @param cc_index_col character name of the cloud cover index column.
#' @return \code{sub_within} list of numeric vectors, each number is an index to a record in \code{records}
#' @importFrom plyr compact
#' @keywords internal
#' @noRd
.select_force_period <- function(records, sub, period, max_sub_period, period_new, 
                                 date_col, aoi_cc_col, cc_index_col) {
  
  # check if covered period of timestamp is within max_sub_period and re-calculate period consecutively with record of next-lowest cloud cover
  sub <- compact(sub)
  max_num_sel <- max(sapply(sub,length))
  orders <- sapply(1:max_num_sel,function(i) unlist(sapply(sub,function(x) return(x[i])))) # to matrix
  orders <- data.frame(orders)
  sub_within <- list()
  for (i in 1:NCOL(orders)) {
    x <- orders[,i]
    # first try to use all records of this order
    order <- x[!is.na(x)]
    period_tmp <- .select_bridge_period(records,order,period_new,date_col)
    period_tmp_le <- .calc_days_period(period_tmp)
    if (period_tmp_le <= max_sub_period) { # the case where all records from current order x are within period_new
      period_new <- period_tmp
      sub_within[[i]] <- order
    } else {
      # for the case where at least one of record of order x is not within period_new
      # try with all values in all possible combinations (not orders). Might be that 
      # from 0 to all records except one are within period
      order_within <- .select_remove_dates(order, records, period_new, max_sub_period, date_col, aoi_cc_col)
      period_new <- c(period_new,.select_bridge_period(records,order_within,period_new,date_col))
      sub_within[[i]] <- order_within
    }
  }
  return(sub_within)
  
}

#' creates a cloud cover index value per record, a synthesis of aoi cloud cover % and aoi mean cloud cover probability
#' @param records data.frame.
#' @param aoi_cc_col character name of aoi cloud cover column.
#' @param aoi_cc_prb_col character name of the mean aoi cloud cover probability column.
#' @param cc_index_col character name of the added column holding the cloud cover index.
#' @param ratio numeric between 0 and 1. How to weight cloud cover and cloud probability. The value steers
#' the weight of cloud cover, all remaining is cloud probability. Default is 0.7.
#' @return \code{records} data.frame with one additional column: 'cc_index'.
#' @keywords internal
#' @noRd
.select_cc_index <- function(records, aoi_cc_col, aoi_cc_prb_col, cc_index_col, ratio = 0.7) {
  
  aoi_cc <- as.numeric(records[[aoi_cc_col]]) # aoi cc cover
  aoi_cc_prb <- as.numeric(records[[aoi_cc_prb_col]]) # mean aoi cc probability
  cc_index <- ((aoi_cc * ratio) + (aoi_cc_prb * (1 - ratio)))
  records[[cc_index_col]] <- cc_index
  return(records)
    
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
    cc <- sapply(records[sel,aoi_cc_col],function(c) {100-as.numeric(c)}) # turn low cc values into high ones because high counts of sel are good
    value <- ifelse(length(cc)==0,0,mean(length(sel) * cc))
    date_grade[[as.character(d)]] <- value
  }
  
  # select best period with maximum values in sum of grades while ensuring max_sub_period
  # if a period_new is given test all possible periods combined with period_new
  best_period <- .select_best_period(date_grade = date_grade, dates_seq = dates_seq, 
                                     min_date = min_date, max_date = max_date,
                                     period_new = period_new,
                                     max_sub_period = max_sub_period)
  incl <- .select_subset_to_best_period(dates, best_period)
  order <- x[incl]
  return(order)
  
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
  
  max_sub_half <- (max_sub_period - 1) / 2
  if ((max_sub_half %% 2) != 0) max_sub_half <- max_sub_half - 0.5
  if (is.null(period_new)) {
    # for each date in dates_seq create the sub_period around it according to max_sub_period
    # calculate the sum grade of all dates within that sub_period
    # check optionally if this sub_period matches max_sub_period together within period_new
    # return the mean grade value or NA
    sum_grade <- sapply(dates_seq,function(d) {
      period_tmp <- c(d - max_sub_half, d + max_sub_half)
      if (period_tmp[1] < min_date) period_tmp[1] <- min_date
      if (period_tmp[2] > max_date) period_tmp[2] <- max_date
      first <- which(dates_seq==period_tmp[1])
      last <- which(dates_seq==period_tmp[2])
      sum_grade <- sum(unlist(date_grade[first:last]))
      return(sum_grade)
    })
    best_middle_date <- dates_seq[which(sum_grade == max(sum_grade))][1]
  } else {
    # find optimal new sub-period from period_new and given grades of dates
    # chose each date in period_new as middle date of a combined period once and return
    # the grade for the dates included from the included records from date_grade
    # but only if this shifted period 
    period_new_date <- sapply(period_new,as.Date)
    period_new_seq <- period_new_date[1]:period_new_date[2]
    shifted_grades <- sapply(period_new_seq,function(d) {
      period_tmp <- c(d - max_sub_half,d + max_sub_half)
      period_new_tmp <- .identify_period(c(period_new_date,period_tmp))
      period_new_tmp_seq <- period_new_tmp[1]:period_new_tmp[2]
      if (length(period_new_tmp_seq) > max_sub_period) return(NA)
      sum_grade <- sum(unlist(date_grade[which(period_new_tmp_seq %in% dates_seq)]))
      return(sum_grade)
    })
    best_middle_date <- period_new_seq[which(shifted_grades == max(shifted_grades))][1]
  }
  best_period <- c((best_middle_date - max_sub_half),best_middle_date + max_sub_half)
  any_difference <- max_sub_period - length(best_period[1]:best_period[2])
  # due to rounding
  if (any_difference > 0) {
    add_dates <- c(as.character(best_period[1]-1),as.character(best_period[2]+1))
    add_grades <- c(date_grade[[add_dates[1]]],date_grade[[add_dates[2]]])
    # if add_grades is null then there are no records given and it does not matter
    add_this <- ifelse(is.null(add_grades),1,which(add_grades == max(add_grades)))
    if (add_this == 1) {
      best_period[1] <- best_period[1] - 1
    } else {
      best_period[2] <- best_period[2] + 1 
    }
  }
  return(best_period)
  
}

#' bridge function to the period identifier \link{.identify_period}. Enables to calculate from
#' a given period with added dates a new period.
#' @param records data.frame.
#' @param order numeric vector pointing to elements of records.
#' @return period_new character holding a period of dates.
#' @param date_col character name of the date column.
#' @keywords internal
#' @noRd
.select_bridge_period <- function(records, order, period_new, date_col) {
  
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

#' creates clean tile ids
#' @param records data.frame.
#' @param identifier numeric identifier column.
#' @return \code{records} data.frame with an added column: 'tile_id'
#' @keywords internal
#' @noRd
.make_tileid <- function(records, identifier) {
  
  sensor_groups <- unique(records$sensor_group)
  for (s in sensor_groups) {
    if (s == sensor_groups[1]) {
      titles <- records[which(records$sensor_group==s),identifier]
      tile_id <- sapply(titles,function(x) {return(substr(x,39,44))})
    } else if (s %in% sensor_groups[2:3]) {
      tile_id <- paste0(records$WRSPath,records$WRSRow)
    }
  }
  records[["tile_id"]] <- tile_id
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
  dates <- sapply(0:num_timestamps,function(i) date <- period[1] + (i * le_subperiods))
  l <- length(dates)
  dates[l] <- dates[l] + 1
  date_col_mirr <- sapply(records[,date_col],as.Date) # mirror of the date column as days since 1970-01-01
  for (i in 1:num_timestamps) {
    within <- intersect(which(date_col_mirr >= dates[i]),which(date_col_mirr < dates[i+1]))
    records[within,"sub_period"] <- i
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
               these values disable the creation of a temporally consistent selection. Modify the values (most likely decrease (some of) them."),3)  
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

#' checks if columns are given in records and if they have values
#' @param records data.frame.
#' @param cols character vector of column names to be checked on.
#' @return nothing. Console error if column is not given.
#' @keywords internal
#' @noRd
.catch_missing_columns <- function(records, cols) {
  
  missing <- c()
  empty <- c()
  for (col in cols) {
    is_missing <- isFALSE(col %in% names(records))
    is_empty <- all(is.na(records[[col]]))
    if (isTRUE(is_missing)) {
      missing <- c(missing,col)
    }
    if (isTRUE(is_empty)) {
      empty <- c(empty,col)
    }
  }
  for (fail in unique(c(missing,empty))) {
    if (fail %in% missing) out(paste0("Argument 'records' lack needed columns:\n",fail,"\n"),2)
    if (fail %in% empty) out(paste0("Arguent 'records' has empty columns that should have values:\n",fail,"\n"),2)
  }
  
  error <- sapply(c(missing,empty),function(x) !is.null(x))
  if (TRUE %in% error) return(TRUE) else return(FALSE)
  
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
.select_SAR_summary <- function(selected_SAR, records, par) {
  
  sep <- "\n----------------------------------------------------------------"
  covered_tiles_ts_wise <- sapply(selected_SAR,function(s) {
    num_tiles <- length(unique(records[match(s[["ids"]],records[[identifier]]),par$tileid_col]))
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








  