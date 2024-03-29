#' .get
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
.get <- function(url, username = NULL, password = NULL, dir.file = NULL, prog = F){
  
  x <- NULL # needed due to checks
  get.str <-"x <- try(GET(url"
  if(!is.null(username)) get.str <- paste0(get.str, ", authenticate(username, password)")
  if(!is.null(dir.file)) get.str <- paste0(get.str, ", write_disk(dir.file)")
  if(isTRUE(prog)) get.str <- paste0(get.str, ", progress()")
  get.str <- paste0(get.str, "), silent = T)")
  eval(parse(text = get.str))
  
  #if(inherits(x, "try-error")) out(paste0("Could not process request: ", gsub("  ", "", strsplit(x[[1]], "\n")[[1]][2])), type=3)
  if(!inherits(x, "try-error")){
    stop_for_status(x, "process request")
    warn_for_status(x)
    #message_for_status(x); cat("\n")
  }
  return(x)
}


#' get odata for uuid
#'
#' @param uuid one or multiple uuids
#' @param cred return of .CopHub_select c(user, password, API ulr)
#' @param field field to be checked
#' @keywords internal
#' @noRd
.get_odata <- function(uuid, cred, field = ""){
  lapply(uuid, function(x) content(.get(paste0(cred[3], "odata/v1/Products('", x, "')/", field),  cred[1], cred[2])))
}


#' get dataset urls
#' 
#' @param records records data.frame
#' @return character vector
#' 
#' @importFrom xml2 xml_children xml_contents
#' @importFrom httr content http_error
#' @importFrom utils tail head
#' @noRd 
.get_ds_urls <- function(records){
  .apply(records, MARGIN = 1, function(x){
    
    # Sentinel Copernicus Hub
    if(x$product_group == "sentinel"){
      paste0(unlist(x$gSD.cred)[3], "odata/v1/Products('", x$entity_id, "')/$value")
      
      # landsat 8 Level 1A AWS
    } else if(x$product_group == "landsat"){
      
      if(x$level == "l1"){
        # assemble index url
        hv <- strsplit(x$record_id, "_")[[1]][3]
        index_url <- paste0(getOption("gSD.api")$aws.l8, substr(hv, 1, 3), "/", substr(hv, 4, 6), "/", x$record_id, "/index.html")
        
        # get file urls
        list(paste0(gsub("index.html", "", index_url), .sapply(as.character(xml_children(xml_children(xml_contents(content(.get(index_url), encoding = "UTF-8"))[2])[4])), function(y){
          strsplit(y,  '\"')[[1]][2]
        }, USE.NAMES = F)))
      } else{
        x[["gSD.espa_item"]][["product_dload_url"]]
      }
      # modis LAADS
    } else if(x$product_group == "modis"){
      
      # assemble file url
      fn <- strsplit(x$summary, "[.]")[[1]]
      ydoy <- gsub("A", "", fn[2]) #positional
      
      url <- paste0(
        getOption("gSD.api")$laads,
        toString(as.numeric(fn[4])), "/",
        fn[1], "/",
        substr(ydoy, 1, 4), "/",
        substr(ydoy, 5, nchar(ydoy)), "/", x$summary
      )
      
      # EROS legacy API 1.4.:
      # fn <- gsub("Entity ID: ", "", strsplit(x$summary, ", ")[[1]][1]) #positional
      # ydoy <- gsub("A", "", strsplit(fn, "[.]")[[1]][2]) #positional
      # url <- paste0(getOption("gSD.api")$laads, toString(as.numeric(strsplit(fn, "[.]")[[1]][4])), "/", strsplit(fn, "[.]")[[1]][1], "/", substr(ydoy, 1, 4),
      #               "/", substr(ydoy, 5, nchar(ydoy)), "/", fn)
      
      # test url
      if(http_error(url)){
        fn.names <- .get(paste0(paste0(head(strsplit(url, "/")[[1]], n = -1), collapse = "/"), ".csv"))
        fn.names <- .sapply(gsub("\r", "", strsplit(content(fn.names), "\n")[[1]][-1]), function(y) strsplit(y, ",")[[1]][1], USE.NAMES = F)
        
        # assign correct fn
        fn <- grep(paste0(strsplit(tail(strsplit(url, "/")[[1]], n=1), "[.]")[[1]][1:4], collapse = "."), fn.names, value = T)
        
        # redefine URL and file
        return(paste0(paste0(head(strsplit(url, "/")[[1]], n=-1), collapse = "/"), "/", fn))
      } else return(url)
    
    # srtm CMR  
    } else if(x$product_group == "srtm"){
      x$links[grep("hgt.zip$", x$links)]
    } else NA
  })
}


#' get dataset filenames
#' 
#' @param records records data.frame
#' @return character vector
#' @importFrom pbapply pbapply
#' @importFrom utils tail
#' @noRd 
.get_ds_filenames <- function(records){
  .apply(records, MARGIN = 1, function(x){
    file <- paste0(x$gSD.dir, "/", x$record_id)
    
    if(x$product_group == "sentinel"){
      if(grepl("GNSS", x$product)){
        paste0(file, ".TGZ")
      } else if(x$product == "sentinel-5p"){
        paste0(file, ".nc")
      } else{
        paste0(file, ".zip")
      }
      
    } else if(x$product_group == "landsat"){
      
      if(x$level == "l1"){
        if(!dir.exists(file)) catch <- try(dir.create(file, recursive = T), silent = T)
        list(paste0(file, "/", .sapply(unlist(x$dataset_url, recursive = T), function(x) tail(strsplit(x, "/")[[1]], n = 1), USE.NAMES = F)))
      } else{
        paste0(file, "_LEVEL_", x$level, ".tar.gz")
      } 
    } else if(any(x$product_group == "modis", x$product_group == "srtm")){
      paste0(x$gSD.dir, "/", tail(strsplit(x$dataset_url, "/")[[1]], n=1)[1])
    } else NA
  })
}


#' translate gSD CMR product names to CMR concept id
#' @param products product name vector
#' @return CMR concept ids
#' @keywords internal
#' @noRd
.getCMR_id <- function(products = NULL){
  srtm_names <- list("srtm_global_3arc_v003" = "C204582034-LPDAAC_ECS",
                     "srtm_global_1arc_v001" = "C1000000240-LPDAAC_ECS")
  if(is.null(products)) unlist(srtm_names) else unlist(srtm_names[products])
}

#' .post
#' @param url url
#' @param username user
#' @param password pass
#' @param ... body
#' 
#' @importFrom httr POST stop_for_status warn_for_status message_for_status progress authenticate
#' 
#' @keywords internal
#' @noRd
.post <- function(url, username = NULL, password = NULL, ...){
  
  # x <- NULL # needed due to checks
  # post.str <-"x <- POST(url"
  # if(!is.null(username)) post.str <- paste0(post.str, ", authenticate(username, password)")
  # post.str <- paste0(post.str, ", body = body)")
  #eval(parse(text = post.str))
  
  if(!is.null(username)) x <- POST(url, authenticate(username, password), ...) else x <- POST(url, ...)
  stop_for_status(x, "connect to server.")
  warn_for_status(x)
  #message_for_status(x); cat("\n")}
  return(x)
}


#' .download
#' @param x file record
#' @param names column names of record
#' @param prog logical
#' @param force logical
#' @importFrom tools md5sum
#' @importFrom utils tail
#' @keywords internal
#' @noRd
.download <- function(url, file, name, head, type = "dataset", md5 = NA, prog = T, force = F, ...){
  
  if(file.exists(file) & !isTRUE(force)){
    #out(paste0("\r", head, "Skipping download of ", type, " '", name, "', since '", file, "' already exists..."), flush = T)
    out(paste0(head, "Skipping download of ", type, " '", name, "', since '", file, "' already exists..."))
    return(TRUE)
  } else{
    
    out(paste0(head, "Downloading ", type, " '", name, "' to '", file, "'..."))
    file.tmp <- tempfile(tmpdir = paste0(head(strsplit(file, "/")[[1]], n=-1), collapse = "/")) #, fileext = ".tar.gz")
    response <- try(.get(url, dir.file = file.tmp, prog = prog, ...), silent = T)
    
    if(inherits(response, "try-error")){
      if(grepl("aborted", as.character(attributes(response)$condition))) out("Operation was aborted by an application callback.", type = 3)
      out(paste0(head, "Download of ", type, " '", name, "' failed:", gsub("\n", "", tail(strsplit(response[1], "\n ")[[1]], n = 1))), type = 2)
      file.remove(file.tmp)
      return(FALSE)
    }
    
    if(!is.na(md5)){
      if(!as.character(md5sum(file.tmp)) == tolower(md5)){
        out(paste0(head, "Download of ", type, " '", name, "' failed: MD5 check sums do not match."), type = 2)
        file.remove(file.tmp)
        return(FALSE)
      }
    }
  }
  
  file.rename(file.tmp, file)
  return(TRUE)
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


#' get ERS API key from user input
#'
#' @param username username
#' @param password password
#' @keywords internal
#' @importFrom httr POST content stop_for_status warn_for_status content_type user_agent
#' @noRd
.ERS_login <- function(username, password, n_retry = 3){
  x <- .post(url = paste0(getOption("gSD.api")$ee, "login"),
             username = NULL, password = NULL,
             body = list(username = username, password = password),
             encode = "json",
             user_agent("httr"))
  stop_for_status(x, "connect to server.")
  warn_for_status(x)
  v <- content(x)$data
  if(is.null(v)) out("Login failed. Please retry later or call services() to check if USGS services are currently unavailable.", type = 3)
  return(v)
}

#' logout from ERS with API key
#'
#' @param api.key api.key
#' @keywords internal
#' @noRd
.ERS_logout <- function(api.key){
  x <- .post(url = paste0(getOption("gSD.api")$ee, "logout"),
             username = NULL, password = NULL,
             add_headers('X-Auth-Token' = api.key),
             encode = "json",
             user_agent("httr"))
  #x <- .get(paste0(getOption("gSD.api")$ee, 'logout?jsonRequest={"apiKey":"', api.key, '"}'))
  stop_for_status(x, "connect to server.")
  warn_for_status(x)
  content(x)$data
}


#' get EE products
#'
#' @param api.key api.key
#' @param wildcard wildcard
#' @keywords internal
#' @importFrom httr add_headers
#' @noRd
.EE_ds <- function(api.key, wildcard = NULL){
  x <- .post(url = paste0(getOption("gSD.api")$ee, 'dataset-search'), 
             username = NULL, password = NULL,
             add_headers('X-Auth-Token' = api.key),
             body = list(datasetName = wildcard), encode = "json")
  sapply(content(x)$data, function(y) y$datasetAlias, USE.NAMES = F)
  #q <- paste0(getOption("gSD.api")$ee, 'datasets?jsonRequest={"apiKey":"', api.key, '"}') #, if(is.null(wildcard)) '}' else  ',"datasetName":"', wildcard, '"}')
  #if(!is.null(wildcard)) q <- gsub("}", paste0(',"datasetName":"', wildcard, '"}'), q)
  #x <- .get(q)
}


#' query EE
#'
#' @param aoi aoi
#' @param time_range time_range
#' @param product_name name
#' @param api.key api.key
#'
#' @importFrom sf st_bbox st_as_text
#' @importFrom xml2 as_list
#'
#' @keywords internal
#' @noRd
.EE_query <- function(aoi, time_range, product_name, api.key){
  
  # assemble request body with a few default fields
  req_body <- list(
    datasetName = product_name,
    sceneFilter = list(
      spatialFilter = list(
        filterType = "mbr",
        lowerLeft = list(
          latitude = st_bbox(aoi)$ymin,
          longitude = st_bbox(aoi)$xmin
        ),
        upperRight = list(
          latitude = st_bbox(aoi)$ymax,
          longitude = st_bbox(aoi)$xmax
        )
      ),
      acquisitionFilter = list(
        end = time_range[2],
        start = time_range[1]
      )
    ),
    startingNumber = 1,
    maxResults = 50000,
    sortDirection = "ASC",
    metadataType = "full"
  )
  
  #spatialFilter <- paste0('"spatialFilter":{"filterType":"mbr","lowerLeft":{"latitude":', st_bbox(aoi)$ymin, ',"longitude":', st_bbox(aoi)$xmin, '},"upperRight":{"latitude":', st_bbox(aoi)$ymax, ',"longitude":', st_bbox(aoi)$xmax, '}}')
  #temporalFilter <- paste0('"temporalFilter":{"startDate":"', time_range[1], '","endDate":"', time_range[2], '"}')
  
  out(paste0("Searching records for product name '", product_name, "'..."))
  #query <- lapply(product_name, function(x, ak = api.key, sf = spatialFilter, tf = temporalFilter) .get(paste0(getOption("gSD.api")$ee, 'search?jsonRequest={"apiKey":"', ak,'","datasetName":"', x,'",',sf,',', tf, ',"startingNumber":1,"sortOrder":"ASC","maxResults":50000}')))
  query <- lapply(product_name, function(x){
    .post(
      url = paste0(getOption("gSD.api")$ee, "scene-search?"),
      username = NULL, password = NULL, 
      add_headers('X-Auth-Token' = api.key),
      body = req_body,
      encode = "json",
      user_agent("httr")
    )
  })
  query.cont <- lapply(query, content)
  if(all(c(length(product_name) == 1), !is.null(query.cont[[1]]$errorCode))){
    out("No results could be obtained for this product, time range and AOI.", msg = T)
  } else{
    
    query.use <- sapply(query.cont, function(x) if(is.null(x$error) & length(x$data$results) != 0) T else F, USE.NAMES = F)
    query.cont <- query.cont[query.use]
    query.names <- product_name[query.use]
    
    query.results <- lapply(query.cont, function(x) x$data$results)
    if(length(query.results) != 0){
      
      query.df <- unlist(mapply(y = query.results, n = query.names, function(y, n) lapply(y, function(x, ds_name = n){
        x.names <- names(x)
        fields.expl <- c("browse", "metadata", "spatialCoverage", "spatialBounds")
        
        # fields that can be parsed generically
        x.nonexpl <- x[!(x.names %in% fields.expl)]
        x.nonexpl <- unlist(x.nonexpl, recursive = T, use.names = T)
        
        # populate data.frame
        df <- rbind.data.frame(x.nonexpl, stringsAsFactors = F)
        colnames(df) <- names(x.nonexpl)
        
        # handle some fields explicitely
        # metadata
        x.metadata <- sapply(x$metadata, function(z) z$value, USE.NAMES = F)
        if(length(x.metadata) > 0){
          sub.valid <- !sapply(x.metadata, is.null)
          x.metadata <- rbind.data.frame(x.metadata[sub.valid], stringsAsFactors = F)
          colnames(x.metadata) <- sapply(x$metadata[sub.valid], function(z) z$fieldName, USE.NAMES = F)
          
          # spatialCoverage
          x.spf.sub <- grep("spatialCoverage", x.names)
          x.spf <- unlist(x[x.spf.sub])
          if(!is.null(x.spf)){
            x.spf <- as.numeric(x.spf[grep("coordinates", names(x.spf))])
            x.spf <- st_as_text(.check_aoi(
              cbind(
                x.spf[seq(1, length(x.spf), by = 2)], 
                x.spf[seq(2, length(x.spf), by = 2)]
              ), type = "sf", quiet = T)
            )
          } else if(!is.null(x.metadata$`NW Corner Lat dec`)){
            if(nchar(x.metadata$`NW Corner Lat dec`) > 0){
              x.spf <- cbind(
                as.numeric(c(x.metadata$`NE Corner Long dec`,
                  x.metadata$`NW Corner Long dec`,
                  x.metadata$`SW Corner Long dec`,
                  x.metadata$`SE Corner Long dec`,
                  x.metadata$`NE Corner Long dec`)),
                as.numeric(c(
                  x.metadata$`NE Corner Lat dec`,
                  x.metadata$`NW Corner Lat dec`,
                  x.metadata$`SW Corner Lat dec`,
                  x.metadata$`SE Corner Lat dec`,
                  x.metadata$`NE Corner Lat dec`)))
              x.spf <- st_as_text(.check_aoi(x.spf, type = "sf", quiet = T))
            } else{
              x.spf <- NA
            }
          } else{
            x.spf <- NA
          }
        } else{
          x.spf <- NA
          x.metadata <- NULL
        }
        # drop spatialBounds as it would return the same Polygon
        
        # browse
        x.preview_url <- sapply(x$browse, function(xb) xb[["browsePath"]], USE.NAMES = F)
        if(length(x.preview_url) > 1){
          x.preview_url <- list(as.list(x.preview_url))
        }
        if(length(x.preview_url) == 0) x.preview_url <- NA
        # x.browse <- unlist(mapply(xb = x$browse, xn = 1:length(x$browse), function(xb, xn){
        #   xb <- unlist(xb)
        #   names(xb) <- paste0(names(xb), "_", xn)
        #   return(xb)
        # }, SIMPLIFY = F))
        # x.browse.names <- names(x.browse)
        # x.browse <- rbind.data.frame(x.browse, stringsAsFactors = F)
        # colnames(x.browse) <- x.browse.names
        
        
        # assemble df
        if(!is.null(x.metadata)) df <- cbind.data.frame(df, x.metadata, stringsAsFactors = F)
        df$spatialFootprint <- x.spf
        df$product <- ds_name
        df$preview_url <- x.preview_url
        return(df)
      }), SIMPLIFY = F), recursive = F)
      
      ## Read out meta data
      # out("Reading meta data of search results from USGS EarthExplorer...", msg = T)
      # meta <- lapply(sapply(query.df, function(x) x$metadataUrl, USE.NAMES = F), function(x) .get(x))
      # meta.list <- lapply(meta, function(x) as_list(xml_contents(xml_contents(content(x))[1])))
      # meta.val <- lapply(meta.list, function(x) sapply(x, function(y){
      #   z <- try(y$metadataValue[[1]], silent = T)
      #   if(inherits(z, "try-error")) NULL else z
      # }, USE.NAMES = F))
      # meta.name <- lapply(meta.list, function(x) sapply(x, function(y) attributes(y)$name))
      # 
      # ## Define meta fields that are usefull for the query output
      # if(is.null(meta.fields)) meta.fields <- unique(unlist(meta.name))
      # meta.subs <- lapply(meta.name, function(mnames, mf = meta.fields) unlist(lapply(mf, function(x, mn = mnames) which(x == mn))))
      # meta.df <- mapply(FUN = function(v, n, i){
      #   x <- v[i]
      #   x <- lapply(x, function(x) if(is.null(x)) "" else x)
      #   x <- rbind.data.frame(x, stringsAsFactors = F)
      #   colnames(x) <- gsub(" ", "", n[i])
      #   return(x)
      # }, v = meta.val, n = meta.name, i = meta.subs, SIMPLIFY = F)
      # 
      # query.df <- mapply(q = query.df, m = meta.df, FUN = function(q, m){
      #   ## apply meaningful order and replace startTime and endTime with meta outputs
      #   x <- cbind.data.frame(q$acquisitionDate, m, q[,-(1:3)], stringsAsFactors = F)
      #   colnames(x)[1] <- colnames(q)[1]
      #   return(x)
      # }, SIMPLIFY = F)
      
      return.names <- unique(unlist(lapply(query.df, colnames)))
      return.df <- as.data.frame(stats::setNames(replicate(length(return.names), numeric(0), simplify = F), return.names), stringsAsFactors = F)
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
.ESPA_order <- function(req.data, id, level = "sr", username, password, format = "gtiff", verbose){
  
  ## group request by collection (single or multi order)
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
  order <- lapply(req.body, function(x, user = username, pass = password) .post(url = paste0(getOption("gSD.api")$espa, "order/"), username = user, password = pass, body = x))
  order.list <- sapply(order, function(x) content(x)[[1]], USE.NAMES = F)
  out(paste0("Products '", paste0(id, collapse = "', '"), "' have been ordered successfully:"))
  out(paste0("[level = '", paste0(level, collapse = "', "), "', format = '", format, "', order ID(s) '", paste0(order.list, collapse = "', '"), "']."))
  return(order.list)
}
