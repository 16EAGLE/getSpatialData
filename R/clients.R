#' query ESA Copernicus Open Access Hub
#'
#' @importFrom getPass getPass
#' @importFrom httr GET content
#' @importFrom xml2 xml_contents as_xml_document
#' @importFrom sf st_as_sfc st_sf
#' 
#' @return records
#' @keywords internal
#' @noRd
#' 
.records_CopHub <- function(time_range, product_name, aoi = NULL, rename_cols = TRUE, ...){
  
  ## check ... extras
  extras <- list(...)
  username <- extras$username
  password <- extras$password
  if(!is.null(extras$hub)) hub <- extras$hub else hub <- "auto"
  if(is.null(username)) .check_login("Copernicus")
  cred <- .check_credentials(username, password, service = "Copernicus")
  
  # GNSS products
  gnss <- grepl("GNSS", product_name)
  #if(!is.null(extras$gnss)) gnss <- extras$gnss else gnss <- FALSE
  out(paste0("Searching records for product name '", product_name, "'..."))
  
  ## Global AOI
  if(!isTRUE(gnss)){
    aoi <- st_as_sfc(st_bbox(.check_aoi(aoi, type = "sf"))) # create bounding box instead of checking npts
    aoi <- .check_aoi(aoi, type = "matrix")
  } else{
    out("AOI is ignored, since the requested product is of type GNSS. GNSS records are AOI-independent, as they are only referenced by mission time using argument time_range.", type = 2)
    aoi <- NULL
    product_name_orig <- product_name
    product_name <- gsub("_GNSS", "", product_name)
  }
  
  ## url assembler function
  cop.url <- function(ext.xy, url.root, product_name, time.range, row.start){
    if(product_name == "sentinel-5p") product_name <- "sentinel-5"
    qs <- list(url.root = paste0(url.root, "/"),
               search = c("search?start=", "&rows=100&q="),  #"search?q=", #start=0&rows=1000&
               and = "%20AND%20",
               aoi.poly = c("footprint:%22Intersects(POLYGON((", ")))%22"),
               platformname = "platformname:",
               time = list("[" = "beginposition:%5b", "to" = "%20TO%20", "]" = "%5d"))
    time.range <- .sapply(time.range, function(x) paste0(x, "T00:00:00.000Z"), USE.NAMES = F)
    if(!is.null(ext.xy)) aoi.str <- paste0(.apply(ext.xy, MARGIN = 1, function(x) paste0(x, collapse = "%20")), collapse = ",")
    
    return(paste0(qs$url.root, qs$search[1], toString(row.start), qs$search[2], "(", if(!is.null(ext.xy)) paste0(qs$aoi.poly[1], aoi.str, qs$aoi.poly[2], qs$and) else "",
                  qs$platformname, product_name, qs$and,
                  qs$time$`[`, time.range[1], qs$time$to, time.range[2], qs$time$`]`, ")"))
  }
  
  ## Manage API access
  cred <- .CopHub_select(x = hub, p = if(isTRUE(gnss)) "GNSS" else product_name, user = cred$username, pw = cred$password)
  
  ## query API
  row.start <- -100; re.query <- T; give.return <- T
  query.list <- list()
  
  while(is.TRUE(re.query)){
    row.start <- row.start + 100
    
    query <- .get(url = cop.url(ext.xy = aoi, url.root = cred[3], product_name = product_name, time.range = time_range, row.start = row.start), username = cred[1], password = cred[2])
    query.xml <- suppressMessages(xml_contents(as_xml_document(content(query, as = "text"))))
    query.list <- c(query.list, .lapply(query.xml[grep("entry", query.xml)], function(x) xml_contents(x)))
    
    if(length(query.list) == 0 & row.start == 0){
      out("No results could be obtained for this product, time range and AOI.", msg = T)
      re.query <- F
      give.return <- F
    }
    if(length(query.list) != row.start+100 ) re.query <- F
    #if(length(query.list) != 100) re.query <- F
  }
  
  ## build query result data frame
  if(is.TRUE(give.return)){
    
    ## predefine tags not having 'name' tag
    field.tag <- c("title", "link href", 'link rel="alternative"', 'link rel="icon"', "summary", "name")
    field.names <- c("title", "url", "url.alt", "url.icon", "summary")
    
    ## get field.tag contents and assemble unique names vector
    query.cont <- .lapply(query.list, function(x, field = field.tag) unlist(.lapply(field, function(f, y = x) grep(f, y, value = T))))
    query.names <- .lapply(query.cont, function(x, field.n = field.names) c(field.n, .sapply(x[(length(field.n)+1):length(x)], function(y) strsplit(y, '\"')[[1]][2], USE.NAMES = F)))
    
    ## make fields and treat url tags differently, as required field data is no content here
    query.fields <- .lapply(query.cont, function(x) .sapply(x, function(y) strsplit(strsplit(y, ">")[[1]][2], "<")[[1]][1], USE.NAMES = F))
    query.fields <- .lapply(1:length(query.fields), function(i, qf = query.fields, qc = query.cont, qn = query.names){
      x <- qf[[i]]
      x[which(is.na(x) == T)] <- .sapply(qc[[i]][which(is.na(x) == T)], function(y) strsplit(strsplit(y, 'href=\"')[[1]][2], '\"/>')[[1]][1], USE.NAMES = F)
      names(x) <- qn[[i]]
      return(x)
    })
    
    records.names <- unique(unlist(query.names))
    records <- as.data.frame(stats::setNames(replicate(length(records.names),numeric(0), simplify = F), records.names))
    records <- do.call(rbind.data.frame, .lapply(query.fields, function(x, rn = records.names,  rdf = records){
      rdf[1, match(names(x), rn)] <- .sapply(x, as.character)
      return(rdf)
    }))
    
    # convert expected numeric fields
    fields.numeric <- names(records)[.sapply(names(records), function(x, y = c("orbitnumber", "relativeorbitnumber", "cloudcoverpercentage", "highprobacloudspercentage", "mediumprobacloudspercentage",
                                                                              "snowicepercentage", "vegetationpercentage", "waterpercentage", "baresoilpercentage", "lowprobacloudspercentage")) x %in% y, USE.NAMES = F)]
    records[,fields.numeric] <- .sapply(fields.numeric, function(x) as.numeric(records[,x]))
    
    # translate record column names
    if(isTRUE(rename_cols)) records <- .translate_records(records, product_name)
    records$is_gnss <- gnss
    
    # remove non-functional preview url if records are GNSS
    if(gnss){
      records$preview_url <- NA
      records$product <- product_name_orig
      #records$md5_url <- NULL
    }
    
    if(is.null(records$level)){
      records$level <- NA
    }
    
    if(product_name == "sentinel-5p"){
      records$preview_url <- NA
    }
    
    if(!is.null(records$footprint)) records$footprint <- st_as_sfc(records$footprint, crs = 4326)
    return(records)
  }
}

#' query USGS Earth Explorer
#'
#' @importFrom getPass getPass
#' @importFrom httr content
#' @importFrom sf st_as_sfc st_sf
#' 
#' @return records
#' @keywords internal
#' @noRd
#' 
.records_EE <- function(time_range, product_name, aoi = NULL, rename_cols = TRUE, ...){
  
  ## check ... extras
  extras <- list(...)
  if(is.null(extras$maxCloudScene)) extras$maxCloudScene <- 100
  if(is.null(extras$maxCloudLand)) extras$maxCloudLand <- 100
  username <- extras$username
  password <- extras$password
  if(is.null(username)) .check_login("USGS")
  
  cred <- .check_credentials(username, password, service = "USGS")
  
  # check aoi
  aoi <- .check_aoi(aoi, type = "sf", quiet = T)
  
  if(grepl("landsat", product_name)){
    meta.fields <- c("Start Time", "Stop Time", "WRS Path", "WRS Row", "Land Cloud Cover", "Scene Cloud Cover",
      "Sun Elevation", "Sun Azimuth", "Sensor Identifier", "Image Quality")
  }
  if(grepl("modis", product_name)){
    meta.fields <- c("Acquisition Start Date", "Acquisition End Date", "Horizontal Tile Number", "Vertical Tile Number",
                     "Auto Quality Flag", "Auto Quality Flag Explanation", "Science Quality Flag",
                     "Science Quality Flag Expln","Missing Data Percentage")
  }
  
  ## query
  records <- .EE_query(aoi, time_range, product_name, cred$api.key, meta.fields)
  
  if(is.null(records)){
    out("No results could be obtained for this product, time range and AOI.", msg = T)
  }else{
    
    if(grepl("landsat", product_name)){
      ## Connect to ESPA to retrieve available products for (no use of entityId, displayId instead)
      out("Recieving available product levels from USGS-EROS ESPA...", msg = T)
      records <- do.call(rbind.data.frame, .apply(records, MARGIN = 1, function(x, names = colnames(records)){
        
        x <- rbind.data.frame(x, stringsAsFactors = F)
        colnames(x) <- names
        
        response <- try(.get(url = paste0(getOption("gSD.api")$espa, "available-products/", x$displayId), username = cred$username, password = cred$password), silent = T)
        if(inherits(response, "try-error")) response <- "l1" else{
          response <- if(all(names(content(response)) == "not_implemented")) "'l1'" else unlist(content(response)[[1]]$products) #paste0("'", paste0(content(t)[[1]]$products,  collapse = "', '"), "'")
        }
        
        x <- do.call(rbind.data.frame, rep(list(x), length(response)))
        x$level <- response
        return(x)
      }))
      
      ## Correct WRS fields
      wrs.sub <- grep("WRS", colnames(records))
      records[,wrs.sub] <- .apply(records[,wrs.sub], MARGIN = 2, function(x) .sapply(x, function(y){
        z <- strsplit(y, " ")[[1]]
        z[length(z)]
      }, USE.NAMES = F))
      
      # convert expected numeric fields
      fields.numeric <- names(records)[.sapply(names(records), function(x, y = c("WRSPath", "WRSRow", "LandCloudCover", "SceneCloudCover", "ImageQuality")) x %in% y, USE.NAMES = F)]
    }
    
    if(grepl("modis", product_name)){
      fields.numeric <- names(records)[.sapply(names(records), function(x, y = c("Horizontal.Tile.Number", "Vertical.Tile.Number", "Missing.Data.Percentage")) x %in% y, USE.NAMES = F)]
    }
    
    # convert fields
    records[,fields.numeric] <- .sapply(fields.numeric, function(x) as.numeric(records[,x]))
    
    if(is.null(records$level)){
      records$level <- NA
    }
    
    # cloud cover filter
    if(extras$maxCloudLand < 100 ) records <- records[records$LandCloudCover <= extras$maxCloudLand,]
    if(extras$maxCloudScene < 100 ) records <- records[records$SceneCloudCover <= extras$maxCloudScene,]
    
    colnames(records)[colnames(records) == "spatialFootprint"] <- "footprint"
    records$footprint <- st_as_sfc(records$footprint, crs = 4326)
    
    # translate record column names
    if(isTRUE(rename_cols)) records <- .translate_records(records, product_name)
    
    return(records)
  }
}


#' query NASA CMR EarthData API
#'
#' @importFrom getPass getPass
#' @importFrom httr content
#' @importFrom sf st_as_sfc st_sf st_centroid st_coordinates st_cast
#' @importFrom lwgeom st_geod_azimuth
#' 
#' @return records
#' @keywords internal
#' @noRd
.records_CMR <- function(time_range, product_name, aoi = NULL, rename_cols = TRUE, ...){
  
  # NASA CMR earth data url
  url <- "https://cmr.earthdata.nasa.gov/search/" # move to options
  
  # get collection from concept id
  id <- .getCMR_id(product_name)
  out(paste0("Searching records for product name '", product_name, "'..."))
  if(grepl("SRTM", product_name)) out("'time_range' is ignored, since the requested product is monotemporal.", type = 2)
  #url.query <- paste0(url, "collections?concept_id=", id)
  #response <- content(GET(url.query))
  #response <- response$feed$entry
  
  # check aoi
  aoi <- st_as_sfc(st_bbox(.check_aoi(aoi, type = "sf", quiet = T)))
  
  # sort coordinates counter-clockwise
  aoi.points <- st_cast(aoi, "POINT")
  aoi.points <- aoi.points[!duplicated(aoi.points)]
  azimuth <- sapply(1:length(aoi.points), function(i, center = quiet(st_centroid(aoi))) suppressPackageStartupMessages(st_geod_azimuth(c(aoi.points[i], center))))
  aoi.points <- aoi.points[rev(order(azimuth))]
  aoi.matrix <- st_coordinates(aoi.points[rev(order(azimuth))])
  
  #aoi.matrix <- .check_aoi(aoi, type = "matrix", quiet = T)
  #aoi.matrix <- aoi.matrix[!duplicated(aoi.matrix),]
  
  # sort coordinates counter-clockwise
  #aoi.center <- quiet(st_centroid(aoi))
  # cc.angles <- geosphere::bearing(aoi.matrix, aoi.center[[1]][1:2])
  # aoi.matrix <- aoi.matrix[rev(order(cc.angles)),]
  
  # start with most western coordinate
  cc.first <- which.min(aoi.matrix[,1])
  if(cc.first > 1){
    aoi.matrix <- aoi.matrix[c(cc.first:nrow(aoi.matrix), 1:(cc.first-1)),]
  }
  aoi.matrix <- rbind(aoi.matrix, aoi.matrix[1,])
  
  # assemble json query string and add further query keywords
  query.json <- paste0("polygon=",
                       paste0(.apply(aoi.matrix, MARGIN=1, function(x) paste0(x, collapse = "%2C")), collapse = "%2C"))
  query.product <- paste0("echo_collection_id=", id)
  query.sort <- "sort_key%5B%5D=-start_date&page_size=20"
  query.url <- paste0(url, "granules.json?", query.json, "&", query.product, "&", query.sort)
  
  # query API for defined product
  response <- content(GET(query.url))
  response <- response$feed$entry
  
  # build a records data.frame containing all returned records
  fields <- names(response[[1]])
  records <- do.call(rbind, .lapply(response, function(x){
    links <- grep("http", unlist(x$links), value = T)
    names(links) <- NULL
    
    y <- data.frame(x[fields != "links"], stringsAsFactors = F)
    y$links <- list(links)
    colnames(y) <- fields
    
    #y$dataset_url <- links[grep("hgt.zip$", links)]
    y$preview_url <- links[grep(".jpg$", links)]
    return(y)
  }))
  
  if(isTRUE(rename_cols)) records <- .translate_records(records, product_name)
  
  # convert coordinates to sf
  if(!is.null(records$footprint)){
    records$footprint <- st_as_sfc(lapply(records$footprint, function(x){
      y <- as.numeric(strsplit(x, " ")[[1]])
      y <- data.frame(x = y[c(2, 4, 4, 2, 2)], y = y[c(1, 1, 3, 3, 1)])
      st_polygon(list(as.matrix(y)))
    }), crs = 4326)
  }
  
  return(records)
}

