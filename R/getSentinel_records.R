#' @rdname get_records
#' 
#' @importFrom getPass getPass
#' @importFrom httr GET content
#' @importFrom xml2 xml_contents as_xml_document
#' @importFrom sf st_as_sfc st_sf
#' 
#' @export

getSentinel_records <- function(time_range, name, aoi = NULL, as_sf = TRUE, rename_cols = TRUE, hub = "auto", check_avail = FALSE, gnss = FALSE, ..., verbose = TRUE){

  ## check ... extras
  extras <- list(...)
  if(!is.null(extras$platform)) name <- extras$platform # support for deprecated argument name
  if(!is.null(extras$username)) username <- extras$username else username <- NULL
  if(!is.null(extras$password)) password <- extras$password else password <- NULL
  
  ## Global Copernicus Hub login
  if(is.TRUE(getOption("gSD.dhus_set"))){
    if(is.null(username)) username <- getOption("gSD.dhus_user")
    if(is.null(password)) password <- getOption("gSD.dhus_pass")
  }
  if(!is.character(username)) out("Argument 'username' needs to be of type 'character'. You can use 'login_CopHub()' to define your login credentials globally.", type=3)
  if(!is.null(password)){ password = password} else{ password = getPass()}
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)

  ## Global AOI
  if(!isTRUE(gnss)){
    if(is.null(aoi)){
      if(is.TRUE(getOption("gSD.aoi_set"))){
        aoi <- getOption("gSD.aoi")
      } else{
        out("Argument 'aoi' is undefined and no session AOI could be obtained. Define aoi or use set_aoi() to define a session AOI.", type = 3)
      }
    }
    aoi <- st_as_sfc(st_bbox(.make_aoi(aoi, type = "sf"))) # create bounding box instead of checking npts
    aoi <- .make_aoi(aoi, type = "matrix")
  } else{
    aoi <- NULL
  }
  
  ## check time_range and platform
  char_args <- list(time_range = time_range, name = name)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(length(time_range) != 2){out("Argument 'time_range' must contain two elements (start and stop time).", type = 3)}
  if(!name %in% getSentinel_names()) out(paste0("The selected product name is not supported. Select either ", paste0("'", paste0(getSentinel_names(), collapse = "', '"), "'")), type = 3)
  
  ## url assembler function
  cop.url <- function(ext.xy, url.root, name, time.range, row.start){
    if(name == "Sentinel-5P") name <- "Sentinel-5"
    qs <- list(url.root = paste0(url.root, "/"),
               search = c("search?start=", "&rows=100&q="),  #"search?q=", #start=0&rows=1000&
               and = "%20AND%20",
               aoi.poly = c("footprint:%22Intersects(POLYGON((", ")))%22"),
               platformname = "platformname:",
               time = list("[" = "beginposition:%5b", "to" = "%20TO%20", "]" = "%5d"))
    time.range <- sapply(time.range, function(x) paste0(x, "T00:00:00.000Z"), USE.NAMES = F)
    if(!is.null(ext.xy)) aoi.str <- paste0(apply(ext.xy, MARGIN = 1, function(x) paste0(x, collapse = "%20")), collapse = ",")
    
    return(paste0(qs$url.root, qs$search[1], toString(row.start), qs$search[2], "(", if(!is.null(ext.xy)) paste0(qs$aoi.poly[1], aoi.str, qs$aoi.poly[2], qs$and) else "",
                  qs$platformname, name, qs$and,
                  qs$time$`[`, time.range[1], qs$time$to, time.range[2], qs$time$`]`, ")"))
  }

  ## Manage API access
  cred <- .CopHub_select(x = hub, p = if(isTRUE(gnss)) "GNSS" else name, user = username, pw = password)

  ## query API
  row.start <- -100; re.query <- T; give.return <- T
  query.list <- list()
  
  out(paste0("Searching records for product name '", name, "'..."))
  while(is.TRUE(re.query)){
    row.start <- row.start + 100

    query <- gSD.get(url = cop.url(ext.xy = aoi, url.root = cred[3], name = name, time.range = time_range, row.start = row.start), username = cred[1], password = cred[2])
    query.xml <- suppressMessages(xml_contents(as_xml_document(content(query, as = "text"))))
    query.list <- c(query.list, lapply(query.xml[grep("entry", query.xml)], function(x) xml_contents(x)))

    if(length(query.list) == 0 & row.start == 0){
      out("No results could be obtained for this request.", msg = T)
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
    query.cont <- lapply(query.list, function(x, field = field.tag) unlist(lapply(field, function(f, y = x) grep(f, y, value = T))))
    query.names <- lapply(query.cont, function(x, field.n = field.names) c(field.n, sapply(x[(length(field.n)+1):length(x)], function(y) strsplit(y, '\"')[[1]][2], USE.NAMES = F)))

    ## make fields and treat url tags differently, as required field data is no content here
    query.fields <- lapply(query.cont, function(x) sapply(x, function(y) strsplit(strsplit(y, ">")[[1]][2], "<")[[1]][1], USE.NAMES = F))
    query.fields <- lapply(1:length(query.fields), function(i, qf = query.fields, qc = query.cont, qn = query.names){
      x <- qf[[i]]
      x[which(is.na(x) == T)] <- sapply(qc[[i]][which(is.na(x) == T)], function(y) strsplit(strsplit(y, 'href=\"')[[1]][2], '\"/>')[[1]][1], USE.NAMES = F)
      names(x) <- qn[[i]]
      return(x)
    })

    records.names <- unique(unlist(query.names))
    records <- as.data.frame(stats::setNames(replicate(length(records.names),numeric(0), simplify = F), records.names))
    records <- do.call(rbind.data.frame, lapply(query.fields, function(x, rn = records.names,  rdf = records){
      rdf[1, match(names(x), rn)] <- sapply(x, as.character)
      return(rdf)
    }))
    if(isTRUE(check_avail)) records$download_available <- as.logical(toupper(unlist(.get_odata(records$uuid, cred, field = "Online/$value"))))
    
    # convert expected numeric fields
    fields.numeric <- names(records)[sapply(names(records), function(x, y = c("orbitnumber", "relativeorbitnumber", "cloudcoverpercentage", "highprobacloudspercentage", "mediumprobacloudspercentage",
                                                                              "snowicepercentage", "vegetationpercentage", "waterpercentage", "baresoilpercentage", "lowprobacloudspercentage")) x %in% y, USE.NAMES = F)]
    records[,fields.numeric] <- sapply(fields.numeric, function(x) as.numeric(records[,x]))
    
    # translate record column names
    records <- if(isTRUE(rename_cols)) .translate_records(records, name)
    records$is_gnss <- gnss
    
    # sf geometry
    records$footprint <- st_as_sfc(records$footprint, crs = 4326)
    if(isTRUE(as_sf)) records <- st_sf(records, sfc_last = F)
    
    return(records)
  }
}

#' @rdname getSpatialData-deprecated
#' @export
getSentinel_query <- function(...){
  .Deprecated("getSentinel_records", "getSpatialData")
  getSentinel_records(...)
}
