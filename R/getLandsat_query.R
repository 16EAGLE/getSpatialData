#' Query Landsat data
#'
#' \code{getLandsat_query} queries the USGS Earth Explorer for Landsat data by some basic input search parameters. The function returns a data frame that can be further filtered.
#'
#' @inheritParams getSentinel_query
#' @param time_range character, containing two elements: the query's starting date and stopping date, formatted "YYYY-MM-DD", e.g. "2017-05-15"
#' @param name character, optional. Identifies the name of the dataset to be queried. If set to "all" (default), every available Landsat dataset is searched for results and included in the output. Use \link{getLandsat_names} to revcieve a vector with all available Landsat datasets from Earth Explorer, if you want to select a specific one.
#' @param username character, a valid USGS user name. Default is NULL. Leave it undefined, if you want to use use \link{login_USGS} to define the login credentials once for all \code{get*} functions that connect to USGS services during the session. Register on \url{https://ers.cr.usgs.gov/register/}.
#' @param password character, the password to the specified user account. If \code{NULL}, the password will be taken from \link{login_USGS} inputs or, if \code{login_USGS} is not in use, asked interactively.
#'
#' @return A data frame; each row represents one dataset. The data frame can be further filtered by its columnwise attributes. The selected rows can be handed over to the other getLandsat functions for previeqing or downloading.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom httr content
#'
#' @seealso \link{getLandsat_names} \link{getLandsat_preview} \link{getLandsat_data}
#' @export

getLandsat_query <- function(time_range, name = "all" , aoi = NULL, username = NULL, password = NULL){

  ## Global USGS login
  if(is.null(username)){
    if(is.TRUE(getOption("gSD.usgs_set"))){
      username <- getOption("gSD.usgs_user")
      password <- getOption("gSD.usgs_pass")
      api.key <- getOption("gSD.usgs_apikey")
    } else {
      out("Argument 'username' needs to be of type 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)
    }
  } else{
    if(is.null(password)) password = getPass()
    api.key <- usgs_login(username, password)
  }

  ## Global AOI
  if(is.null(aoi)){
    if(is.TRUE(getOption("gSD.aoi_set"))){
      aoi <- getOption("gSD.aoi")
    } else{
      out("Argument 'aoi' is undefined and no session AOI could be obtained. Define aoi or use set_aoi() to define a session AOI.", type = 3)
    }
  } else{
    aoi <- make_aoi(aoi, type = "matrix")
  }
  aoi <- make_aoi(aoi, type = "sf", quiet = T)

  ## check time_range and name
  char_args <- list(time_range = time_range, name = name)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(length(time_range) != 2) out("Argument 'time_range' must contain two elements (start and stop time).", type = 3)
  if(length(name) != 1) out("Argument 'name' must contain a single element of type character.", type = 3)

  spatialFilter <- paste0('"spatialFilter":{"filterType":"mbr","lowerLeft":{"latitude":', st_bbox(aoi)$ymin, ',"longitude":', st_bbox(aoi)$xmin, '},"upperRight":{"latitude":', st_bbox(aoi)$ymax, ',"longitude":', st_bbox(aoi)$xmin, '}}')
  temporalFilter <- paste0('"temporalFilter":{"startDate":"', time_range[1], '","endDate":"', time_range[2], '"}')

  out("[1/3] Querying USGS Earth Explorer for available products...")
  if(name == "all") name <- getLandsat_names()
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
      spf.sf <- make_aoi(cbind(spf[seq(1, length(spf), by = 2)], spf[seq(2, length(spf), by = 2)]), type = "sf", quiet = T)

      df <- rbind.data.frame(x.char, stringsAsFactors = F)
      colnames(df) <- x.names
      df[,spf.sub] <- st_as_text(spf.sf)
      df <- cbind.data.frame(df, ds_name, stringsAsFactors = F)
      colnames(df)[ncol(df)] <- "dataset_name"
      return(df)
    }), SIMPLIFY = F), recursive = F)

    ## Read out meta data
    out("[2/3] Reading out additional meta data...")
    meta <- lapply(sapply(query.df, function(x) x$metadataUrl, USE.NAMES = F), function(x) gSD.get(x))
    meta.list <- lapply(meta, function(x) as_list(xml_contents(xml_contents(content(x))[1])))
    meta.val <- lapply(meta.list, function(x) sapply(x, function(y){
      z <- try(y$metadataValue[[1]], silent = T)
      if(inherits(z, "try-error")) NULL else z
    }, USE.NAMES = F))
    meta.name <- lapply(meta.list, function(x) sapply(x, function(y) attributes(y)$name))

    ## Define meta fields that are usefull for the query output
    meta.fields <- c("Start Time", "Stop Time", "WRS Path", "WRS Row", "Land Cloud Cover", "Scene Cloud Cover",
                     "Sun Elevation", "Sun Azimuth", "Sensor Identifier", "Image Quality")
    meta.subs <- lapply(meta.name, function(mnames, mf = meta.fields) unlist(lapply(mf, function(x, mn = mnames) grep(x, mn))))
    meta.df <- mapply(FUN = function(v, n, i){
      x <- v[i]
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

    ## Connect to ESPA to revieve available products for dataset results (no use of entityId, displayId instead)
    out("[3/3] Recieving available product levels from USGS-EROS ESPA...")
    avail.products <- as.character(sapply(return.df$displayId, function(x){
      t <- gSD.get(paste0(getOption("gSD.api")$espa, "available-products/", x), getOption("gSD.usgs_user"), getOption("gSD.usgs_pass"))
      paste0("'", paste0(content(t)[[1]]$products,  collapse = "', '"), "'")
    }, USE.NAMES = F))
    return.df <- cbind(return.df, avail.products, stringsAsFactors = FALSE)
    colnames(return.df)[ncol(return.df)] <- "levels_available"
    return(return.df)

  } else { out("No results could be obtained for this request.", msg = T) }
}
