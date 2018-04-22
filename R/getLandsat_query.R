#' Query Landsat data
#'
#' \code{getLandsat_query} queries the USGS Earth Explorer for Landsat data by some basic input search parameters. The function returns a data frame that can be further filtered.
#'
#' @inheritParams getSentinel_query
#' @param time_range character, containing two elements: the query's starting date and stopping date, formatted "YYYY-MM-DD", e.g. "2017-05-15"
#' @param name character, identifies the name of the dataset to be queried. Use \link{getLandsat_names} to revcieve a vector with all available Landsat datasets from Earth Explorer.
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

getLandsat_query <- function(time_range, name , aoi = NULL, username = NULL, password = NULL){

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

  query <- gSD.get(paste0(getOption("gSD.api")$ee, 'search?jsonRequest={"apiKey":"', api.key,'","datasetName":"', name,'",',spatialFilter,',', temporalFilter, ',"startingNumber":1,"sortOrder":"ASC"}')) #"maxResults":100000000,
  query.cont <- content(query)
  if(query.cont$error != "") out("Invalid query. This dataset seems to be not available for the specified time range.", type = 3)

  query.results <- query.cont$data$results
  if(length(query.results) != 0){

    query.df <- lapply(query.results, function(x){
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
      return(df)
    })
    query.df <- do.call(rbind.data.frame, query.df)

    ## Connect to ESPA to revieve available products for dataset results (no use of entityId, displayId instead)
    avail.products <- as.character(sapply(query.df$displayId, function(x){
      t <- gSD.get(paste0(getOption("gSD.api")$espa, "available-products/", x), getOption("gSD.usgs_user"), getOption("gSD.usgs_pass"))
      paste0("'", paste0(content(t)[[1]]$products,  collapse = "', '"), "'")
    }, USE.NAMES = F))
    query.df <- cbind(query.df, avail.products, stringsAsFactors = FALSE)
    colnames(query.df)[ncol(query.df)] <- "levels_available"
    return(query.df)

  } else { out("No results could be obtained for this request.", msg = T) }
}
