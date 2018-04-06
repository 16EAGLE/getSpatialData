#' Query Sentinel-1, -2 and -3 data from the Copernicus Open Access Hubs
#'
#' \code{getSentinel_query} queries the Copernicus Open Access Hubs for Sentinel data by some basic input search parameters. The function returns a data frame that can be further filtered.
#'
#' @param time_range character, containing two elements: the query's starting date and stopping date, formatted "YYYY-MM-DD", e.g. "2017-05-15"
#' @param platform character, identifies the platform. Either "Sentinel-1", "Sentinel-2" or "Sentinel-3".
#' @param aoi sfc_POLYGON or SpatialPolygons or matrix, representing a single multi-point (at least three points) polygon of your area-of-interest (AOI). If it is a matrix, it has to have two columns (longitude and latitude) and at least three rows (each row representing one corner coordinate). If its projection is not \code{+proj=longlat +datum=WGS84 +no_defs}, it is reprojected to the latter. Use \link{set_aoi} instead to once define an AOI globally for all queries within the running session. If \code{aoi} is undefined, the AOI that has been set using \link{set_aoi} is used.
#' @param hub_user character, a valid user name to the Copernicus Open Access Hub. Default is NULL. Leave it undefined, if you want to use use \link{set_login_CopHub} to define the login credentials once for all \code{getSentinel*} functions during the session. Register on \url{https://scihub.copernicus.eu/}. Not needed for connections to the pre-operational hub.
#' @param hub_pass character, the password to the specified user account. If \code{NULL}, the password will be taken from \link{set_login_CopHub} inputs or, if \code{set_login_CopHub} is not in use, asked interactively. Not needed for connections to the pre-operational hub.
#' @param hub_access character, either "auto" to access the Copernicus Open Access Hubs by \code{platform} input, "operational" to look for ESA's operational products from the Open Hub,  "pre-ops" to look for pre-operational products from the Pre-Ops Hub (e.g. currently all Sentinel-3 products), or an valid API URL. Default is "auto".
#'
#' @return A data frame; each row represents one dataset, recognized by an individual UUID. The data frame can be further filtered by its columnwise attributes. The UUIDs of the selected datasets can be handed over to the other getSentinel functions for downloading.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom raster extent
#' @importFrom httr GET content
#' @importFrom xml2 xml_contents
#'
#' @examples
#' ## Load packages
#' library(getSpatialData)
#' library(sf)
#' library(sp)
#'
#' ## Define an AOI (either matrix, sf or sp object)
#' data("aoi_data") # example aoi
#'
#' aoi <- aoi_data[[3]] # AOI as matrix object, or better:
#' aoi <- aoi_data[[2]] # AOI as sp object, or:
#' aoi <- aoi_data[[1]] # AOI as sf object
#'
#' ## set AOI for this session
#' set_aoi(aoi)
#' view_aoi() #view AOI in viewer
#'
#' ## Define time range and platform
#' time_range <-  c("2017-08-01", "2017-08-30")
#' platform <- "Sentinel-2"
#'
#' ## set login credentials and an archive directory
#' \dontrun{
#' set_login_CopHub(hub_user = "username") #asks for password or define 'hub_pass'
#' set_archive("/path/to/archive/")
#'
#' ## Use getSentinel_query to search for data (using the session AOI)
#' products <- getSentinel_query(time_range = time_range, platform = platform)
#'
#' ## Get an overview of the products
#' View(products) #get an overview about the search products
#' colnames(products) #see all available filter attributes
#' unique(products$processinglevel) #use one of the, e.g. to see available processing levels
#'
#' ## Filter the products
#' products_filtered <- products[which(products$processinglevel == "Level-1C"),] #filter by Level
#'
#' ## Preview a single product
#' getSentinel_preview(product = products_filtered[5,])
#'
#' ## Download some datasets
#' files <- getSentinel_data(products = products_filtered[c(4,5,6),])
#' }
#'
#' @seealso \link{getSentinel_data}
#' @export

getSentinel_query <- function(time_range, platform, aoi = NULL, hub_user = NULL, hub_pass = NULL,
                              hub_access = "auto"){

  ## Global Copernicus Hub login
  if(is.TRUE(getOption("gSD.cophub_set"))){
    if(is.null(hub_user)) hub_user <- getOption("gSD.cophub_user")
    if(is.null(hub_pass)) hub_pass <- getOption("gSD.cophub_pass")
  }
  if(!is.character(hub_user)) out("Argument 'hub_user' needs to be of type 'character'. You can use 'set_login_CopHub()' to define your login credentials globally.", type=3)
  if(!is.null(hub_pass)){ hub_pass = hub_pass} else{ hub_pass = getPass()}

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

  ## check time_range and platform
  char_args <- list(time_range = time_range, platform = platform)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}else{TRUE}
  }
  if(length(time_range) != 2){out("Argument 'time_range' must contain two elements (start and stop time)", type = 3)}

  ## url assembler function
  cop.url <- function(ext.xy, url.root, platform, time.range, row.start){
    qs <- list(url.root = paste0(url.root, "/"),
               search = c("search?start=", "&rows=100&q="),  #"search?q=", #start=0&rows=1000&
               and = "%20AND%20",
               aoi.poly = c("footprint:%22Intersects(POLYGON((", ")))%22"),
               platformname = "platformname:",
               time = list("[" = "beginposition:%5b", "to" = "%20TO%20", "]" = "%5d"))
    aoi.str <- paste0(apply(ext.xy, MARGIN = 1, function(x) paste0(x, collapse = "%20")), collapse = ",")
    time.range <- sapply(time.range, function(x) paste0(x, "T00:00:00.000Z"), USE.NAMES = F)
    return(paste0(qs$url.root, qs$search[1], toString(row.start), qs$search[2], "(", qs$aoi.poly[1], aoi.str, qs$aoi.poly[2], qs$and,
                  qs$platformname, platform, qs$and,
                  qs$time$`[`, time.range[1], qs$time$to, time.range[2], qs$time$`]`, ")"))
  }

  ## Manage API access
  cred <- access_API(hub_access, platform, hub_user, hub_pass)

  ## query API
  row.start <- -100; re.query <- T; give.return <- T
  query.list <- list()

  while(is.TRUE(re.query)){
    row.start <- row.start + 100

    query <- gSD.get(cop.url(ext.xy = aoi, url.root = cred[3], platform = platform, time.range = time_range, row.start = row.start), cred[1], cred[2])
    query.xml <- suppressMessages(xml_contents(content(query)))
    query.list <- c(query.list, lapply(query.xml[grep("entry", query.xml)], function(x) xml_contents(x)))

    if(length(query.list) == 0 & row.start == 0){
      out("No results could be obtained for this request.", msg = T)
      re.query <- F
      give.return <- F
    }
    if(length(query.list) != 100) re.query <- F
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

    ## assemble products data.frame
    pd.names <- unique(unlist(query.names))
    pd.frame <- as.data.frame(stats::setNames(replicate(length(pd.names),numeric(0), simplify = F), pd.names))
    for( i in 1:length(query.fields)){
      subs <- match(names(query.fields[[i]]), pd.names)
      pd.frame[i,subs] <- sapply(query.fields[[i]], as.character)
    }
  }

  if(is.TRUE(give.return)) return(pd.frame)
}
