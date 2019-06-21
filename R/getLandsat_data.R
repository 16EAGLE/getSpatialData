#' Download Landsat data
#'
#' \code{getLandsat_data} downloads Landsat data queried using \link{getLandsat_query} from different sources.
#'
#' @inheritParams getLandsat_query
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{getLandsat_query}.
#' @param dir_out character, full path to download target directory. Optional. If not set, \code{getLandsat_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData  archive folder.
#' @param level character, one or multiple product levels to be requested. Defaul is "sr" for surface reflectance. Available levels can be obtained from the "levels_available" field returned for each product by \link{getLandsat_query}.
#' @param source character, either:
#' \itemize{
#'    \item "auto" for automatic selection of data source depending on \code{level}
#'    \item "ESPA" to download on-demand products from USGS-EROS ESPA
#'    \item "AWS" to download from Amazon Webservices (Landsat-8 with \code{level="l1"} only)
#' }
#' @param espa_order character, optional. A vector of a single or multiple ESPA order IDs. Use this argument, if you want to download items being part of an order that already had been placed by this function or yourself earlier, e.g. in case you arboted the function while it was waiting for the order to be completed. The ESPA order ID is displayed when the order is placed and you recieve it via E-Mail from USGS-EROS. If defined, \code{records} is allowed to be undefined.
#' @param force logical. If \code{TRUE}, download is forced even if file already exisits in the download directory. Default is \code{FALSE}.
#' @param n.retry numeric, maximum number of download (re-)attempts. If the downloads of datasets fail (e.g. MD5 checksums do not match), these downloads will be reattampted.
#'
#' @return Character vector of paths to the downloaded files.
#'
#' @note ESPA is used as source if higher-level products are requested by the user (see \code{level}). Since ESPA is an on-demand service, \code{getLandsat_data} places an order and then waits for the requested items to be available, before they are downloaded. Therefore, the runtime of the function is depending on how fast an order is being processed by the ESPA server. The ESPA processing time depends on the size of the order and can take up to 48 hours in highly demanding cases! The function status is indicated by the console messages that it is prompting during execution.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @examples
#'
#' ## Load packages
#' library(getSpatialData)
#' library(sf)
#'
#' ## set aoi and time range for the query
#' set_aoi(aoi_data[[1]])
#' time_range <-  c("2017-08-01", "2017-08-30")
#'
#' ## Login to USGS ERS
#' \dontrun{
#' login_USGS("username")
#'
#' ## set archive directory
#' set_archive("/path/to/archive/")
#'
#' ## get available products and select one
#' product_names <- getLandsat_names()
#'
#' ## query for records for your AOI, time range and product
#' query <- getLandsat_query(time_range = time_range, name = product_names[7])
#'
#' ## preview a record
#' getLandsat_preview(query[5,])
#'
#' #print available levels for a record
#' query[5,]$levels_available
#'
#' ## download record 5 with level "l1" (will direct to AWS automaticaly)
#' files <- getLandsat_data(records = query[5,], level = "l1", source = "auto")
#'
#' ## download record 5 with level "sr" (will be processed on demand by ESPA)
#' files <- getLandsat_data(records = query[5,], level = "sr", source = "auto")
#' # this can take very long, since the function will wait,
#' # until the processing by ESPA is done
#'
#' ## you can abort the function while it is waiting for ESPA and resume later:
#' files <- getLandsat_data(espa_order = "espa-XYZA@host.com-YOUR-ORDER-ID")
#' # the order IDs are displayed and send by mail, use them to resume the task
#' }
#'
#'
#' @importFrom getPass getPass
#' @importFrom tools md5sum
#' @importFrom httr content
#' @importFrom utils head tail
#'
#' @seealso \link{getLandsat_names} \link{getLandsat_query} \link{getLandsat_preview}
#' @export
#'
getLandsat_data <- function(records, level = "sr", source = "auto", dir_out = NULL, espa_order = NULL, force = FALSE, username = NULL, password = NULL, n.retry = 3, verbose = TRUE){

  ## Global USGS login
  if(is.TRUE(getOption("gSD.usgs_set"))){
    if(is.null(username)) username <- getOption("gSD.usgs_user")
    if(is.null(password)) password <- getOption("gSD.usgs_pass")
  }
  if(!is.character(username)) out("Argument 'username' needs to be of level 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)
  if(!is.null(password)) password = password else password = getPass()
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)


  ## Argument checks
  source <- tolower(source)
  level <- tolower(level)
  char_args <- list(level = level, source = source)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)

  if(is.null(espa_order)){
    if(missing(records)) out("Argument 'records' must be defined (except if an ESPA order is given).", type = 3)
    levels <- sapply(strsplit(level, ','), function(x) paste0("'", x, "'"))
    levels <- paste(level, collapse = ".*" )
    for(x in records$levels_available) if(length(grep(levels, x)) == 0) out(paste0("Requested product level '", level, "' is not available for at least one record in 'records'. Check column 'levels_available'."), type = 3)
  }


  ## check/set source (direct EE download needs machine-to-machine privilleges)
  if(!is.null(espa_order)){ source <- "espa"
  } else{
    if(source == "auto"){
      if(all(level == "l1")){
        if(all(sapply(records$displayId, function(x) if(length(grep("LC08", strsplit(x, "_")[[1]][1])) == 0) F else T, USE.NAMES = F))){
          source <- "aws"
        } else out("getLandsat_data currently supports download of Level 1 (level = 'l1') products for Landsat-8 only. Argument 'records' contains records that do not originate from Landsat-8.", type = 3)
      } else source <- "espa"
    } else{
      if(any(level == "l1") & source == "espa") out("Argument 'source' cannot be set to 'espa', if 'level' is set to 'l1'. ESPA is currently providing higher-level on-demand products only.", type = 3)
    }
  }


  ## Redirect, if direct ESPA order download
  if(!is.null(espa_order)){
    prod.id <- as.vector(unlist(sapply(espa_order, function(x, user = username, pass = password){
      sapply(content(gSD.get(paste0(getOption("gSD.api")$espa, "item-status/", x), user, pass))[[1]], function(y) y$name, USE.NAMES = F)
    }, USE.NAMES = F)))
    level <- unique(sapply(espa_order, function(x, user = username, pass = password){
      y <- unlist(content(gSD.get(paste0(getOption("gSD.api")$espa, "order/", x), user, pass))$product_opts)
      y <- y[grep("products", names(y))]
      names(y) <- NULL
      return(y)
    }, USE.NAMES = F))
    #if(length(level) > 1) out(paste0("The provided order IDs refer to orders of different processing levels ['", paste0(level, collapse = "', '"), "']. Please use order IDs of identical levels per call."), type = 3)
  } else{
    records$gSD.prod.id <- records$displayId
  }


  ## Check output directory
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)) dir_out <- paste0(getOption("gSD.archive_get"), "/LANDSAT/", toupper(paste0(level,collapse = "_"))) else dir_out <- path.expand(dir_out)
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  if(!is.character(dir_out)) out(paste0("Argument 'dir_out' needs to be of type 'character'."), type = 3)
  if(!dir.exists(dir_out)) out("The defined archive directory does not exist.", type=3)


  ######## APPPLY CHANGES DONE FOR getSentinel_data and for AWS in here also for ESPA and getMODIS_data (returning records and using gSD.retry)
  
  ## ESPA
  if(source == "espa"){

    ## files
    records$gSD.dir.file <- sapply(records$gSD.prod.id, function(x, d = dir_out, l = paste0(level, collapse = "_")) paste0(d, "/", x, "_LEVEL_", l), USE.NAMES = F)
    catch <- sapply(records$gSD.dir.file, function(x) dir.create(x, showWarnings = F))
    records$gSD.file <- sapply(records$gSD.dir.file, function(x) paste0(x, "/", tail(strsplit(x, "/")[[1]], n=1), ".tar.gz"), USE.NAMES = F)
    
    if(!isTRUE(force)){
      items.exist <- file.exists(file.ds)
      items.skip <- which(items.exist == T)
      items.order <- which(items.exist == F)
      
      catch <- mapply(x = prod.id[items.skip], y = file.ds[items.skip], i.item = items.skip, function(x, y, i.item, n.item = length(items.exist)){
        out(paste0("[", i.item, "/", n.item, "] Skipping download of '", x, "', since '", y, "' already exists..."), msg = T)
      }, SIMPLIFY = F)
      if(is.null(espa_order)) records <- records[items.order,]
      file.down <- file.ds[items.order]
    } else{
      file.down <- file.ds
    }

    if(length(file.down) != 0){
      if(is.null(espa_order)){
        order.list <- .ESPA_order(id = records$displayId, level = level, username = username, password = password, format = "gtiff", verbose = verbose)
      } else{
        order.list <- espa_order
      }
      .ESPA_download(order.list = order.list, username = username, password = password, file.down = file.down, dir_out = dir_out, items.order = items.order, n.item = length(items.exist))
    }
  }


  ## AWS
  if(source == "aws"){
    
    ## get AWS scenes
    out("Accessing AWS download URLs of requested records...")
    file.gz <- tempfile(fileext = ".gz")
    aws.scenes <- gSD.get(getOption("gSD.api")$aws.l8.sl, dir.file = file.gz)
    aws.scenes <- readLines(file.gz) #much faster than read.csv

    out("Collecting AWS download URLs of requested records...")
    records$gSD.url.index <- unlist(mapply(x = records$entityId, d = records$displayId, function(x, d, ds = aws.scenes){
      c.cat <- tail(strsplit(d, "_")[[1]], n=1)
      y <- grep(x, ds, value = T)

      if(length(grep(c.cat, y)) != 0){
        y <- grep(c.cat, y, value = T)[1]
      } else{
        if(length(grep("T2", y)) != 0) y <- grep("T2", y, value = T)
        if(length(grep("T1", y)) != 0) y <- grep("T1", y, value = T)
        y <- y[1]
        out(paste0("Collection category '", c.cat, "' for product '", d, "' is not available on AWS, using lower category '",
                   strsplit(tail(strsplit(y, "_")[[1]], n=1), "/")[[1]][1], "' instead."), msg = T)
      }

      tail(strsplit(y, ",")[[1]], n = 1)
    }, SIMPLIFY = F, USE.NAMES = F))

    ## create URLs
    records$gSD.url.ds <- mapply(x = records$gSD.url.index, y = records$displayId, function(x, y){
      name = paste0(head(strsplit(grep("LC08", strsplit(x, "/")[[1]], value = T), "_")[[1]], n=-4), collapse = "_")
      z <- grep("href", grep(name, unlist(strsplit(strsplit(grep("body", xml_contents(content(gSD.get(x), encoding = "UTF-8")), value = T), ">")[[1]], "<"), recursive = T), value = T), value = T, invert = T)
      z <- unique(c(grep("TIF", z, value = T), grep("IMD", z, value = T), grep("ovr", z, value = T), grep("txt", z, value = T)))
      return(paste0(paste0(head(strsplit(x, "/")[[1]], n=-1), collapse = "/"), "/", z))
    }, SIMPLIFY = F, USE.NAMES = F)

    ## create dir names
    records$gSD.dir.file <- sapply(records$gSD.url.index, function(x, d = dir_out, l = level) paste0(d, "/", tail(strsplit(x, "/")[[1]], n=2)[1], "_", toupper(l)), USE.NAMES = F)
    catch <- sapply(records$gSD.dir.file, function(x) dir.create(x, showWarnings = F))
    
    ## create file names
    records$gSD.file <- mapply(url = records$gSD.url.ds, dir = records$gSD.dir.file, i.item = 1:length(records$gSD.url.ds), FUN = function(url, dir, i.item, n.item = length(records$gSD.url.ds)){
      sapply(url, function(x, d = dir) paste0(d, "/", tail(strsplit(x, "/")[[1]], n=1)), USE.NAMES = F)
    }, SIMPLIFY = F)
    
    ## create console items
    records$gSD.name <- sapply(records$gSD.url.ds, function(u) sapply(u, function(x) tail(strsplit(x, "/")[[1]], n=1), USE.NAMES = F), USE.NAMES = F, simplify = F)
    records$gSD.item <- 1:nrow(records)
    records$gSD.head <- sapply(records$gSD.item, function(i, n = nrow(records)) paste0("[Dataset ", toString(i), "/", toString(n), "] "))
    
    # sep records per files and try to download them
    records <- do.call(rbind, lapply(1:nrow(records), function(i){
      x <- records[i,]
      files <- data.frame(x$gSD.head, x$gSD.name, unlist(x$gSD.url.ds), unlist(x$gSD.file), stringsAsFactors = F)
      colnames(files) <-  c("gSD.head", "gSD.name", "gSD.url.ds", "gSD.file")
      files$gSD.head <- paste0(gsub("] ", "", files$gSD.head), paste0(" | File ", 1:nrow(files), "/", nrow(files), "] "))
      
      files <- gSD.retry(files, gSD.download, names = colnames(files), prog = getOption("gSD.verbose"), force = force, n.retry = n.retry)
      x$gSD.attempts <- max(files$gSD.attempts)
      x$gSD.downloaded <- all(files$gSD.downloaded)
      return(x)
    }))
  }

  ## message the user
  if(any(!records$gSD.downloaded)){
    out(paste0("Some downloads have not been succesfull after ", max(records$gSD.attempts), " attempt(s) (see column 'gSD.downloaded'). Please retry later."), type = 2)
  } else{
    out(paste0("All downloads have been succesfull after ", max(records$gSD.attempts), " attempt(s)."), msg = T)
  }
  
  ## remove internal fields
  records <- records[,-sapply(c("gSD.url.ds", "gSD.name", "gSD.item", "gSD.head", "gSD.url.index"), function(x, names = colnames(records)) which(names == x), USE.NAMES = F)]
  
  out("Columns added to records: 'gSD.prod.id', 'gSD.dir.file', 'gSD.file', 'gSD.downloaded', 'gSD.attempts'")
  return(records)
}
