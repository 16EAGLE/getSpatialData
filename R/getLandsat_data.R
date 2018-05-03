#' Download Landsat data
#'
#' \code{getLandsat_data} downloads Landsat data queried using \link{getLandsat_query} from different sources.
#'
#' @inheritParams getLandsat_query
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{getLandsat_query}.
#' @param dir_out character, full path to download target directory. Optional. If not set, \code{getLandsat_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData  archive folder.
#' @param level character, the requested product level. Defaul is "sr" for surface reflectance. Available levels can be obtained from the "levels_available" field returned for each product by \link{getLandsat_query}.
#' @param source character, either:
#' \itemize{
#'    \item "auto" for automatic selection of data source depending on \code{level}
#'    \item "ESPA" to download on-demand products from USGS-EROS ESPA
#'    \item "AWS" to download from Amazon Webservices (Landsat-8 with \code{level="l1"} only)
#' }
#' @param espa_order character, optional. A vector of a single or multiple ESPA order IDs. Use this argument, if you want to download items being part of an order that already had been placed by this function or yourself earlier, e.g. in case you arboted the function while it was waiting for the order to be completed. The ESPA order ID is displayed when the order is placed and you recieve it via E-Mail from USGS-EROS. If defined, \code{records} is allowed to be undefined.
#' @param force logical. If \code{TRUE}, download is forced even if file already exisits in the download directory. Default is \code{FALSE}.
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
getLandsat_data <- function(records, level = "sr", source = "auto", dir_out = NULL, espa_order = NULL, force = FALSE, username = NULL, password = NULL, verbose = TRUE){

  ## Global USGS login
  if(is.TRUE(getOption("gSD.usgs_set"))){
    if(is.null(username)){username <- getOption("gSD.usgs_user")}
    if(is.null(password)){password <- getOption("gSD.usgs_pass")}
  }
  if(!is.character(username)){out("Argument 'username' needs to be of level 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)}
  if(!is.null(password)){password = password}else{password = getPass()}
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)


  ## Argument checks
  source <- tolower(source)
  level <- tolower(level)
  char_args <- list(level = level, source = source)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)

  if(is.null(espa_order)){
    if(missing(records)) out("Argument 'records' must be defined (except if an ESPA order is given).", type = 3)
    for(x in records$levels_available) if(length(grep(level, x)) == 0) out(paste0("Requested product level '", level, "' is not available for at least one record in 'records'. Check column 'levels_available'."), type = 3)
  }


  ## check/set source (direct EE download needs machine-to-machine privilleges)
  if(!is.null(espa_order)){ source <- "espa"
  } else{
    if(source == "auto"){
      if(level == "l1"){
        if(all(sapply(records$displayId, function(x) if(length(grep("LC08", strsplit(x, "_")[[1]][1])) == 0) F else T, USE.NAMES = F))){
          source <- "aws"
        } else out("getLandsat_data currently supports download of Level 1 (level = 'l1') products for Landsat-8 only. Argument 'records' contains records that do not originate from Landsat-8.", type = 3)
      } else source <- "espa"
    } else{
      if(level == "l1" & source == "espa") out("Argument 'source' cannot be set to 'espa', if 'level' is set to 'l1'. ESPA is currently providing higher-level on-demand products only.", type = 3)
    }
  }


  ## Redirect, if direct ESPA order download
  if(!is.null(espa_order)){
    prod.id <- sapply(espa_order, function(x, user = username, pass = password){
      content(gSD.get(paste0(getOption("gSD.api")$espa, "item-status/", x), user, pass))[[1]][[1]]$name
    }, USE.NAMES = F)
    level <- unique(sapply(espa_order, function(x, user = username, pass = password){
      y <- unlist(content(gSD.get(paste0(getOption("gSD.api")$espa, "order/", x), user, pass))$product_opts)
      y <- y[grep("products", names(y))]
      names(y) <- NULL
      return(y)
    }, USE.NAMES = F))
    if(length(level) > 1) out(paste0("The provided order IDs refer to orders of different processing levels ['", paste0(level, collapse = "', '"), "']. Please use order IDs of identical levels per call."), type = 3)
  } else{
    prod.id <- records$displayId
  }


  ## Check output directory
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)){dir_out <- paste0(getOption("gSD.archive"), "/LANDSAT/", toupper(level), "/")}
    if(!dir.exists(dir_out)) dir.create(dir_out)
  }


  ## ESPA
  if(source == "espa"){

    ## files
    dir.ds <- sapply(prod.id, function(x, d = dir_out, l = level) paste0(d, "/", x, "_", toupper(l)), USE.NAMES = F)
    catch <- sapply(dir.ds, function(x) dir.create(x, showWarnings = F))
    file.ds <- sapply(dir.ds, function(x) paste0(x, "/", tail(strsplit(x, "/")[[1]], n=1), ".tar.gz"), USE.NAMES = F)

    if(!isTRUE(force)){
      sub.avoid <- which(file.exists(file.ds) == T)
      if(is.null(espa_order)) records <- records[!file.exists(file.ds),]
      file.down <- file.ds[!file.exists(file.ds)]
    }

    if(length(file.down) != 0){
      if(is.null(espa_order)){
        order.list <- .ESPA_order(id = records$displayId, level = level, username = username, password = password, format = "gtiff", verbose = verbose)
      } else{
        order.list <- espa_order
      }
      .ESPA_download(order.list = order.list, username = username, password = password, file.down = file.down, dir_out = dir_out)
    }
  }


  ## AWS
  if(source == "aws"){

    out("Checking availability of requested records on AWS...")
    file.gz <- tempfile(fileext = ".gz")
    aws.scenes <- gSD.get(getOption("gSD.api")$aws.l8.sl, dir.file = file.gz)
    aws.scenes <- readLines(file.gz) #much faster than read.csv

    out("Recieving download AWS URLs of requested records...")
    url.index <- mapply(x = records$entityId, d = records$displayId, function(x, d, ds = aws.scenes){
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
    }, SIMPLIFY = F)

    url.files <- mapply(x = url.index, y = records$displayId, function(x, y){
      name = paste0(head(strsplit(grep("LC08", strsplit(x, "/")[[1]], value = T), "_")[[1]], n=-4), collapse = "_")
      z <- grep("href", grep(name, unlist(strsplit(strsplit(grep("body", xml_contents(content(gSD.get(x), encoding = "UTF-8")), value = T), ">")[[1]], "<"), recursive = T), value = T), value = T, invert = T)
      z <- unique(c(grep("TIF", z, value = T), grep("IMD", z, value = T), grep("ovr", z, value = T), grep("txt", z, value = T)))
      return(paste0(paste0(head(strsplit(x, "/")[[1]], n=-1), collapse = "/"), "/", z))
    }, SIMPLIFY = F)

    dir.ds <- sapply(url.index, function(x, d = dir_out, l = level) paste0(d, "/", tail(strsplit(x, "/")[[1]], n=2)[1], "_", toupper(l)), USE.NAMES = F)
    catch <- sapply(dir.ds, function(x) dir.create(x, showWarnings = F))
    file.ds <- mapply(url = url.files, dir = dir.ds, FUN = function(url, dir){
      files <- sapply(url, function(x, d = dir) paste0(d, "/", tail(strsplit(x, "/")[[1]], n=1)), USE.NAMES = F)
      sub.make <- !file.exists(files)
      mapply(u = url[sub.make], f = files[sub.make], FUN = function(u, f) gSD.download(name = tail(strsplit(u, "/")[[1]], n=1), url.file = u, file = f), SIMPLIFY = F)
      return(files)
    }, SIMPLIFY = F)
  }

  out(paste0("Requested records are stored in '", dir_out, "'."))
  return(file.ds)
}
