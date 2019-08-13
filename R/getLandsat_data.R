#' Download Landsat data
#'
#' \code{getLandsat_data} downloads Landsat data queried using \link{getLandsat_records} from different sources.
#'
#' @inheritParams getLandsat_records
#' @param records data.frame, one or multiple records (each represented by one row), as it is returned by \link{getLandsat_records}.
#' @param dir_out character, full path to download target directory. Optional. If not set, \code{getLandsat_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData  archive folder.
#' @param level character, one or multiple product levels to be requested. Defaul is "sr" for surface reflectance. Available levels can be obtained from the "levels_available" field returned for each product by \link{getLandsat_records}.
#' @param source character, either:
#' \itemize{
#'    \item "auto" for automatic selection of data source depending on \code{level}
#'    \item "ESPA" to download on-demand products from USGS-EROS ESPA
#'    \item "AWS" to download from Amazon Webservices (Landsat-8 with \code{level="l1"} only)
#' }
#' @param force logical. If \code{TRUE}, download is forced even if file already exisits in the download directory. Default is \code{FALSE}.
#' @param n.retry numeric, maximum number of download (re-)attempts. If downloads of datasets fail (e.g. MD5 checksums do not match), these downloads will be reattampted.
#' @param wait_for_espa logical, whether to wait for ESPA orders to be available for download \code{TRUE} or not.
#' @param username to be removed.
#' @param password to be removed.
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
#' query <- getLandsat_records(time_range = time_range, name = product_names[7])
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
#' @seealso \link{getLandsat_names} \link{getLandsat_records} \link{getLandsat_preview}
#' @export
#'
getLandsat_data <- function(records, level = "sr", source = "auto", dir_out = NULL, wait_for_espa = NULL, force = FALSE, username = NULL, password = NULL, n.retry = 3, verbose = TRUE){

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

  if(missing(records)) out("Argument 'records' must be defined.", type = 3)
  levels <- sapply(strsplit(level, ','), function(x) paste0("'", x, "'"))
  levels <- paste(level, collapse = ".*" )
  for(x in records$levels_available) if(length(grep(levels, x)) == 0) out(paste0("Requested product level '", level, "' is not available for at least one record in 'records'. Check column 'levels_available'."), type = 3)
  

  ## check/set source (direct EE download needs machine-to-machine privilleges)
  if(source == "auto"){
    if(all(level == "l1")){
      if(all(sapply(records$displayId, function(x) if(length(grep("LC08", strsplit(x, "_")[[1]][1])) == 0) F else T, USE.NAMES = F))){
        source <- "aws"
      } else out("getLandsat_data currently supports download of Level 1 (level = 'l1') products for Landsat-8 only. Argument 'records' contains records that do not originate from Landsat-8.", type = 3)
    } else source <- "espa"
  } else{
    if(any(level == "l1") & source == "espa") out("Argument 'source' cannot be set to 'espa', if 'level' is set to 'l1'. ESPA is currently providing higher-level on-demand products only.", type = 3)
  }

  records.names <- colnames(records)
  
  ## Check output directory
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)) dir_out <- paste0(getOption("gSD.archive_get"), "/LANDSAT/", toupper(paste0(level,collapse = "_"))) else dir_out <- path.expand(dir_out)
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  if(!is.character(dir_out)) out(paste0("Argument 'dir_out' needs to be of type 'character'."), type = 3)
  if(!dir.exists(dir_out)) out("The defined archive directory does not exist.", type=3)

  ## ESPA
  if(source == "espa"){

    ## files
    records$dataset_name <- records$displayId
    records$dataset_dir <- sapply(records$dataset_name, function(x, d = dir_out, l = paste0(level, collapse = "_")) paste0(d, "/", x, "_LEVEL_", l), USE.NAMES = F)
    catch <- sapply(records$dataset_dir, function(x) dir.create(x, showWarnings = F))
    records$dataset_file <- sapply(records$dataset_dir, function(x) paste0(x, "/", tail(strsplit(x, "/")[[1]], n=1), ".tar.gz"), USE.NAMES = F)
    
    ## assign item and head
    records$gSD.item <- 1:nrow(records)
    records$gSD.head <- sapply(records$gSD.item, function(i, n = nrow(records)) paste0("[Dataset ", toString(i), "/", toString(n), "] "))
    records$gSD.order <- F
    records$gSD.download <- F

    ## spare existing once from being checked on ESPA
    if(!isTRUE(force)) records$gSD.check <- !file.exists(records$dataset_file) else records$gSD.check <- T
    
    ## check query and skip, if not available
    records <- do.call(rbind, lapply(1:nrow(records), function(i){
      x <- records[i,]
    
      ## check if downloaded
      if(isFALSE(x$gSD.check)){
        out(paste0(x$gSD.head, "Skipping order and download of '", x$dataset_name, "', since '", x$dataset_file, "' already exists..."))
        x$gSD.order <- F
        x$gSD.download <- F
        return(x)
      }
      
      if(!is.null(x$ESPA_orderID)){
        out(paste0(x$gSD.head, "Staging '", x$dataset_name, "' for checking ESPA order status..."))
        x$gSD.order <- F
        x$gSD.download <- T
        return(x)
      }
      
      ## check if available via ESPA
      response <- gSD.get(paste0(getOption("gSD.api")$espa, "available-products/", x$dataset_name), getOption("gSD.usgs_user"), getOption("gSD.usgs_pass"))
      if(names(content(response)) == "not_implemented"){
        out(paste0(x$gSD.head, "Skipping order and download of '", x$dataset_name, "', since it could not be found in the ESPA database."), type = 2)
        x$gSD.order <- F
        x$gSD.download <- F
        return(x)
        
      } else{
        out(paste0(x$gSD.head, "Staging '", x$dataset_name, "' for being ordered at ESPA..."))
        x$gSD.order <- T
        x$gSD.download <- T
        return(x)
      }
    }))
    
    if(nrow(records[records$gSD.order,]) > 0){
      records$ESPA_orderID <- .ESPA_order(records[records$gSD.order,]$dataset_name, level = level, username = username, password = password, format = "gtiff", verbose = verbose)
    }
    
    ## download
    if(any(records$gSD.download)){
      records <- .ESPA_download(records[records$gSD.download == T,], username = username, password = password, dir_out = dir_out, n.retry = n.retry)
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
    records$dataset_url <- mapply(x = records$gSD.url.index, y = records$displayId, function(x, y){
      name = paste0(head(strsplit(grep("LC08", strsplit(x, "/")[[1]], value = T), "_")[[1]], n=-4), collapse = "_")
      z <- grep("href", grep(name, unlist(strsplit(strsplit(grep("body", xml_contents(content(gSD.get(x), encoding = "UTF-8")), value = T), ">")[[1]], "<"), recursive = T), value = T), value = T, invert = T)
      z <- unique(c(grep("TIF", z, value = T), grep("IMD", z, value = T), grep("ovr", z, value = T), grep("txt", z, value = T)))
      return(paste0(paste0(head(strsplit(x, "/")[[1]], n=-1), collapse = "/"), "/", z))
    }, SIMPLIFY = F, USE.NAMES = F)

    ## create dir names
    records$dataset_dir <- sapply(records$gSD.url.index, function(x, d = dir_out, l = level) paste0(d, "/", tail(strsplit(x, "/")[[1]], n=2)[1], "_", toupper(l)), USE.NAMES = F)
    catch <- sapply(records$dataset_dir, function(x) dir.create(x, showWarnings = F))
    
    ## create file names ERROR
    records$dataset_file <- mapply(url = records$dataset_url, dir = records$dataset_dir, FUN = function(url, dir){
      sapply(url, function(x, d = dir) paste0(d, "/", tail(strsplit(x, "/")[[1]], n=1)), USE.NAMES = F)
    }, SIMPLIFY = F, USE.NAMES = F)
    
    ## create console items
    records$dataset_name <- sapply(records$dataset_url, function(u) sapply(u, function(x) tail(strsplit(x, "/")[[1]], n=1), USE.NAMES = F), USE.NAMES = F, simplify = F)
    records$gSD.item <- 1:nrow(records)
    records$gSD.head <- sapply(records$gSD.item, function(i, n = nrow(records)) paste0("[Dataset ", toString(i), "/", toString(n), "] "))
    
    # sep records per files and try to download them
    records <- do.call(rbind, lapply(1:nrow(records), function(i){
      x <- records[i,]
      files <- data.frame(x$gSD.head, x$dataset_name, unlist(x$dataset_url), unlist(x$dataset_file), stringsAsFactors = F)
      colnames(files) <-  c("gSD.head", "dataset_name", "dataset_url", "dataset_file")
      files$gSD.head <- paste0(gsub("] ", "", files$gSD.head), paste0(" | File ", 1:nrow(files), "/", nrow(files), "] "))
      
      files <- gSD.retry(files, gSD.download, names = colnames(files), prog = getOption("gSD.verbose"), force = force, n.retry = n.retry)
      x$download_attempts <- max(files$download_attempts)
      x$download_success <- all(files$download_success)
      return(x)
    }))
  }

  return(.column_summary(records, records.names, download_success = T))
}
