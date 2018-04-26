#' Download Landsat data
#'
#' \code{getLandsat_data} downloads Landsat datasets queried using \link{getLandsat_query} from USGS ESPA.
#'
#' @inheritParams getLandsat_query
#' @param products data.frame, one or multiple prodcuts (each represented by one row), as it is returned by \link{getLandsat_query}.
#' @param dir_out character, full path to download target directory. Optional. If not set, \code{getSentinel_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData  archive folder.
#' @param level character, the requested product level. Defaul is "sr" for surface reflectance. Available levels can be obtained from the "levels_available" field returned for each product by \link{getLandsat_query}.
#' @param source character, either:
#' \itemize{
#'    \item "auto" for automatic selection of data source depending on \code{level}
#'    \item "ESPA" to download on-demand products from USGS-EROS ESPA
#'    \item "AWS" to download from Amazon Webservices (Landsat-8 with \code{level="l1"} only)
#' }
#' @param force logical. If \code{TRUE}, download is forced even if file already exisits in the download directory. Default is \code{FALSE}.
#'
#' @return Character vector of files that had been downloaded.
#'
#' @details Using ESPA, \code{getLandsat_data} offers the possibility of downloading not only L1 products, but also atmospherically corrected imager (surfaces reflectances) or multiple indeces (see argument \code{level}).
#' @note  Since ESPA is an on-demand service, \link{getLandsat_query} places an order and than waits for the requested items to be available, before they are downloaded. Therefore, the runtime of the function is depending on how fast an order is being processed by the ESPA server. The function status is indicated by the console messages that it is prompting during execution.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom tools md5sum
#' @importFrom httr content
#'
#' @seealso \link{getLandsat_names} \link{getLandsat_query} \link{getLandsat_preview}
#' @export
#'
getLandsat_data <- function(products, level = "sr", source = "auto", dir_out = NULL, force = FALSE, username = NULL, password = NULL, verbose = TRUE){

  ## Global USGS login
  if(is.TRUE(getOption("gSD.usgs_set"))){
    if(is.null(username)){username <- getOption("gSD.usgs_user")}
    if(is.null(password)){password <- getOption("gSD.usgs_pass")}
  }
  if(!is.character(username)){out("Argument 'username' needs to be of level 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)}
  if(!is.null(password)){password = password}else{password = getPass()}
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)

  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)){dir_out <- paste0(getOption("gSD.archive"), "/LANDSAT/", toupper(level), "/")}
    if(!dir.exists(dir_out)) dir.create(dir_out)
  }

  source <- tolower(source)
  level <- tolower(level)
  char_args <- list(level = level, source = source)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(!dir.exists(dir_out)) out("The defined output directory does not exist.", type=3)

  prod.id <- products$displayId
  levels.avail <- products$levels_available

  ## check if requested level is allowed
  for(x in levels.avail) if(length(grep(level, x)) == 0) out(paste0("Requested product level '", level, "' is not available for at least one product in 'products'. Check column 'levels_available'."), type = 3)

  ## check/set source (the Level-1 issue fix: direct EE download, needs machine-to-machine privilleges)
  if(source == "auto"){
    if(level == "l1"){
      if(all(sapply(products$displayId, function(x) if(length(grep("LC08", strsplit(x, "_")[[1]][1])) == 0) F else T, USE.NAMES = F))){
        source <- "aws"
      } else out("getLandsat_data currently supports download of Level 1 (level = 'l1') products for Landsat-8 only. Argument 'products' contains products that do not originate from Landsat-8.", type = 3)
    } else source <- "espa"
  } else{
    if(level == "l1" & source == "espa") out("Argument 'source' cannot be set to 'espa', if 'level' is set to 'l1'. ESPA is currently providing higher-level on-demand products only.", type = 3)
  }

  ## AWS
  if(source == "aws"){

    out("Checking availability of requested products on AWS...")
    file.gz <- tempfile(fileext = ".gz")
    aws.scenes <- gSD.get(getOption("gSD.api")$aws.l8.sl, dir.file = file.gz)
    aws.scenes <- readLines(file.gz) #much faster than read.csv

    out("Recieving download AWS URLs of requested products...")
    url.index <- mapply(x = products$entityId, d = products$displayId, function(x, d, ds = aws.scenes){
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

    url.files <- mapply(x = url.index, y = products$displayId, function(x, y){
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


  ## ESPA
  if(source == "espa"){

    ## files
    dir.ds <- sapply(products$displayId, function(x, d = dir_out, l = level) paste0(d, "/", x, "_", toupper(l)), USE.NAMES = F)
    catch <- sapply(dir.ds, function(x) dir.create(x, showWarnings = F))
    file.ds <- sapply(dir.ds, function(x) paste0(x, "/", tail(strsplit(x, "/")[[1]], n=1), ".tar.gz"), USE.NAMES = F)

    if(!isTRUE(force)){
      sub.avoid <- which(file.exists(file.ds) == T)
      if(length(sub.avoid) > 0) out(paste0("Product(s) '", paste0(products$displayId[sub.avoid], collapse = "', "), "' are already present and will be skipped, since force = FALSE."))
      products <- products[!file.exists(file.ds),]
      file.down <- file.ds[!file.exists(file.ds)]
    }

    if(nrow(products) != 0){
      out("Ordering requested items from ESPA...")
      #order.list <- espa.order(id = prod.id, product = level, username = username, password = password, format = "gtiff")
      order.list <- "espa-jxsw@web.de-04262018-073617-054"
      out("DEMO: FIXED ORDER!!!", type = 2)

      ## check order(s)
      remain.active = TRUE; ini = TRUE; show.status = TRUE
      while(remain.active){

        ## query server for all ordered items
        order.status <- lapply(order.list, function(x, user = username, pass = password) gSD.get(paste0(getOption("gSD.api")$espa, "order-status/", x), user, pass))

        ## get tiems
        items <- lapply(order.list, function(x, user = username, pass = password){
          y <- gSD.get(paste0(getOption("gSD.api")$espa, "item-status/", x), user, pass)
          status <- content(y)
        })

        ## get items content
        items <- lapply(items, function(x) lapply(x[[1]], function(y){
          r <- unlist(y)
          names(r) <- names(y)
          return(r)
        }))

        ## make items data.frame containing recieve status
        items <- data.frame(do.call(rbind, lapply(items, function(x) do.call(rbind, lapply(x, function(y) rbind(y))))), row.names = NULL, check.names = F, fix.empty.names = F, stringsAsFactors = F)
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
            out(paste0("Starting download of product(s) '", paste0(items.get$name, collapse = "', "), "."), msg = T)
            items.df$recieved[sub.download] <- apply(items.get, MARGIN = 1, function(x, d = dir_out){

              y <- rbind.data.frame(x, stringsAsFactors = F)
              colnames(y) <- names(x)
              gSD.download(name = y$name, url.file = y$product_dload_url, url.checksum = y$cksum_download_url, file = y$file)
            })
            show.status <- TRUE

          } else{
            if(isTRUE(show.status)) out(paste0("Waiting for product(s) '", paste0(items.df$name[items.df$available == F], collapse = "', "), "' to be ready for download from ESPA (this may take a while)..."))
            show.status <- FALSE
          }
        }
        Sys.sleep(10) #wait before reconnecting to ESPA to recheck status
      }
    }
  }
  out(paste0("Requested products are stored in '", dir_out, "'."))
  return(file.ds)
}
