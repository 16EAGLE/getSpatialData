#' Download Landsat data
#'
#' \code{getLandsat_data} downloads Landsat datasets queried using \link{getLandsat_query} from USGS ESPA.
#'
#' @inheritParams getLandsat_query
#' @param products data.frame, one or multiple prodcuts (each represented by one row), as it is returned by \link{getLandsat_query}.
#' @param dir_out character, full path to download target directory. Optional. If not set, \code{getSentinel_data} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData  archive folder.
#' @param level character, the requested product level. Defaul is "sr" for surface reflectance. Available levels can be obtained from the "levels_available" field returned for each product by \link{getLandsat_query}.
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
getLandsat_data <- function(products, level = "sr", dir_out = NULL, force = FALSE, username = NULL, password = NULL){

  ## Global USGS login
  if(is.TRUE(getOption("gSD.usgs_set"))){
    if(is.null(username)){username <- getOption("gSD.usgs_user")}
    if(is.null(password)){password <- getOption("gSD.usgs_pass")}
  }
  if(!is.character(username)){out("Argument 'username' needs to be of level 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)}
  if(!is.null(password)){password = password}else{password = getPass()}

  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)){dir_out <- paste0(getOption("gSD.archive"), "/USGS/", toupper(level), "/")}
    if(!dir.exists(dir_out)) dir.create(dir_out)
  }

  prod.id <- products$displayId
  levels.avail <- products$levels_available
  char_args <- list(prod.id = prod.id, levels.avail = levels.avail)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(!dir.exists(dir_out)) out("The defined output directory does not exist.", type=3)

  ## check if requested level is allowed
  for(x in levels.avail) if(length(grep(level, x)) == 0) out(paste0("Requested product level '", level, "' is not available for at least one product in 'products'. Check column 'levels_available'."), type = 3)

  ## place order(s)
  out("Ordering requested items from ESPA...")
  order.list <- espa.order(id = prod.id, product = level, username = username, password = password, format = "gtiff")
  #order.list <- "espa-jxsw@web.de-04222018-101758-710"

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

    ## make items data.frame containing revieve status
    items <- data.frame(do.call(rbind, lapply(items, function(x) do.call(rbind, lapply(x, function(y) rbind(y))))), row.names = NULL, check.names = F, fix.empty.names = F, stringsAsFactors = F)
    items <- cbind(items, items$status == "complete")
    #items <- cbind.data.frame(items, sapply(as.character(items$product_dload_url), function(x) if(nchar(x) == 0) "" else paste0(dir_out, "/", tail(strsplit(x, "/")[[1]], n=1)), USE.NAMES = F), stringsAsFactors = F)
    items <- cbind.data.frame(items, sapply(as.character(items$name), function(x, l = level) paste0(dir_out, "/", x, "_", toupper(level), ".tar.gz"), USE.NAMES = F), stringsAsFactors = F)
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
        items.df$recieved[sub.download] <- apply(items.get, MARGIN = 1, function(x){
          y <- rbind.data.frame(x, stringsAsFactors = F)
          colnames(y) <- names(x)

          out(paste0("Attempting to download '", y$name, "' to '", y$file, "'..."), msg = T)
          md5 <- strsplit(content(gSD.get(y$cksum_download_url), as = "text", encoding = "UTF-8"), " ")[[1]][1]
          gSD.get(y$product_dload_url,dir.file = y$file, prog = T)

          if(as.character(md5sum(y$file)) == tolower(md5)){
            out("Successfull download, MD5 check sums match.", msg = T)
            return(TRUE)

          } else{
            out(paste0("Download failed, MD5 check sums do not match. Will retry."), type = 2)
            file.remove(y$file)
            return(FALSE)
          }
        })
        show.status <- TRUE

      } else{
        if(isTRUE(show.status)) out(paste0("Waiting for product(s) '", paste0(items.df$name[items.df$available == F], collapse = "', "), "' to be ready for download from ESPA (this may take a while)..."))
        show.status <- FALSE
      }
    }
    Sys.sleep(10)
  }
  out(paste0("Requested products have been stored to '", dir_out, "'."))
  return(items.df$file)
}
