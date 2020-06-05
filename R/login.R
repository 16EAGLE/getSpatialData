#' Session-wide login
#'
#' Functions to login to services supported by \code{getSpatialData} once for the running session and to recieve their current status.
#'
#' @inheritParams getSentinel_records
#' @param username character, user name to login at the respective service.
#' @param password character, password to login at the respective service.
#' @param n_retry numeric, number of attempts to login, if login fails
#' @param value logical, whether to return a data frame containing service status or not. Default is \code{FALSE}.
#'
#' @details
#' 
#' \code{login_CopHub} logs you in at the ESA Copernicus Open Access Hub using your credentials (register once at https://scihub.copernicus.eu/).
#' 
#' \code{login_USGS} logs you in at the USGS EROS Registration System (ERS) using your credentials (register once at https://ers.cr.usgs.gov/register/).
#' 
#' \code{login_earthdata} logs you in at the NASA Earth Data User Registration System (URS) using your credentials (register once at https://urs.earthdata.nasa.gov/users/new
#' 
#' \code{services} displays the status of all online services used by \code{getSpatialData}. Services that are operating as usual are labeled "available". Returns a \code{data.frame} containing the service status, if argument \code{value} is set to \code{TRUE}.
#'
#' @note
#' Login credentials will be saved and made available for all \code{getSpatialData} functions during the whole session. They will be erased when quitting the session. Alternatively, login credentials can be set individually with each \code{get*} function call.
#'
#'
#' @author Jakob Schwalb-Willmann
#'
#'
#' @export
#' @name login
#' @examples
#' library(getSpatialData)
#' 
#' \dontrun{
#' ## Define user credentials for the Copernicus Open Access Hub
#' login_CopHub(username = "my_user_name", password = "my_password")
#'
#' ## Define USGS user credentials
#' login_USGS(username = "my_user_name", password = "my_password")
#' 
#' ## get status of all services
#' services()
#' }

#' @seealso \link{get_records}
#'
login_CopHub <- function(username = NULL, password = NULL, n_retry = 3, verbose = TRUE){
  
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  if(is.null(username)) username <- getPass("Username (ESA Copernicus Open Access Hub):")
  if(is.null(password)) password <- getPass("Password (ESA Copernicus Open Access Hub):")
  char_args <- list(username = username, password = password)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  
  # verify credentials
  .retry(.get, url = paste0(getOption("gSD.api")$dhus, "odata/v1/"), username = username, password = password, value = F,
         fail = out("Login failed. Please retry later or call services() to check if ESA Copernicus services are currently unavailable.", type=3),
         n = n_retry)
  
  # save credentials, if login was succesfull
  options(gSD.dhus_user = username, gSD.dhus_pass = password, gSD.dhus_set = TRUE, gSD.dhus_time = Sys.time())
  out("Login successfull. ESA Copernicus credentials have been saved for the current session.")
}


#' @rdname login
#' @export
login_USGS <- function(username = NULL, password = NULL, n_retry = 3, verbose = TRUE){
  
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  if(is.null(username)) username <- getPass("Username (USGS EROS Registration System):")
  if(is.null(password)) password <- getPass("Password (USGS EROS Registration System):")
  char_args <- list(username = username, password = password)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  
  # verify credentials
  .ERS_login(username, password, n_retry = n_retry) -> key
  
  # save credentials
  options(gSD.usgs_apikey = key, gSD.usgs_user = username, gSD.usgs_pass = password, gSD.usgs_set = TRUE, gSD.usgs_time = Sys.time())
  out("Login successfull. USGS ERS credentials have been saved for the current session.")
}

#' @rdname login
#' @export
login_earthdata <- function(username = NULL, password = NULL, n_retry = 3, verbose = TRUE){
  
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  if(is.null(username)) username <- getPass("Username (NASA URS EarthData):")
  if(is.null(password)) password <- getPass("Password (NASA URS EarthData):")
  char_args <- list(username = username, password = password)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  
  # verify credentials
  x <- .retry(httr::GET, url = gsub("allData", "README", getOption("gSD.api")$laads), 
         config = httr::authenticate(username, password),
         fail = out("Login failed. Please retry later or call services() to check if LAADS is currently unavailable.", type=3),
         n = n_retry)

  # save credentials
  options(gSD.ed_user = username, gSD.ed_pass = password, gSD.ed_set = TRUE, gSD.ed_time = Sys.time())
  out("Login successfull. NASA URS EarthData credentials have been saved for the current session.")
}

#' @rdname login
#' @importFrom httr GET http_status
#' @importFrom cli cat_bullet
#' @export
services <- function(value = F, verbose = T){
  
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  
  # get service URLs
  urls <- getOption("gSD.api")
  urls <- urls[names(urls) != "aws.l8.sl"]
  urls$aws.l8 <- gsub("c1/L8/", "", urls$aws.l8)
  
  # get service status (login for ESPA)
  response <- lapply(urls, function(x) try(.get(x), silent = T))
  if(isTRUE(getOption("gSD.usgs_set"))) response$espa <- try(.get(urls$espa, username = getOption("gSD.usgs_user"), password = getOption("gSD.usgs_pass")), silent = T)
  
  df <- do.call(rbind, lapply(response, function(x) if(!inherits(x, "try-error")) rbind.data.frame(http_status(x), stringsAsFactors = F) else NA))
  df$code <- sapply(response, function(y) if(!inherits(y, "try-error")) y$status_code else NA)
  df$service <- unlist(getOption("gSD.api.names")[rownames(df)])
  df$id <- rownames(df)
  
  # add codes for errored requests to figure out reason below
  error.sub <- sapply(response, function(x) inherits(x, "try-error"))
  df[error.sub, "code"] <- sapply(response[error.sub], function(x) as.numeric(strsplit(strsplit(x, "HTTP ")[[1]][2], ")")[[1]][1]))
  
  # interpret service status
  df$status <- "available"
  df$colour <- "green"
  df$remark <- "Connection successfully established."
  
  # not ok
  items <- which(df$code != 200 & df$code != 401)
  if(length(items) > 0){
    df[items,]$status <- "unknown"
    df[items,]$colour <- "blue"
    df[items,]$remark <- as.character(df[items,]$message)
  }
  
  # maintenace
  items <- which(df$code == 301)
  if(length(items) > 0){
    df[items,]$status <- "maintenance"
    df[items,]$colour <- "orange"
  }
  
  # maintenace
  items <- which(df$code == 503)
  if(length(items) > 0){
    df[items,]$status <- "unavailable"
    df[items,]$colour <- "red"
    df[items,]$remark <- "Internal server error."
  }
  
  
  # timeout, no connection etc.
  items <- which(is.na(df$code))
  if(length(items) > 0){
    df[items,]$status <- "no connection"
    df[items,]$colour <- "red"
    df[items,]$remark <- "Connection could not be established."
  }
  
  
  if(isTRUE(getOption("gSD.verbose"))){
    catch <- apply(df, MARGIN = 1, function(x, nc = max(nchar(df$service)), names = colnames(df)){
      y <- rbind.data.frame(x, stringsAsFactors = F)
      colnames(y) <- names
      cat_bullet(paste0(y$service, ":", paste0(rep(" ", times = nc-nchar(y$service)), collapse = ""), "  '", y$status, "' ", paste0(rep(" ", times = (13-nchar(y$status))), collapse = ""), "'", y$remark, "'"), bullet_col = y$colour)
    })
  }
  if(isTRUE(value)) return(df[c("service", "status", "remark", "category", "reason", "message", "code")])
}

#' @rdname getSpatialData-deprecated
#' @export
services_avail <- function(...){
  .Deprecated("services", "getSpatialData")
  services(...)
}