#' Session-wide login
#'
#' Functions to login to services supported by \code{getSpatialData} once for the running session and to recieve their current status.
#'
#' @inheritParams getSentinel_records
#' @param username character, user name to login at the respective service.
#' @param password character, password to login at the respective service.
#' @param value logical, whether to return a data frame containing service status or not. Default is \code{FALSE}.
#'
#' @details
#' \code{login_CopHub} defines login credentials for the Copernicus Open Access Hub (register on \url{https://scihub.copernicus.eu/})
#'
#' \code{login_USGS} defines login credentials to the USGS EROS Registration System (ERS). Register on \url{https://ers.cr.usgs.gov/register/}.
#' 
#' \code{services_avail} displays the status of all online services used by \code{getSpatialData}. Services that are operating as usual are labeled "available". Returns a \code{data.frame} containing the service status, if argument \code{value} is set to \code{TRUE}.
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
#' services_avail()
#' }

#' @seealso \link{get_records}
#'
login_CopHub <- function(username = NULL, password = NULL, verbose = TRUE){
  
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  if(is.null(username)) username <- getPass("Username (Copernicus Open Access Hub):")
  if(is.null(password)) password <- getPass("Password (Copernicus Open Access Hub):")
  char_args <- list(username = username, password = password)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  
  # save credentials
  options(gSD.dhus_user=username)
  options(gSD.dhus_pass=password)
  options(gSD.dhus_set=TRUE)
  
  # verify credentials
  x <- try(gSD.get(paste0(getOption("gSD.api")$dhus, "odata/v1/"), username, password), silent = T)
  if(inherits(x, "try-error")) out("Login failed. Please retry later or call services_avail() to check if ESA Copernicus services are currently unavailable.", type=3)
  
  out("Login successfull. ESA Copernicus credentials have been saved for the current session.", msg = T)
}


#' @rdname login
#' @export
login_USGS <- function(username = NULL, password = NULL, verbose = TRUE){
  
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  if(is.null(username)) username <- getPass("Username (USGS EROS Registration System):")
  if(is.null(password)) password <- getPass("Password (USGS EROS Registration System):")
  char_args <- list(username = username, password = password)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  
  ## verify
  options(gSD.usgs_apikey=.ERS_login(username, password))
  
  # save credentials
  options(gSD.usgs_user=username)
  options(gSD.usgs_pass=password)
  options(gSD.usgs_set=TRUE)
  out("Login successfull. USGS ERS credentials have been saved for the current session.", msg = T)
}

#' @rdname login
#' @importFrom httr GET http_status
#' @importFrom cli cat_bullet
#' @export
services_avail <- function(value = F, verbose = T){
  
  .check_login()
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  
  # get service URLs
  urls <- getOption("gSD.api")
  urls <- urls[names(urls) != "aws.l8.sl"]
  urls$aws.l8 <- gsub("c1/L8/", "", urls$aws.l8)
  
  # get service status (login for ESPA)
  response <- lapply(urls, function(x) try(gSD.get(x), silent = T))
  if(isTRUE(getOption("gSD.usgs_set"))) response$espa <- try(gSD.get(urls$espa, username = getOption("gSD.usgs_user"), password = getOption("gSD.usgs_pass")))
  
  df <- do.call(rbind, lapply(response, function(x) if(!inherits(x, "try-error")) rbind.data.frame(http_status(x), stringsAsFactors = F) else NA))
  df$code <- sapply(response, function(y) if(!inherits(y, "try-error")) y$status_code else NA)
  df$service <- unlist(getOption("gSD.api.names")[rownames(df)])
  df$id <- rownames(df)
  
  # interpret service status
  df$status <- "available"
  df$colour <- "green"
  df$remark <- "Connection successfully established."
  
  # not ok
  items <- which(df$code != 200)
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