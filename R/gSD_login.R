#' Session-wide login
#'
#' Login to services supported by \code{getSpatialData} once for the running session.
#'
#' @param username character, username to the corresponding service. For details on registration, see details.
#' @param password character, password to the corresponding service.
#'
#' @details
#' \code{login_CopHub} defines the login credentials for the Copernicus Open Access Hub (register on \url{https://scihub.copernicus.eu/})
#'
#' \code{login_USGS} defines USGS login credentials Register on \url{https://ers.cr.usgs.gov/register/}.
#'
#' @note
#' Login credentials will be saved and made available for all \code{getSpatialData} functions during the whole session. They will be erased when quitting the session. Alternatively, login credentials can be set individually with each \code{get*} function call.
#'
#'
#' @return None.
#' @author Jakob Schwalb-Willmann
#'
#'
#' @export
#' @name gSD_login
#' @examples
#' ## Define user credentials for the Copernicus Open Access Hub
#' login_CopHub(username = "my_user_name", password = "my_password")
#'
#' ## Define USGS user credentials
#' login_USGS(username = "my_user_name", password = "my_password")

#' @seealso getSentinel_query getLandsat_query
#'
login_CopHub <- function(username, password = NULL){

  if(is.null(password)){password <- getPass()}
  char_args <- list(username = username, password = password)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  options(gSD.cophub_user=username)
  options(gSD.cophub_pass=password)
  options(gSD.cophub_set=TRUE)
}


#' @rdname gSD_login
#' @export
login_USGS <- function(username, password = NULL){

  if(is.null(password)){password <- getPass()}
  char_args <- list(username = username, password = password)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  options(gSD.usgs_user=username)
  options(gSD.usgs_pass=password)
  options(gSD.usgs_set=TRUE)
  options(gSD.usgs_apikey=usgs_login(username, password))
}
