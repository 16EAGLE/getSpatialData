#' getSpatialData settings
#'
#' Defines global settings that are used by all \code{getSpatialData} functions
#'
#' @param hub_user character, user name to the Copernicus Open Access Hub. Register on \url{https://scihub.copernicus.eu/}.
#' @param hub_pass character, password to the Copernicus Open Access Hub.
#' @param dir_archive character, directory to the \code{getSpatialData} archive folder.
#'
#' @details
#' \code{set_login_CopHub} defines the login credentials for the Copernicus Open Access Hub. These will be saved and made available for all \code{getSpatialData} functions during the whole session and will be erased when quitting the session. Leave the \code{hub_user} and \code{hub_pass} arguments of the \code{getSentinel*} functions to their default value (\code{NULL}) to force them to use the global credentials defined with \code{set_login_CopHub}.
#' \code{set_archive} globally defines the directory on your machine (or an external device) where getSpatialData should build up its donwload data archive. Since getSpatialData handles big amounts of data, it is recommended to once define a location where enough free storage is available and then afterwards to not change the archive location. You need to define the archives location for each session after loading getSpatialData. It will then be remembered for the duration of the session. Apart from the archive location, you can manually define a download path when calling the *_data functions. If you do not define a path there, getSpatialData will direct the download to the defined archive. The archive is structred by sensors.
#'
#'
#' @return None.
#' @author Jakob Schwalb-Willmann
#'
#'
#' @export
#' @name gSD_settings
#' @examples
#' ## Define user credentials for the Copernicus Open Access Hub
#' set_login_CopHub(hub_user = "my_user_name", hub_pass = "my_password")
#'
#' @seealso getSentinel_query
#'
set_login_CopHub <- function(hub_user, hub_pass = NULL){

  if(is.null(hub_pass)){hub_pass <- getPass()}
  char_args <- list(hub_user = hub_user, hub_pass = hub_pass)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}
  }
  options(gSD.cophub_user=hub_user)
  options(gSD.cophub_pass=hub_pass)
  options(gSD.cophub_set=TRUE)
}

#' @rdname gSD_settings
#' @export

set_archive <- function(dir_archive){

  if(!is.character(dir_archive)){out(paste0("Argument 'dir_archive' needs to be of type 'character'."), type = 3)}
  if(!dir.exists(dir_archive)) out("The defined directory does not exist.", type=3)
  options(gSD.archive=dir_archive)
  options(gSD.archive_set=TRUE)
}
