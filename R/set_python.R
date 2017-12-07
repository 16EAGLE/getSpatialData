#' getSpatialData settings
#'
#' Defines global settings that are used by all \code{getSpatialData} functions
#'
#' @param py_path character, path to preferred python installation.
#' @param hub_user character, user name to the Copernicus Open Access Hub. Register on \url{https://scihub.copernicus.eu/}.
#' @param hub_pass character, password to the Copernicus Open Access Hub.
#'
#' @details
#' \code{set_python} sets up the connection to a python installation manually. This prevents getSpatialData from using a wrong Pyhton version, if multiple installs are available or if the correct install is not recognized automatically.
#'
#' \code{set_cophub_login} defines the login credentials for the Copernicus Open Access Hub. These will be saved and made available for all \code{getSpatialData} functions during the whole session and will be erased when quitting the session. Leave the \code{hub_user} and \code{hub_pass} arguments of the \code{getSentinel*} functions to their default value (\code{NULL}) to force them to use the global credentials defined with \code{set_cophub_login}.
#'
#' @return None.
#' @author Jakob Schwalb-Willmann
#'
#'
#' @importFrom reticulate use_python
#' @export
#' @name gSD_settings
#' @examples
#' ## Define a python path
#' set_python("path/to/my/python") #needs to lead to an executable file, on windows, a python.exe
#'
#' ## Define user credentials for the Copernicus Open Access Hub
#' set_cophub_login(hub_user = "my_user_name", hub_pass = "my_password")
#'
#' @seealso getSentinel_query

set_python <- function(py_path){

  if(!is.character(py_path)){out("Argument 'py_path' needs to be of type 'character'", type = 3)}
  pt <- try(use_python(python = py_path, required = TRUE), silent = TRUE)
  if(class(pt) == "try-error"){out("The specified Python path could not be executed.", type=3)}

}

#' @importFrom getPass getPass
#' @rdname gSD_settings
#' @export
set_cophub_login <- function(hub_user, hub_pass = NULL){

  if(is.null(hub_pass)){hub_pass <- getPass()}
  char_args <- list(hub_user = hub_user, hub_pass = hub_pass)
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}else{TRUE}
  }
  options(gSD.cophub_user=hub_user)
  options(gSD.cophub_pass=hub_pass)
  options(gSD.cophub_def=TRUE)
}

