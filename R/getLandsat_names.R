#' Get Landsat dataset names from the USGS Earth Explorer
#'
#' \code{getLandsat_names} obtains names of available Landsat datasets from the USGS Earth Explorer. They can optionally be used with the \link{getLandsat_query} function for querying a specific Landsat dataset instead of all.
#'
#' @param username character, a valid USGS user name. Default is NULL. Leave it undefined, if you want to use \link{login_USGS} to define the login credentials once for all \code{get*} functions that connect to USGS services during the session. Register on \url{https://ers.cr.usgs.gov/register/}.
#' @param password character, the password to the specified user account. If \code{NULL}, the password will be taken from \link{login_USGS} inputs or, if \code{login_USGS} is not in use, asked interactively.
#'
#' @return A character vector
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#'
#' @seealso \link{getLandsat_query}
#' @export
#'
getLandsat_names <- function(username = NULL, password = NULL){

  if(is.null(username)){
    if(is.TRUE(getOption("gSD.usgs_set"))){
      api.key <- getOption("gSD.usgs_apikey")
    } else {
      out("Argument 'username' needs to be of type 'character'. You can use 'login_USGS()' to define your login credentials globally.", type=3)
    }
  } else{
    if(is.null(password)) password = getPass()
    api.key <- usgs_login(username, password)
  }
  usgs_ds(api.key, "LANDSAT_")
}
