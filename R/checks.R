#' check logins
#'
#' @param services character, services to check
#'
#' @keywords internal
#' @noRd
.check_login <- function(services = c("USGS", "Copernicus")){
  if(all("Copernicus" %in% services, !getOption("gSD.dhus_set"))) out("You are not logged in to Copernicus Hub (anymore). Please log in first using login_CopHub().", type = 3) 
  if(all("USGS" %in% services, !getOption("gSD.dhus_set"))) out("You are not logged in to USGS ERS (anymore). Please log in first using login_USGS().", type = 3)
}