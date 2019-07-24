#' check logins
#'
#' @param records
#'
#' @keywords internal
#' @noRd
.check_login <- function(services = NULL, records = NULL, verbose = F){
  
  if(is.null(services)){
    if(is.null(records)){
      services <- c("USGS", "Copernicus")
    } else{
      if("Landsat" %in% records$product_group | "MODIS" %in% records$product_group) services <- c(services, "USGS")
      if("Sentinel" %in% records$product_group) services <- c(services, "Copernicus")
    }
  }
  
  if(all("Copernicus" %in% services, !getOption("gSD.dhus_set"))) out("You are not logged in to Copernicus Hub (anymore). Please log in first using login_CopHub().", type = 3) else if(isTRUE(verbose)) out("You are currently logged in to Copernicus Hub.", msg = T)
  if(all("USGS" %in% services, !getOption("gSD.usgs_set"))) out("You are not logged in to USGS ERS (anymore). Please log in first using login_USGS().", type = 3) else if(isTRUE(verbose)) out("You are currently logged in to USGS ERS.", msg = T)
}

#' check dir_out
#'
#' @param dir_out
#'
#' @keywords internal
#' @noRd
.check_dir_out <- function(dir_out, which = NULL){
  
  ## Check output directory
  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)) dir_out <- getOption(paste0("gSD.archive", if(!is.null(which)) paste0("_", which)))
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }
  return(path.expand(dir_out))
}