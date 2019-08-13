#' Restore Sentinel datasets from Copernicus LTA
#'
#' \code{getSentinel_restore} requests to restore Setninel datasets that have been archived by ESA to the Copernicus Long-Term Archive (LTA) (see argument \code{check_avail} of \link{getSentinel_records}).
#'
#' @inheritParams getSentinel_records
#' @param record data.frame, single row data.frame collected from the return of \link{getSentinel_records}, representing the selected record and all its attributes.
#' @param username to be removed.
#' @param password to be removed.
#' 
#' @return TRUE on success.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom getPass getPass
#' @importFrom httr GET content
#' @importFrom xml2 xml_contents as_xml_document
#'
#' @seealso \link{getSentinel_records}
#' @export

getSentinel_restore <- function(record, username = NULL, password = NULL, hub = "auto", verbose = TRUE){
  
  ## Global Copernicus Hub login
  if(is.TRUE(getOption("gSD.dhus_set"))){
    if(is.null(username)) username <- getOption("gSD.dhus_user")
    if(is.null(password)) password <- getOption("gSD.dhus_pass")
  }
  if(!is.character(username)) out("Argument 'username' needs to be of type 'character'. You can use 'login_CopHub()' to define your login credentials globally.", type=3)
  if(!is.null(password)){ password = password} else{ password = getPass()}
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  
  ## check uuid
  if(is.null(record$uuid)) out("Could not find field 'uuid' in record.", type = 3)
  if(!is.character(record$uuid)) out("Field 'uuid' in record must be of type character.", type = 3)
  
  ## Manage API access
  cred <- .CopHub_select(hub, record$platformname, username, password)
  
  ## check availability
  if(is.null(record$download_available)) record$download_available <- as.logical(toupper(unlist(.get_odata(record$uuid, cred, field = "Online/$value"))))
  if(isTRUE(record$download_available)){
    out("Dataset is available on-demand and can be downloaded without restoring from LTA.")
  } else {
    request <- try(gSD.get(paste0(cred[3], "/odata/v1/Products('", record$uuid, "')/$value")), silent = T)
    if(inherits(request, "try-error")) out("The request is not accepted because the number of submitted requested exceeded the allowed user quota. Please retry later.", type = 3)
    out("The restoring request has been successfully forwared to LTA. The record will be available for download as soon as it is restored from LTA. Use getSentinel_records() with check_avail=TRUE or getSentinel_data() to check availability.")
    return(TRUE)
  }
}