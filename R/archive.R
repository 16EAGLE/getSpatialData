#' Session-wide archive directory
#'
#' Functions that set and get a session-wide archive directory that can be used by all \code{getSpatialData} functions. 
#'
#' @inheritParams getSentinel_records
#' @param dir_archive character, directory to the \code{getSpatialData} archive folder.
#' @param create logical, whether to create directory, if not existing or not.
#'
#' @details
#' \code{set_archive} defines the session directory on your machine (or an external device) where getSpatialData should build up its donwload data archive. Since getSpatialData handles big amounts of data, it is recommended to once define a location where enough free storage is available and then afterwards to not change the archive location. You need to define the archives location for each session after loading getSpatialData. It will then be remembered for the duration of the session. Apart from the archive location, you can manually define a download path when calling the *_data functions. If you do not define a path there, getSpatialData will direct the download to the defined archive. The archive is structred by sensors.
#'
#' \code{get_archive} returns the currently defined archive directory.
#'
#' @author Jakob Schwalb-Willmann
#'
#'
#' @export
#' @name archive
#' @examples
#' ## set archive directory
#' dir.arc <- tempdir()
#' set_archive(dir.arc)
#' 
#' ## return current archive directory
#' get_archive()
#'
#' @seealso \link{get_data} \link{get_previews}
#'
set_archive <- function(dir_archive, create = T){
  
  if(!is.character(dir_archive)){out(paste0("Argument 'dir_archive' needs to be of type 'character'."), type = 3)}
  if(!dir.exists(dir_archive)) if(isTRUE(create)) dir.create(dir_archive, recursive = T) else out("The defined directory does not exist.", type=3)
  
  options(gSD.archive = path.expand(dir_archive))
  options(gSD.archive_data = paste0(dir_archive, "/data"))
  options(gSD.archive_previews = paste0(dir_archive, "/previews"))
  #options(gSD.archive_prep = paste0(dir_archive, "/prep_data"))
  options(gSD.archive_set = TRUE)
  #out(paste0("Session archive directory has been set to '", dir_archive, "'."), msg = T)
}

#' @rdname archive
#' @importFrom mapedit drawFeatures
#' @export
get_archive <- function() if(isTRUE(getOption("gSD.archive_set"))) getOption("gSD.archive") else out("Archive has not been set. Use set_archive() to define an getSpatialData archive directory.", msg = T)



