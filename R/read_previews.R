#' Read records previews
#' 
#' \code{read_previews} reads georeferences preview images downloaded using \code{get_previews} and returns them as a list of \code{raster stack} objects.
#'
#' @inheritParams get_previews
#' 
#' @return A list of \code{raster stack} objects
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom raster stack
#' @export

read_previews <- function(records){
  
  records <- .check_records(records, col.names = "preview_file")
  return(lapply(records$preview_file, stack))
  
}