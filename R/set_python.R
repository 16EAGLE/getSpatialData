#' Set Python path
#'
#' Sets up the connection to a python installation manually.
#'
#' @param py_path character, path to preferred python installation
#'
#' @return None.
#' @author Jakob Schwalb-Willmann
#'
#'
#' @importFrom reticulate use_python
#' @export
#'

set_python <- function(py_path){

  if(!is.character(py_path)){out("Argument 'py_path' needs to be of type 'character'", type = 3)}
  pt <- try(use_python(python = py_path, required = TRUE), silent = TRUE)
  if(class(pt) == "try-error"){out("The specified Python path could not be executed.", type=3)}

}
