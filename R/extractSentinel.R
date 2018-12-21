#' Extract Sentinel zip archives
#'
#' \code{extractSentinel} unzips Sentinel zip archives downloaded using \link{getSentinel_data}.
#'
#' @inheritParams getSentinel_data
#' @param datasets character vector or list, containing paths to datasets (.zip, as downloaded by \code{getSentinel_data}) that should be extracted.
#' @param dir_out character, full path to target directory. By default, extracted .SAFE folders are stored in the same directory as the zip originals (recommended).
#' @param junkpaths logical, reduces the folder structure inside the extracted SAFE folder to one level. Although loosing the original folder structure, this is recommended especially on windows machine due to issues that can occure with very long paths.
#' @param list_only logical, if \code{TRUE}, extracting is skipped and just the files contained in \code{datasets} are returned.
#' @param pattern character, pattern (e.g. "xml") of files to be extracted.
#' 
#' @return List containing paths to the extracted .SAFE folders and all contained files.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom utils unzip
#' 
#' @seealso prepSentinel
#' @export

extractSentinel <- function(datasets, dir_out = NULL, junkpaths = T, list_only = F, pattern = NULL, verbose = T){
  
  ## Intercept false inputs and get inputs
  if(!isTRUE(inherits(datasets, "list"))) datasets <- unlist(datasets)
  if(!is.null(dir_out)) if(!dir.exists(dir_out)) out("The defined output directory does not exist.", type=3)
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  
  sapply(datasets, function(x, jp = junkpaths, lo = list_only, p = pattern){
    out(paste0("Extracting '", x, "'..."))
    
    #look into archive
    files <- unzip(x, list = T)$Name
    name <- files[1]
    if(!is.null(p)) files <- grep(p, files, value =T)
    if(isTRUE(lo)) return(files)
    
    SAFE.dir <- paste0(paste0(head(strsplit(x, "/")[[1]], n=-1), collapse = "/"), "/", name)
    if(!dir.exists(SAFE.dir)) catch <- dir.create(SAFE.dir)
    
    #extract archive
    catch <- try(unzip(x, exdir = SAFE.dir, junkpaths = jp, files = files)) #sapply(files.zip, function(x) tail(strsplit(x, "/")[[1]], n=1) USE.NAMES = F)
    if(inherits(catch, "try-error")) if(inherits(catch, "try-error")) out(paste0("Extracting '", x, "' failed."), type = 3)
    return(catch)
  }, simplify = F)
}