#' Check on-demand availability of records
#' 
#' \code{check_availability} checks for each record whether it is available on-demand (and thus can be downloaded instantly) or not (and thus must be ordered before download).
#'
#' @inheritParams get_previews
#' 
#' @note To use this function, you must be logged in at the services required for your request. See the examples and \link{login} for details.
#' @return A data frame of records (as defined with argument \code{records}), extended by a column \code{available_instantly} (logical).
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @export

check_availability <- function(records){
  
  # check login
  .check_login(records)
  
  # create new colunm
  records.names <- colnames(records)
  records$available_instantly <- NA
  
  # Sentinel
  if("Sentinel" %in% records$product_group){
    out("Checking instant availability for Sentinel records...")
    records.sentinel <- records[records$product_group == "Sentinel",]
    records.sentinel$cred <- .lapply(records.sentinel$product, function(x){
      .CopHub_select(x = "auto", p = x, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass"))
    })
    records[records$product_group == "Sentinel",]$available_instantly <- .apply(records.sentinel, MARGIN = 1, function(x, names = colnames(records.sentinel)){
      as.logical(toupper(unlist(.get_odata(x$entity_id, x$cred, field = "Online/$value"))))
    })
  }
  
  # Landsat
  if("Landsat" %in% records$product_group){
    out("Checking instant availability for Landsat records...")
    records[records$product_group == "Landsat",]$available_instantly <- records[records$product_group == "Landsat",]$level == "l1"
  }
  
  # MODIS
  if("MODIS" %in% records$product_group){
    out("Checking instant availability for MODIS records...")
    records[records$product_group == "MODIS",]$available_instantly <- TRUE
  }
  
  return(.column_summary(records, records.names))
}