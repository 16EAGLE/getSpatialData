#' Something
#'
#'
#'
#' @author Jakob Schwalb-Willmann
#' 
#' @export

check_availability <- function(records){
  
  # create new colunm
  records.names <- colnames(records)
  records$available_instantly <- NA
  
  # Sentinel
  if("Sentinel" %in% records$product_group){
    out("Checking instant availability for Sentinel records...")
    records.sentinel <- records[records$product_group == "Sentinel",]
    records.sentinel$cred <- lapply(records.sentinel$product, function(x){
      .CopHub_select(x = "auto", p = x, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass"))
    })
    records[records$product_group == "Sentinel",]$available_instantly <- apply(records.sentinel, MARGIN = 1, function(x, names = colnames(records.sentinel)){
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