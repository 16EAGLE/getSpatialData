#' Something
#'
#'
#'
#' @author Jakob Schwalb-Willmann
#' 
#' @export

check_availability <- function(records){
  
  # create new colunm
  records$available_instantly <- NA
  if("Sentinel" %in% records$product_group){
    records.sentinel <- records[records$product_group == "Sentinel",]
    records.sentinel$cred <- lapply(records.sentinel$product, function(x){
      .CopHub_select(x = "auto", p = x, user = getOption("gSD.dhus_user"), pw = getOption("gSD.dhus_pass"))
    })
    
    records[records$product_group == "Sentinel",]$available_instantly <- apply(records.sentinel, MARGIN = 1, function(x, names = colnames(records.sentinel)){
      as.logical(toupper(unlist(.get_odata(x$entity_id, x$cred, field = "Online/$value"))))
    })
  }
  
  return(records)
}