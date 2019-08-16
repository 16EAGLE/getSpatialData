#' View footprints of records
#' 
#' \code{view_records} displays the footsprint of each record on a map.
#'
#' @inheritParams view_previews
#' 
#' @return None or a \code{mapview}/\code{plot} object
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom mapview mapview viewRGB
#' 
#' @name view_records
#' @export
view_records <- function(records, show_aoi = TRUE, aoi_colour = "deepskyblue", value = FALSE, verbose = TRUE){
  
  # checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  records <- .check_records(records, col.names = "footprint")
  records <- records[!duplicated(records$record_id),]
  
  # load raster
  out("Composing records map...")
  map.list <- mapply(x = records$footprint, y = records$record_id, function(x, y) mapview(x, layer.name = y, label = y, homebutton = FALSE), SIMPLIFY = F)
  map <- map.list[[1]]
  for(i in 2:length(map.list)) map <- "+"(map, map.list[[i]])
  
  # add AOI
  if(isTRUE(show_aoi)){
    if(isFALSE(getOption("gSD.aoi_set"))){
      out("Records are previewed without AOI, since no AOI has been set yet (use 'set_aoi()' to define an AOI).", type = 2)
    } else{
      aoi.sf <- getOption("gSD.aoi")
      map <- map + mapview(aoi.sf, layer.name = "AOI", label = "AOI", lwd = 6, color = aoi_colour, fill = F, legend = F, homebutton = FALSE)
    }
  }
  
  # display
  print(quiet(map))
  if(isTRUE(value)) return(map)
}