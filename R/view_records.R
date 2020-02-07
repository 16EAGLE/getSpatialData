#' View footprints of records
#' 
#' \code{view_records} displays the footsprint of each record on a map.
#'
#' @inheritParams view_previews
#' @param line_colours character, colours for each footprint's outline. Vector must be of same length as number of records (\code{nrow(records)}).
#' @param fill_colours character, colours of each footprint's fill. Vector must be of same length as number of records (\code{nrow(records)})
#' @param fill_alpha numeric, alpha value for \code{fill_colours}. Value must be between 0 (transparent) and 1 (opaque).
#' 
#' @return None or a \code{mapview}/\code{plot} object
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom mapview mapview viewRGB
#' 
#' @name view_records
#' @export
view_records <- function(records, show_aoi = TRUE, line_colours = hcl.colors(nrow(records)), fill_colours = hcl.colors(nrow(records)), fill_alpha = 0.3, aoi_colour = "deepskyblue", verbose = TRUE){
  
  # checks
  .check_verbose(verbose)
  records <- .check_records(records, col.names = "footprint")
  records <- records[!duplicated(records$record_id),]
  
  # adjust number of colours to nrow(records)
  .length_colours <- function(col){
    if(length(col) < nrow(records)){
      c(col, rep(tail(col, n = 1), nrow(records)-length(col)))
    } else if(length(col) > nrow(records)){
      col[1:nrow(records)]
    } else col
  }
  line_colours <- .length_colours(line_colours)
  fill_colours <- .length_colours(fill_colours)
  
  # create map object
  out("Composing records map...")
  map.list <- lapply(1:nrow(records), function(i) mapview(records$footprint[i], layer.name = records$record_id[i], label = records$record_id[i],
                                                          homebutton = FALSE, color = line_colours[i], col.regions = fill_colours[i], alpha.regions = fill_alpha))
  map <- map.list[[1]]
  if(length(map.list) > 1) for(i in 2:length(map.list)) map <- "+"(map, map.list[[i]])
  
  # add aoi
  if(isTRUE(show_aoi)) map <- .add_aoi(map, aoi_colour)
  return(map)
}

#' @rdname view_records
#' @importFrom ggplot2 coord_sf xlab ylab theme theme_bw geom_sf aes_string scale_colour_identity scale_fill_identity
#' @export
plot_records <- function(records, show_aoi = TRUE, line_colours = hcl.colors(nrow(records)), fill_colours = hcl.colors(nrow(records)), fill_alpha = 0.3, aoi_colour = "deepskyblue", value = FALSE, verbose = TRUE){
  
  # checks
  .check_verbose(verbose)
  records <- .check_records(records, col.names = "footprint")
  records <- records[!duplicated(records$record_id),]
  
  # checking number of colours
  if(nrow(records) != length(unique(line_colours))){
    out("The number of unique colours in 'line_colours' must be eqaul to nrow(records). Using default colours instead.", type = 3)
    line_colours <- hcl.colors(nrow(records))
  }
  if(nrow(records) != length(unique(fill_colours))){
    out("The number of unique colours in 'fill_colours' must be eqaul to nrow(records). Using default colours instead.", type = 2)
    fill_colours <- hcl.colors(nrow(records))
  }
  out("Composing preview plot...")
  
  # make ggplot2 object
  records$line <- line_colours
  records$fill <- fill_colours
  gg <- ggplot(records) + geom_sf(aes_string(fill = "fill", colour = "line"), alpha = fill_alpha) + xlab("Longitude") + ylab("Latitude") +
    theme_bw() + scale_fill_identity(guide = "none")
  
  # add aoi
  if(isTRUE(show_aoi)){
    aoi.sf <- .check_aoi(aoi = NULL, type = "sf")
    gg <- gg + geom_sf(data = st_sf(geometry = aoi.sf, colour = aoi_colour), aes_string(colour = "colour"), fill = NA, show.legend = T)
  }
  
  gg <- gg + scale_colour_identity(guide = "legend", labels = c(records$record_id,  if(isTRUE(show_aoi)) "AOI" else NULL), name = "Records")
  return(gg)
}