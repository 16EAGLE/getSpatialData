#' View or plot previews of records
#' 
#' \code{view_previews} and \code{plot_previews} load and display georeferenced previews acquired using \code{\link{get_previews}}.
#'
#' @inheritParams get_previews
#' @param show_aoi logical, whether the session AOI defined with \link{set_aoi} should be drawn on the map or plot(s) (\code{TRUE}) or not.
#' @param aoi_colour character, colour of the AOI. Ignored, if \code{show_aoi = FALSE}.
#' @param separate logical, whether previews should be displayed separately on a \code{cowplot} grid (\code{TRUE}) or as a mosaic in a single plot (\code{FALSE}, default).
#' @param maxcol numeric, maximum number of columns that the plot grid should have.
#' @param maxpixels numeric, maximum number of pixels to be displayed
#' 
#' @details 
#' \code{view_previews} renders previews as RGB images on an interactive map using \code{mapview} and \code{leaflet}.
#' 
#' \code{plot_previews} plots previews to a graphics device using \code{ggplot2}.
#' 
#' @return A \code{mapview}/\code{ggplot} object
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom raster stack nlayers
#' @importFrom mapview mapview viewRGB
#' 
#' @name view_previews
#' @export
view_previews <- function(records, show_aoi = TRUE, aoi_colour = "deepskyblue", maxpixels = 100000, verbose = TRUE){
  
  # checks
  .check_verbose(verbose)
  records <- .check_records(records, col.names = c("preview_file", "footprint", "record_id"))
  records <- records[!duplicated(records$record_id),]
  
  na.previews <- is.na(records$preview_file)
  if(all(na.previews)) out("Column 'preview_file' does not contain paths to preview files. See get_previews() to get previews.")
  if(any(na.previews)) out(paste0(length(records$record_id[na.previews]), "/", nrow(records), " previews of records are skipped, since there is no preview file available: '", paste0(records$record_id[na.previews], collapse = "', '"), "'"), type = 2)
  records <- records[!na.previews,]
  
  # load raster
  out("Composing preview map...")
  prev <- lapply(records$preview_file, stack)
  map.list <- mapply(x = prev, y = records$record_id, function(x, y){
    if(nlayers(x) == 3) quiet(viewRGB(x, r=1, g=2, b=3, layer.name = y, homebutton = FALSE, maxpixels = maxpixels)) else{
      quiet(mapview(x[[1]], layer.name = y, homebutton = FALSE, legend = TRUE, maxpixels = maxpixels))
    } 
  }, SIMPLIFY = F)
  map <- map.list[[1]]
  if(length(map.list) > 1) for(i in 2:length(map.list)) map <- "+"(map, map.list[[i]])
  
  if(isTRUE(show_aoi)) map <- .add_aoi(map, aoi_colour)
  
  return(map)
}

#' @rdname view_previews
#' @importFrom RStoolbox ggRGB ggR
#' @importFrom ggplot2 coord_sf xlab ylab theme theme_bw geom_sf aes_string scale_colour_identity guides guide_legend scale_x_continuous scale_y_continuous
#' @importFrom patchwork wrap_plots
#' @importFrom raster nlayers
#' @export
plot_previews <- function(records, show_aoi = TRUE, aoi_colour = "deepskyblue", separate = FALSE, maxcol = 3, maxpixels = 100000, verbose = TRUE){
  
  # checks
  .check_verbose(verbose)
  records <- .check_records(records, col.names = c("preview_file", "footprint", "record_id"))
  
  # load raster
  out("Composing preview plot...")
  prev <- lapply(records$preview_file, stack)
  
  # make ggplot2
  layered <- if(separate == T) F else T
  gg.list <- lapply(1:length(prev), function(i){
    if(nlayers(prev[[i]]) == 3) quiet(ggRGB(prev[[i]], r = 1, g = 2, b = 3, geom_raster = T, maxpixels = maxpixels, ggLayer = if(i == 1) F else if(isTRUE(layered)) T else F)) else{
      quiet(ggR(prev[[i]], geom_raster = T, maxpixels = maxpixels, ggLayer = if(i == 1) F else if(isTRUE(layered)) T else F))  
    }
  })
  
  # fun to add coord system
  cf <- function(x, x.crs = crs(prev[[1]])){
    quiet(x + coord_sf(crs = st_crs(x.crs), datum = st_crs(x.crs)) + xlab("Longitude") + ylab("Latitude") + theme_bw()) +
      scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))
  }
  
  # fun to add aoi
  aoi <- function(x){
    aoi.sf <- .check_aoi(aoi = NULL, type = "sf")
    quiet(x + geom_sf(data = st_sf(geometry = aoi.sf, colour = aoi_colour), aes_string(colour = "colour"), fill = NA, show.legend = T) +
            scale_colour_identity(guide = "legend", labels = "AOI", name = ""))
  }
  
  # combine plots
  if(isFALSE(separate)){
    gg.list[[1]] <- cf(gg.list[[1]])
    gg <- gg.list[[1]]
    
    if(length(gg.list) > 1) for(i in 2:length(gg.list)) gg <- "+"(gg, gg.list[[i]])
    if(isTRUE(show_aoi)) gg <- aoi(gg)
    
  } else{
    gg.list <- lapply(gg.list, cf)
    if(isTRUE(show_aoi)){
      gg.list <- lapply(gg.list, function(x) aoi(x))
      #legend <- get_legend(gg.list[[1]])
      gg.nl <- lapply(gg.list, function(x) x + theme(legend.position = "none"))
    }
    
    #gg <- quiet(plot_grid(plotlist = gg.nl, ncol = min(length(prev), maxcol), nrow = ceiling(length(prev)/maxcol)))
    #gg <- plot_grid(gg, legend, ncol = 2, rel_widths = c(9,1))
    gg <- wrap_plots(gg.list, guides = "collect")
  }
  
  for(i in 1:length(gg$layers)) gg$layers[[i]]$geom_params$na.rm <- T
  return(gg)
}

#' @rdname getSpatialData-deprecated
#' @export
getSentinel_preview <- function(records = NULL, ...){
  .Deprecated("view_previews", "getSpatialData", "This function is deprecated. Use get_previews, view_previews and plot_previews to download, view or plot previews for different sensors at once.")
  
  if(missing(records)){
    extras <- list(...)
    records <- extras$record
  }
  
  view_previews(...)
}


#' @rdname getSpatialData-deprecated
#' @export
getLandsat_preview <- getSentinel_preview


#' @rdname getSpatialData-deprecated
#' @export
getMODIS_preview <- getSentinel_preview
