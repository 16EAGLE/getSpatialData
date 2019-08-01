#' View or plot previews of records
#' 
#' \code{view_previews} and \code{plot_previews} load and display georeferenced previews acquired using \code{\link{get_previews}}.
#'
#' @inheritParams get_previews
#' @param show_aoi logical, whether the session AOI defined with \link{set_aoi} should be drawn on the map or plot(s) (\code{TRUE}) or not.
#' @param aoi_colour character, colour of the AOI. Ignored, if \code{show_aoi = FALSE}.
#' @param separate logical, whether previews should be displayed separately on a \code{cowplot} grid (\code{TRUE}) or as a mosaic in a single plot (\code{FALSE}, default).
#' @param maxcol numeric, maximum number of columns that the plot grid should have.
#' @param value logical, whether to return the displayed map/plot object (\code{TRUE}) or not.
#' 
#' @details 
#' \code{view_previews} renders previews as RGB images on an interactive map using \code{mapview} and \code{leaflet}.
#' 
#' \code{plot_previews} plots previews to a graphics device using \code{ggplot2}.
#' 
#' @return None or a \code{mapview}/\code{plot} object
#' 
#' @author Jakob Schwalb-Willmann
#' 
#' @importFrom raster stack
#' @importFrom mapview mapview viewRGB
#' 
#' @name view_previews
#' @export
view_previews <- function(records, show_aoi = TRUE, aoi_colour = "deepskyblue", value = FALSE, verbose = TRUE){
  
  # checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  .check_records(records, col.names = "preview_file")
  
  na.previews <- is.na(records$preview_file)
  if(all(na.previews)) out("Column 'preview_file' does not contain paths to preview files. See get_previews() to get previews.")
  if(any(na.previews)) out(paste0("Previes of records '", paste0(records$record_id[na.previews], collapse = "', '"), "' are skipped, since there is no preview file is available."), type = 2)
  records <- records[!na.previews,]
  
  # load raster
  out("Composing preview map...")
  prev <- lapply(records$preview_file, stack)
  
  map.list <- mapply(x = prev, y = records$record_id, function(x, y) viewRGB(x, r=1, g=2, b=3, layer.name = y), SIMPLIFY = F)
  map <- map.list[[1]]
  for(i in 2:length(map.list)) map <- "+"(map, map.list[[i]])
  
  # add AOI
  if(isTRUE(show_aoi)){
    if(isFALSE(getOption("gSD.aoi_set"))){
      out("Records are previewed without AOI, since no AOI has been set yet (use 'set_aoi()' to define an AOI).", type = 2)
    } else{
      aoi.sf <- getOption("gSD.aoi")
      map <- map + mapview(aoi.sf, layer.name = "AOI", label = "AOI", lwd = 6, color = aoi_colour, fill = F, legend = F)
    }
  }
  
  # display
  print(quiet(map))
  if(isTRUE(value)) return(map)
}

#' @rdname view_previews
#' @importFrom RStoolbox ggRGB
#' @importFrom ggplot2 coord_sf xlab ylab theme theme_bw geom_sf aes scale_colour_identity guides guide_legend scale_x_continuous scale_y_continuous
#' @importFrom cowplot plot_grid get_legend
#' @export
plot_previews <- function(records, show_aoi = TRUE, aoi_colour = "deepskyblue", separate = FALSE, maxcol = 3, value = FALSE, verbose = TRUE){
  
  # checks
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)
  .check_records(records, col.names = "preview_file")
  
  # load raster
  out("Composing preview plot...")
  prev <- lapply(records$preview_file, stack)
  
  # make ggplot2
  layered <- if(separate == T) F else T
  gg.list <- lapply(1:length(prev), function(i){
    y <- quiet(ggRGB(prev[[i]], r = 1, g = 2, b = 3, geom_raster = T, ggLayer = if(i == 1) F else if(isTRUE(layered)) T else F))
  })
  
  # fun to add coord system
  cf <- function(x, x.crs = crs(prev[[1]])){
    quiet(x + coord_sf(crs = st_crs(x.crs), datum = st_crs(x.crs)) + xlab("Longitude") + ylab("Latitude") + theme_bw()) +
      scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))
  }
  
  # fun to add aoi
  aoi <- function(x){
    aoi.sf <- getOption("gSD.aoi")
    quiet(x + geom_sf(data = st_sf(geometry = aoi.sf, colour = aoi_colour), aes(colour = colour), fill = NA, show.legend = T) +
            scale_colour_identity(guide = "legend", labels = "AOI", name = ""))
  }
  
  # combine plots
  if(isFALSE(separate)){
    gg.list[[1]] <- cf(gg.list[[1]])
    gg <- gg.list[[1]]
    
    for(i in 2:length(gg.list)) gg <- "+"(gg, gg.list[[i]])
    if(isTRUE(show_aoi)) gg <- aoi(gg)
    
  } else{
    gg.list <- lapply(gg.list, cf)
    if(isTRUE(show_aoi)){
      gg.list <- lapply(gg.list, function(x) aoi(x))
      legend <- get_legend(gg.list[[1]])
      gg.nl <- lapply(gg.list, function(x) x + theme(legend.position = "none"))
    }
    gg <- quiet(plot_grid(plotlist = gg.nl, ncol = min(length(prev), maxcol), nrow = ceiling(length(prev)/maxcol)))
    gg <- plot_grid(gg, legend, ncol = 2, rel_widths = c(9,1))
  }
  
  for(i in 1:length(gg$layers)) gg$layers[[i]]$geom_params$na.rm <- T
  print(gg)
  if(isTRUE(value)) return(gg)
}

#' @rdname getSpatialData-deprecated
#' @export
getSentinel_preview <- function(...){
  
  .Deprecated("view_previews", "getSpatialData", "This function is deprecated. Use get_previews, view_previews and plot_previews to download, view or plot previews for different sensors at once.")
  extras <- list(...)
  if(!is.null(extras$record)){
    records <- extras$record
    extras$record <- NULL
  }
  if(!is.null(extras$records)){
    records <- extras$records
    extras$records <- NULL
  }
  
  view_previews(records, ...)
}


#' @rdname getSpatialData-deprecated
#' @export
getLandsat_preview <- getSentinel_preview


#' @rdname getSpatialData-deprecated
#' @export
getMODIS_preview <- getSentinel_preview
