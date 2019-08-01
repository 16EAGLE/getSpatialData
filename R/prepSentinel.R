#' Prepare Sentinel data to be ready-to-use (beta)
#'
#' \code{prepSentinel} makes downloaded Sentinel datasets ready-to-use by automatically inspecting, extracting, sorting and converting the relevant contents of the datasets to a user-defined format.
#'
#' @inheritParams getSentinel_data
#' @param datasets character vector or list, containing paths to datasets (.zip, as downloaded by \code{getSentinel_data}) that should be prepared.
#' @param dir_out character, full path to target directory. Optional. If not set, \code{prepSentinel} uses the directory to the \code{getSpatialData} archive folder. Use \link{set_archive} to once define a getSpatialData  archive folder.
#' @param format character, output format of the raster datasets (indicated by the file extension, e.g. "tiff" for format GTiff). Default is "vrt".
#' @param select.tiles character, selecting one or multuple specific tiles to prepare. Older datasets archived by ESA contain multiple tiles. If \code{select.tiles} is set to one or multiple specific tiles contained within the selected datasets (e.g. "T32TNT"), only these tiles will be extracted and prepared. By default, all tiles are extracted and prepared. Ignored for datasets that contain only a single tile.
#' @param overwrite logical, whether to overwrite existing files or not.
#'
#' @return Character vector of paths to the prepared files.
#'
#' @author Jakob Schwalb-Willmann
#'
#' @importFrom gdalUtils gdalbuildvrt gdal_translate
#' @importFrom utils unzip
#'
#' @examples
#' ## Load packages
#' library(getSpatialData)
#' library(sf)
#' library(sp)
#'
#' ## Define an AOI (either matrix, sf or sp object)
#' data("aoi_data") # example aoi
#'
#' aoi <- aoi_data[[3]] # AOI as matrix object, or better:
#' aoi <- aoi_data[[2]] # AOI as sp object, or:
#' aoi <- aoi_data[[1]] # AOI as sf object
#'
#' ## set AOI for this session
#' set_aoi(aoi)
#' view_aoi() #view AOI in viewer
#' # or, simply call set_aoi() without argument to interactively draw an AOI
#'
#' ## Define time range and platform
#' time_range <-  c("2017-08-01", "2017-08-30")
#' platform <- "Sentinel-2"
#'
#' ## set login credentials and an archive directory
#' \dontrun{
#' login_CopHub(username = "username") #asks for password or define 'password'
#' set_archive("/path/to/archive/")
#'
#' ## Use getSentinel_records to search for data (using the session AOI)
#' records <- getSentinel_records(time_range = time_range, platform = platform)
#'
#' ## Get an overview of the records
#' View(records) #get an overview about the search records
#' colnames(records) #see all available filter attributes
#' unique(records$processinglevel) #use one of the, e.g. to see available processing levels
#'
#' ## Filter the records
#' records_filtered <- records[which(records$processinglevel == "Level-1C"),] #filter by Level
#'
#' ## Preview a single record
#' getSentinel_preview(record = records_filtered[5,])
#'
#' ## Download some datasets
#' datasets <- getSentinel_data(records = records_filtered[c(4,5,6),])
#'
#' ## Make them ready to use
#' datasets_prep <- prepSentinel(datasets, format = "tiff")
#'
#' ## Load them to R
#' r <- stack(datasets_prep[[1]][[1]][1]) #first dataset, first tile, 10m resoultion
#' }
#'
#' @seealso \link{getSentinel_data}
#' @export

prepSentinel <- function(datasets, dir_out = NULL, format = "vrt", select.tiles = "all", overwrite = F, verbose = T){

  if(is.TRUE(getOption("gSD.archive_set"))){
    if(is.null(dir_out)){dir_out <- paste0(getOption("gSD.archive_prep"), "/SENTINEL/")}
    if(!dir.exists(dir_out)) dir.create(dir_out, recursive = T)
  }

  ## Intercept false inputs and get inputs
  if(!isTRUE(inherits(datasets, "list"))) datasets <- unlist(datasets)

  char_args <- list(datasets = datasets, format = format)
  for(i in 1:length(char_args)) if(!is.character(char_args[[i]])) out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)
  if(!is.character(dir_out)) out(paste0("Argument 'dir_out' needs to be set, if no getSpatialData archive folder was defined. Otherwise, use set_archive() to do so."), type = 3)
  if(!dir.exists(dir_out)) out("The defined output directory does not exist.", type=3)
  if(inherits(verbose, "logical")) options(gSD.verbose = verbose)

  prepare <- function(dataset, do = dir_out, fo = format, st = select.tiles, owrt = overwrite){

    #look into archives
    out(paste0("Preparing '", dataset, "'..."), msg = T)
    zip.files <- unzip(dataset, list = T)$Name
    f.jp2 <- grep("IMG_DATA", grep(".jp2", zip.files, value = T), value = T)
    f.jp2.split <- lapply(f.jp2, function(x) strsplit(x, "/")[[1]])

    #get granuels
    granules <- unique(sapply(f.jp2.split, function(x) x[grep("GRAN", x)+1]))

    #get tiles
    tiles <- sapply(granules, function(x){
      y <- strsplit(x, "_")[[1]]
      grep("T", y[which(nchar(y) == 6)], value = T)
    }, USE.NAMES = F)
    if(st[1] != "all") tiles <- sapply(st, function(x, y = tiles) grep(x, y, value = T), USE.NAMES = F)
    tiles.use <- which(sapply(tiles, nchar, USE.NAMES = F) > 0)
    if(length(tiles.use) == 0) out("Selected tiles are not contained within dataset.", type = 3) else{
      tiles <- unlist(tiles[tiles.use])
      granules <- unlist(granules[tiles.use])
    }

    #group per tile
    f.jp2.tile <- lapply(tiles, function(x, y = f.jp2) grep(x, y, value = T))

    #extract tilewise
    out("Extracting files...")
    dir.extr <- paste0(gsub(".zip", "", dataset), "/", granules)
    catch <- sapply(dir.extr, function(x) if(!dir.exists(x)) dir.create(x, recursive = T))
    files.jp2 <- mapply(x = f.jp2.tile, y = dir.extr, function(x, y, z = dataset, ow = owrt){
      suppressWarnings(unzip(zipfile = dataset, exdir = y, files = x, overwrite = ow))
      paste0(y, "/", x)
    }, SIMPLIFY = F)

    #get MTD and level
    mtd.file <- grep("DATASTRIP", grep("GRANULE", grep("MTD", grep(".xml", zip.files, value = T), value = T), invert = T, value = T), invert = T, value = T)
    dir.mtd <- gsub(".zip", "", dataset)
    mtd.file <- suppressWarnings(unzip(zipfile = dataset, exdir = dir.mtd, files = mtd.file, overwrite = owrt, junkpaths = T))
    mtd <- readLines(mtd.file)
    level <- tolower(strsplit(strsplit(grep("PROCESSING_LEVEL", mtd, value = T), "<")[[1]][2], ">")[[1]][2])

    #sort by bands
    if(length(grep("1c", level)) > 0){
      band.order <- list("10m" = list("refl" = c("B02", "B03", "B04", "B08")),
                         "20m" = list("refl" = c("B05", "B06", "B07", "B8A", "B11", "B12")),
                         "60m" = list("refl" = c("B01", "B09", "B10")))
      bands.jp2 <- lapply(files.jp2, function(x, b = band.order) lapply(b, function(r, y = x) lapply(r, function(r2, z = y) sapply(r2, function(band, a = z) grep(band, a, value = T), USE.NAMES = F))))
    }

    if(length(grep("2a", level)) > 0){
      band.order <- list("aux" = c("AOT", "CLD", "SCL", "TCI", "WVP", "SNW"),
                         "refl" = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B09", "B10", "B11", "B12"))
      band.res <- c("_10m", "_20m", "_60m")
      names(band.res) <- c("10m", "20m", "60m")
      band.names <- lapply(band.res, function(x, ro = band.order) lapply(ro, function(ro2, x2 = x) paste0(ro2, x2)))

      bands.jp2 <- lapply(files.jp2, function(f1, bn1 = band.names) lapply(bn1, function(bn2, f2 = f1) lapply(bn2, function(bn3, f3 = f2) sapply(bn3, function(x, f4 = f3) grep(x, f4, value = T), USE.NAMES = F))))
      bands.jp2 <- lapply(bands.jp2, function(bn1) lapply(bn1, function(x) lapply(x, function(y) y[sapply(y, length, USE.NAMES = F) > 0])))
    }

    #prep
    out("Sorting and stacking bands...")
    dir.prep <- paste0(dir_out, "/", granules)
    catch <- sapply(dir.prep, function(x) if(!dir.exists(x)) dir.create(x, recursive = T))

    vrt <- mapply(bjp2 = bands.jp2, dprep = dir.prep, fjp2 = files.jp2, function(bjp2, dprep, fjp2) lapply(names(bjp2), function(x, y = names(bjp2[[1]]), dp = dprep, f = fjp2, b = bjp2) paste0(dp, "/", gsub(".SAFE", "", tail(strsplit(f[1], "/")[[1]], n=6)[1]), "_", x, "_", y, ".vrt")), SIMPLIFY = F)
    catch <- mapply(bjp2 = bands.jp2, v = vrt, function(bjp2, v, owr = owrt) mapply(x = bjp2, y = v, FUN = function(x, y, ow = owr){
      mapply(x2 = x, y2 = y, FUN = function(x2, y2, ow2 = ow){
        if(!isTRUE(ow2) & !file.exists(y2)) gdalbuildvrt(x2, y2, resolution = "highest", separate = T)
      }, SIMPLIFY = F)
    }), SIMPLIFY = F)

    txt <- lapply(vrt, function(v) lapply(v, function(x) gsub(".vrt", "_ORIGIN.txt", x)))
    catch <- mapply(bjp2 = bands.jp2, t = txt, function(bjp2, t, owr = owrt) mapply(x = bjp2, y = t, FUN = function(x, y, ow = owr){
      mapply(x2 = x, y2 = y, FUN = function(x2, y2, ow2 = ow){
        if(!isTRUE(ow2) & !file.exists(y2)){
          file.txt <- file(y2)
          #writeLines(sapply(x2, function(x3) tail(strsplit(x3, "/")[[1]], n=1), USE.NAMES = F), file.txt)
          writeLines(unlist(x2), file.txt)
          close(file.txt)
        }
      }, SIMPLIFY = F)
    }), SIMPLIFY = F)

    if(fo != "vrt"){
      out(paste0("Converting datasets to '", fo, "' files..."))
      dst <- lapply(vrt, function(v, fo1 = fo) lapply(v, function(x, fo2 = fo1) gsub(".vrt", paste0(".", fo2), x)))
      catch <- mapply(x = unlist(vrt), y = unlist(dst), function(x, y, ow = owrt) if(!isTRUE(ow) & !file.exists(y)) gdal_translate(x, y), SIMPLIFY = F)
    } else{
      dst <- vrt
    }
    dst <- lapply(dst, function(x) unlist(x, recursive = F))
    names(dst) <- tiles
    return(dst)
  }

  #assemble return
  dst <- lapply(datasets, function(x) prepare(x))
  names(dst) <- sapply(datasets, function(x) tail(strsplit(gsub(".zip", "", x), "/")[[1]], n=1), USE.NAMES = F)
  return(dst)

}
