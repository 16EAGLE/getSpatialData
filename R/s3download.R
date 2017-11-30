s3download <- function(dir.out, ext, time_range = list("20171101", "20171130"), 
                       instrumentname = "Ocean Land Colour Instrument", producttype = "OL_1_EFR___",
                       timeliness = "Non Time Critical", skip_dates = "", py_path = NULL){
  
  ## Connect to base
  library(RCurl)
  eval(parse(text = RCurl::getURL("https://raw.githubusercontent.com/16EAGLE/16EAGLE/master/R/base.R", .opts = RCurl::curlOptions(followlocation = TRUE))))
  r_load(c("reticulate", "getPass", "raster"))

  if(!is.null(py_path)){use_python(python = py_path, required = TRUE)}
  
  shub.user = "s3guest"
  pass <- "s3guest" #getPass()
  platform <-  "Sentinel-3"
  shub.url = "https://scihub.copernicus.eu/s3"
  
  ext.xy <- rbind(c(ext@xmin, ext@ymin),c(ext@xmin, ext@ymax),c(ext@xmax, ext@ymax),c(ext@xmax, ext@ymin))
  ext.gj <- paste0('{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":{"type":"Polygon","coordinates":[[[',toString(ext.xy[1,1]),',',toString(ext.xy[1,2]),'],[',toString(ext.xy[2,1]),',',toString(ext.xy[2,2]),'],[',toString(ext.xy[3,1]),',',toString(ext.xy[3,2]),'],[',toString(ext.xy[4,1]),',',toString(ext.xy[4,2]),'],[',toString(ext.xy[1,1]),',',toString(ext.xy[1,2]),']]]}}]}')
  
  tmp.gj <- paste0(tempfile(),".geojson")
  tmp.file <- file(tmp.gj)
  writeLines(ext.gj, tmp.file)
  close(tmp.file)

  reticulate::py_available(initialize = TRUE)
  sat <- py_load("sentinelsat")$sentinelsat
  
  api <- sat$SentinelAPI(shub.user, pass, shub.url)
  footprint <- sat$geojson_to_wkt(sat$read_geojson(tmp.gj))
  products = api$query(area = footprint, platformname=platform, date = time_range)
  
  products.list <- lapply(names(products), function(x, p = products){ eval(parse(text = paste0("p$`",x,"`"))) })
  olci.list <- lapply(products.list, function(x, iname = instrumentname){if(x$instrumentname == iname){return(x)} })
  olci.list <- olci.list[-which(sapply(olci.list, is.null) == TRUE)]
  
  olci.list <- lapply(products.list, function(x, pd = producttype, tl = timeliness){if(x$producttype == pd & x$timeliness == tl){return(x)} })
  olci.list <- olci.list[-which(sapply(olci.list, is.null) == TRUE)]
  dates <- sapply(olci.list, function(x){substr(as.character(x$beginposition), start = 1, stop = 8)})
  if(skip_dates[1] != ""){
    sub <- which(dates != skip_dates)
  }else{ sub <- seq(1, length(dates))}
  uuid <- sapply(olci.list[[sub]], function(x){as.character(x$uuid)})
  
  api$download_all(uuid, dir.out)
}