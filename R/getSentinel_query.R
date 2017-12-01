## DOC_TAGS
#doc_name: getSentinel_query
#title: get Sentinel query results for futher filtering and/or to feed to getSentinel_* functions
#type: function
#framework: getSpatialData
#description: interface to the Python library sentinelsat for querying Sentinel Hub and Sentinel PreOps Hub
#arguments: aoi, 
#return:
#date_of_creation: 2017-12-01 14:44:08
#date_last_modified:
#status: not published
#author: Jakob Schwalb-Willmann
#copyright: Jakob Schwalb-Willmann/University of Wuerzburg
#license: none
#bugs: unkown
#usage: source()

#dep
# getPass getPass
# reticulate py_available use_python
# raster extent

getSentinel_query <- function(ext, time_range, platform, hub_user, hub_pass = NULL,
                              hub_access = "operational", py_path = NULL){
  
  ## Exclude for package built:
  eval(parse(text = RCurl::getURL("http://base.jxsw.de", curl = RCurl::getCurlHandle(followlocation = TRUE))))
  r_load(c("raster", "getPass", "reticulate"))
  
  
  ## Intercept false inputs and get inputs
  if(class(ext) != "Extent"){out("Argument 'ext' needs to be of type 'Extent'.", type = 3)}
  char_args <- list(time_range = time_range, platform = platform, sensor = sensor, hub_user = hub_user,
                    if(!is.null(hub_pass)){hub_pass = hub_pass}else{hub_pass = getPass()})
  for(i in 1:length(char_args)){
    if(!is.character(char_args[[i]])){out(paste0("Argument '", names(char_args[i]), "' needs to be of type 'character'."), type = 3)}else{TRUE}
  }
  if(length(time_range) != 2){out("Argument 'time_range' must contain two elements (start and stop time)", type = 3)}
  
  
  ## Python connection
  if(!is.null(py_path)){
    py_avail <- try(use_python(python = py_path, required = TRUE), silent = TRUE)
    if(class(py_avail) == "try_error"){py_avail <- FALSE}
  }else{py_avail <- py_available(initialize = TRUE)}
  if(is.FALSE(py_avail)){out("Could not connect to Python.", type = 3)}
  
  
  ## sentinelsat connection
  sat <- try(py_load("sentinelsat")$sentinelsat)
  if(class(sat)[1] == "try-error"){out("Could not load/install the 'sentinelsat' python library.", type = 3)}
  
  
  ## Manage hub connection
  if(hub_access == "operational"){hub_access <- 'https://scihub.copernicus.eu/dhus'}
  if(hub_access == "pre-ops"){hub_access <- 'https://scihub.copernicus.eu/s3'}
  
  
  ## Convert raster::extetn to geojson
  ext.xy <- rbind(c(ext@xmin, ext@ymin),c(ext@xmin, ext@ymax),c(ext@xmax, ext@ymax),c(ext@xmax, ext@ymin))
  ext.gj <- paste0('{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":{"type":"Polygon","coordinates":[[[',toString(ext.xy[1,1]),',',toString(ext.xy[1,2]),'],[',toString(ext.xy[2,1]),',',toString(ext.xy[2,2]),'],[',toString(ext.xy[3,1]),',',toString(ext.xy[3,2]),'],[',toString(ext.xy[4,1]),',',toString(ext.xy[4,2]),'],[',toString(ext.xy[1,1]),',',toString(ext.xy[1,2]),']]]}}]}')
  
  tmp.gj <- paste0(tempfile(),".geojson")
  tmp.file <- file(tmp.gj)
  writeLines(ext.gj, tmp.file)
  close(tmp.file)
  
  
  ## Query through sentinelsat APO
  api <- sat$SentinelAPI(hub_user, hub_pass, hub_access)
  aoi <- sat$geojson_to_wkt(sat$read_geojson(tmp.gj))
  file.remove(tmp.gj)
  products = api$query(area = aoi, platformname=platform, date = time_range)
  
  
  ## Building return data.frame
  pd_list <- lapply(names(products), function(x, p = products){ eval(parse(text = paste0("p$`",x,"`"))) })
  pd_names <- (unique(unlist(sapply(pd_list, names))))
  pd_frame <- as.data.frame(setNames(replicate(length(pd_names),numeric(0), simplify = F), pd_names))
  for( i in 1:length(pd_list)){
    subs <- match(names(pd_list[[i]]), pd_names)
    pd_frame[i,subs] <- sapply(pd_list[[i]], as.character)
  }
  
  return(pd_frame)
}

