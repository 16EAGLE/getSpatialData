#--------------------------------------------------------------------------------------------------------------------------------
#Title: get_MODIS
#Type: function
#Description: Creates MODIS image files from an extent input by buidldin a HD4 MODIS archive and using GDAL to create e. g. TIF files from it
#Date of creation: 2017-07-13
#Author: Jakob Schwalb-Willmann
#Arguments:
#sub:         extent of requestetd images, shape as SpatialPolygonsDataFrame or coordinate matrix (MANDATORY)
#start:       string, starting datetime, default is "2013-06-01"
#end:         string, end datetime, default is "2013-06-30"
#modis_dir:   full path to your MODIS archive directory
#format:      output file format following MODISoptions argument, default is "GTiff"
#crs.quest:   requested CRS, in case that sub is a coordinate matrix
#Bugs: NONE
#appl: SOURCE
#cat: dev

#Getting MODIS data
getMODIS <- function(sub,prod.id = "MOD13Q1", start = "2013-06-01", end = "2013-06-30", modis_dir = "/home/jakob/Dokumente/wd/JMU_EAGLE_MSc/dev/data/arc_down/MODIS",
                      format = "GTiff", crs.quest = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")){
  
  #Requirements
  #For HD4 support, GDAL needs to be installed on your system.
  #Ubuntu: sudo apt-get install gdal-bin
  #Windows: Install GDAL via http://trac.osgeo.org/osgeo4w/
  
  #Load required packages
  t1 <- Sys.time()
  loadp(c("MODIS","raster","rgdal","ggplot2","geosphere"))
  out("Accessing data...")
  
  #Check required argument
  if(missing(sub)){
    out("Argument 'sub' must be specified. Use a coordinate matrix or a 'SpatialPointsDataFrame'.")
  }else{
    if(class(sub)[1] == "SpatialPolygonsDataFrame"){
      sub.spdf <- sub
      sub.tile <- getTile(x = sub.spdf)
    }
    if(class(sub)[1] == "matrix"){
      sub.coords <- sub
      sub.poly <- Polygon(sub.coords)
      sub.sppoly = SpatialPolygons(list(Polygons(list(sub.poly), ID = "a")),
                                   proj4string=crs.quest)
      sub.list <- list(xmin = min(sub.coords[,1]), xmax = max(sub.coords[,1]),
                       ymax = max(sub.coords[,2]), ymin = min(sub.coords[,2]))
      sub.tile <- getTile(x = extent(sub.sppoly)) #sub.list)
    }
    if(class(sub)[1] == "Extent"){
      sub.tile <- getTile(x = sub)
    }
  }
  
  #Define system-wide MODISoptions and working CRS
  dir.arc <- paste0(modis_dir, "/MODIS_ARC"); dir.create(dir.arc,recursive = TRUE,showWarnings = FALSE)
  dir.pro <- paste0(modis_dir, "/PROCESSED"); dir.create(dir.pro,recursive = TRUE,showWarnings = FALSE)
  
  #Define MODISoptions
  MODIS:::checkTools("GDAL")
  MODISoptions(localArcPath = dir.arc,outDirPath = dir.pro, dataFormat = format)
  
  #Add start and stop date
  date.start <- as.POSIXct(strptime(start, "%Y-%m-%d", tz = "UTC"))
  date.end <- as.POSIXct(strptime(end, "%Y-%m-%d", tz = "UTC"))
  dates <- transDate(date.start,date.end)
  doy.beg <- as.numeric(dates$beginDOY); doy.end <- as.numeric(dates$endDOY)
  
  #Use GDAL to download HD4 tile files and convert them as specified in MODISoptions()
  runGdal(product = prod.id,begin = dates$beginDOY, end = dates$endDOY,
          tileH = sub.tile$tileH, tileV = sub.tile$tileV)
  dir.quest <- paste0(dir.pro,"/",list.files(dir.pro)[length(list.files(dir.pro))])
  out("Data processing finished. Check output directory.")
  
  t2 <- Sys.time()
  td <- as.character(round(as.numeric(difftime(t2,t1,units = "mins")),digits = 2))
  
  out(paste0("Processing duration: ",td," minutes."))
  return(list.files(dir.quest))
}
#end
