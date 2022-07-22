# test AOI
data("aoi_data")

# which tests
do <- list(
  downloads = if(Sys.getenv("gSD_downtests") == "yes") TRUE else FALSE
)

# logins
if(Sys.getenv("gSD_user") != ""){
  auth <- TRUE
  try(login_CopHub(Sys.getenv("gSD_user"), Sys.getenv("gSD_pass")))
  try(login_USGS(Sys.getenv("gSD_user"), Sys.getenv("gSD_pass")))
}else{
  auth <- FALSE
}

# update offline product list?
if(all(Sys.getenv("gSD_updprod") == "yes", auth)){
  .prod.list <- get_products(update_online = T, grouped = T)
  usethis::use_data(.prod.list, internal = TRUE, overwrite = T)
  message("Build and install getSpatialData ater tests finished to include updated product list in future runs.")
  products <- .prod.list
} else{
  products <- try(get_products(grouped = T, update_online = F))
}

# vars
vars <- list(
  dir.arc = tempdir(),
  prods =  do.call(rbind, lapply(names(products), function(group){
    data.frame(
      product = products[[group]],
      group = group
    )
  }))
)
vars$prods$aoi <- list(aoi_data[[1]])
vars$prods$time_range <- list(c("2019-03-01", "2019-03-30"))
vars$prods$time_range[grepl("landsat_tm", vars$prods$product) | grepl("landsat_etm", vars$prods$product) | grepl("landsat_mss", vars$prods$product)] <- list(c("2010-03-01", "2010-03-30"))
