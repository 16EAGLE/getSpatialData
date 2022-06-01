# test AOI
data("aoi_data")

# which tests
do <- list(
  downloads = if(Sys.getenv("gSD_downtests") == "yes") TRUE else FALSE
)

# vars
products <- try(get_products(grouped = T, update_online = F))
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


# logins
if(Sys.getenv("gSD_user") != ""){
  auth <- TRUE
  try(login_CopHub(Sys.getenv("gSD_user"), Sys.getenv("gSD_pass")))
  try(login_USGS(Sys.getenv("gSD_user"), Sys.getenv("gSD_pass")))
}else{
  auth <- FALSE
}