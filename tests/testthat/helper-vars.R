# test AOI
data("aoi_data")

# which tests
do <- list(
  downloads = if(Sys.getenv("gSD_downtests") == "yes") TRUE else FALSE
)

# vars
products <- try(get_products(grouped = T))
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

# logins
try(login_CopHub(Sys.getenv("gSD_user"), Sys.getenv("gSD_pass")))
try(login_USGS(Sys.getenv("gSD_user"), Sys.getenv("gSD_pass")))
