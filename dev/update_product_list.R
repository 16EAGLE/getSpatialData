# script updates internal offline product list before build
library(getSpatialData)
.prod.list <- get_products(update_online = T, grouped = T)
usethis::use_data(.prod.list, internal = TRUE, overwrite = T)
