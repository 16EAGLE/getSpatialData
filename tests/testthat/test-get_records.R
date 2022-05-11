context("get_records product tests")

# sentinel 
prods <- vars$prods[vars$prods$group == "sentinel",]
prods <- prods[!grepl("gnss", prods$product),]
for(i in nrow(prods)){
  test_that(paste0("get_records() for product group 'sentinel', product '", prods$product[i], "'"),{
    
    # correct case
    records <- expect_output(get_records(
      time_range = prods$time_range[[i]],
      products = prods$product[i],
      aoi = prods$aoi[[i]],
      as_sf = T,
      rename_cols = T,
      check_products = T,
      simplify_cols = T
    ))
    expect_is(records, "sf")
    expect_is(records, "data.frame")
  })
}

# sentinel gnss
# prods <- vars$prods[vars$prods$group == "sentinel",]
# prods <- prods[grepl("gnss", prods$product)]
# for(i in nrow(prods)){
#   test_that(paste0("get_records() for product group 'sentinel', product '", prods$product[i], "'"),{
#     
#     # correct case
#     records <- expect_warning(expect_output(get_records(
#       time_range = prods$time_range[[i]],
#       products = prods$product[i],
#       aoi = prods$aoi[[i]],
#       as_sf = T,
#       rename_cols = T,
#       check_products = T,
#       simplify_cols = T
#     )))
#     expect_is(records, "data.frame")
#   })
# }

context("get_records generic tests")
# fail tests
i = which(vars$prods$product == "sentinel-2")

test_that("get_records() with future time",{
  records <- expect_message(expect_output(get_records(
    time_range = as.character(c(as.Date(Sys.time())+30, as.Date(Sys.time())+60)),
    products = vars$prods$product[i],
    aoi = vars$prods$aoi[[i]],
    as_sf = T,
    rename_cols = T,
    check_products = T,
    simplify_cols = T
  )), regexp = "No results could be obtained for this product, time range and AOI.")
})

test_that("get_records() with wrong class for time_range",{
  records <- expect_error(get_records(
    time_range = 123,
    products = vars$prods$product[i],
    aoi = vars$prods$aoi[[i]],
    as_sf = T,
    rename_cols = T,
    check_products = T,
    simplify_cols = T
  ))
})

test_that("get_records() with invalid product",{
  records <- expect_error(get_records(
    time_range = vars$prods$time_range[[i]],
    products = "abcdefg",
    aoi = vars$prods$aoi[[i]],
    as_sf = T,
    rename_cols = T,
    check_products = T,
    simplify_cols = T
  ))
})

test_that("get_records() with as_sf=FALSE",{
  records <- expect_is(expect_output(get_records(
    time_range = vars$prods$time_range[[i]],
    products = vars$prods$product[i],
    aoi = vars$prods$aoi[[i]],
    as_sf = F,
    rename_cols = T,
    check_products = T,
    simplify_cols = T
  )), "data.frame")
})

test_that("get_records() with rename_cols=FALSE",{
  records <- expect_is(expect_output(get_records(
    time_range = vars$prods$time_range[[i]],
    products = vars$prods$product[i],
    aoi = vars$prods$aoi[[i]],
    as_sf = T,
    rename_cols = F,
    check_products = T,
    simplify_cols = T
  )), "sf")
})

test_that("get_records() with check_products=FALSE",{
  records <- expect_is(expect_output(get_records(
    time_range = vars$prods$time_range[[i]],
    products = vars$prods$product[i],
    aoi = vars$prods$aoi[[i]],
    as_sf = T,
    rename_cols = T,
    check_products = F,
    simplify_cols = T
  )), "sf")
})

test_that("get_records() with simplify_cols=FALSE",{
  records <- expect_is(expect_output(get_records(
    time_range = vars$prods$time_range[[i]],
    products = vars$prods$product[i],
    aoi = vars$prods$aoi[[i]],
    as_sf = T,
    rename_cols = T,
    check_products = T,
    simplify_cols = F
  )), "sf")
})