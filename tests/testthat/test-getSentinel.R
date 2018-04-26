context("getSentinel_*")

test_that("getSentinel_query (Sentinel-1)", {
  expect_is(products <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.a, username = vars.global$username, password = vars.global$password), "data.frame")
  #expect_gt(nrow(products), 0)
})

test_that("getSentinel_query (Sentinel-2)", {
  expect_is(products <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.b, username = vars.global$username, password = vars.global$password), "data.frame")
  #expect_gt(nrow(products), 0)
})

test_that("getSentinel_query (Sentinel-3)", {
  expect_is(products <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.c, username = vars.global$username, password = vars.global$password), "data.frame")
  #expect_gt(nrow(products), 0)
})

test_that("getSentinel_preview (Sentinel-1)", {
  products <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.a, username = vars.global$username, password = vars.global$password)
  expect_null(x <- getSentinel_preview(product = products[1,], username = vars.global$username, password = vars.global$password, on_map = F, show_aoi = F))
  expect_is(x <- recordPlot(), "recordedplot")
})

test_that("getSentinel_preview (Sentinel-2)", {
  products <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.b, username = vars.global$username, password = vars.global$password)
  expect_null(x <- getSentinel_preview(product = products[1,], username = vars.global$username, password = vars.global$password, on_map = F, show_aoi = F))
  expect_is(x <- recordPlot(), "recordedplot")
})

test_that("getSentinel_preview (Sentinel-3)", {
  products <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.c, username = vars.global$username, password = vars.global$password)
  expect_null(x <- getSentinel_preview(product = products[1,], username = vars.global$username, password = vars.global$password, on_map = F, show_aoi = F))
  expect_is(x <- recordPlot(), "recordedplot")
})

# test_that("getSentinel_data: download and checksum dataset", {
#   products <- getSentinel_query(aoi = aoi, time_range = time_range, platform = platform, username = username, password = password)
#   products_filtered <- products[which(products$processinglevel == "Level-1C"),] #filter by Level
#   expect_is(x <- getSentinel_data(products = products_filtered[5,], dir_out = dir.arc, username = username, password = password), "character")
# })

#unlink(vars.global$dir.arc)
