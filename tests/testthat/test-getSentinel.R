context("getSentinel_*")

username = getPass::getPass("Enter username: ")
password = getPass::getPass("Enter password: ") # make helper or setup file for this test variables
dir.arc <- tempdir()

data("aoi_data") # example aoi
aoi <- aoi_data[[1]]
time_range <-  c("2017-08-01", "2017-08-30")
platform <- "Sentinel-2"


test_that("getSentinel_query: query Copernicus Hub", {
  expect_is(products <- getSentinel_query(aoi = aoi, time_range = time_range, platform = platform, username = username, password = password), "data.frame")
})

test_that("getSentinel_preview: plot preview", {
  products <- getSentinel_query(aoi = aoi, time_range = time_range, platform = platform, username = username, password = password)
  expect_null(x <- getSentinel_preview(product = products[1,], username = username, password = password))
  expect_is(x <- recordPlot(), "recordedplot")
})

# test_that("getSentinel_data: download and checksum dataset", {
#   products <- getSentinel_query(aoi = aoi, time_range = time_range, platform = platform, username = username, password = password)
#   products_filtered <- products[which(products$processinglevel == "Level-1C"),] #filter by Level
#   expect_is(x <- getSentinel_data(products = products_filtered[5,], dir_out = dir.arc, username = username, password = password), "character")
# })

unlink(dir.arc)

