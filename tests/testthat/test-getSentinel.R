if(runAuthTests){
  context("getSentinel_* (dhus)")

  test_that("getSentinel_query (Sentinel-1)", {
    expect_is(records <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.a, username = vars.auth$dhus.user, password = vars.auth$dhus.pass), "data.frame")
    #expect_gt(nrow(records), 0)
  })

  test_that("getSentinel_query (Sentinel-2)", {
    expect_is(records <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.b, username = vars.auth$dhus.user, password = vars.auth$dhus.pass), "data.frame")
    #expect_gt(nrow(records), 0)
  })

  test_that("getSentinel_preview (Sentinel-1)", {
    records <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.a, username = vars.auth$dhus.user, password = vars.auth$dhus.pass)
    expect_null(x <- getSentinel_preview(record = records[1,], username = vars.auth$dhus.user, password = vars.auth$dhus.pass, on_map = F, show_aoi = F))
    expect_is(x <- recordPlot(), "recordedplot")
  })

  test_that("getSentinel_preview (Sentinel-2)", {
    records <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.b, username = vars.auth$dhus.user, password = vars.auth$dhus.pass)
    expect_null(x <- getSentinel_preview(record = records[1,], username = vars.auth$dhus.user, password = vars.auth$dhus.pass, on_map = F, show_aoi = F))
    expect_is(x <- recordPlot(), "recordedplot")
  })
}

context("getSentinel_* (pre-ops)")

test_that("getSentinel_query (Sentinel-3)", {
  expect_is(records <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.c, username = vars.auth$s3.user, password = vars.auth$s3.pass), "data.frame")
  #expect_gt(nrow(records), 0)
})

test_that("getSentinel_preview (Sentinel-3)", {
  records <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platform.c, username = vars.auth$s3.user, password = vars.auth$s3.pass)
  expect_null(x <- getSentinel_preview(record = records[1,], username = vars.auth$s3.user, password = vars.auth$s3.pass, on_map = F, show_aoi = F))
  expect_is(x <- recordPlot(), "recordedplot")
})

# test_that("getSentinel_data: download and checksum dataset", {
#   records <- getSentinel_query(aoi = aoi, time_range = time_range, platform = platform, username = username, password = password)
#   records_filtered <- records[which(records$processinglevel == "Level-1C"),] #filter by Level
#   expect_is(x <- getSentinel_data(records = records_filtered[5,], dir_out = dir.arc, username = username, password = password), "character")
# })

#unlink(vars.global$dir.arc)

