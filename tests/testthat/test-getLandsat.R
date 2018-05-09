if(runAuthTests){

  context("getLandsat_*")
  test_that("getLandsat_*", {
    expect_is(names <- getLandsat_names(username = vars.auth$ee.user, password = vars.auth$ee.pass), "character")
    expect_is(records <- getLandsat_query(time_range = vars.global$time_range, name = "LANDSAT_8_C1", aoi = vars.global$aoi, username = vars.auth$ee.user, password = vars.auth$ee.pass, verbose = F), "data.frame")
    expect_gt(nrow(records), 0)
    expect_null(x <- getLandsat_preview(record = records[1,], on_map = F, show_aoi = F, verbose = F))
    expect_is(x <- recordPlot(), "recordedplot")
    # if(runDownTests){}
  })
}
