if(runAuthTests){

  context("getMODIS_*")
  test_that("getMODIS_*", {
    expect_is(names <- getMODIS_names(username = vars.auth$ee.user, password = vars.auth$ee.pass), "character")
    expect_is(records <- getMODIS_query(time_range = vars.global$time_range, name = grep("MOD13Q1", names, value = T), aoi = vars.global$aoi, username = vars.auth$ee.user, password = vars.auth$ee.pass, verbose = F), "data.frame")
    expect_gt(nrow(records), 0)
    expect_null(x <- getMODIS_preview(record = records[1,], on_map = F, show_aoi = F, verbose = F))
    expect_is(x <- recordPlot(), "recordedplot")
    if(runDownTests){
      expect_is(down.file <- getMODIS_data(records = records[1,], dir_out = vars.global$dir.arc, force = T, verbose = F), "character")
      expect_gt(nchar(down.file), 0)
    }
  })
}
