context("getSentinel_*")
for(i in 1:length(vars.sentinel$platforms)){
  # test_that(paste0("getSentinel_* (", vars.sentinel$platforms[i], ")"), {
  #   expect_is(records <- getSentinel_query(aoi = vars.global$aoi , time_range = vars.global$time_range, platform = vars.sentinel$platforms[i],
  #                                          username = vars.sentinel$user[i], password = vars.sentinel$pass[i]), "data.frame")
  #   expect_gt(nrow(records), 0)
  #   expect_null(x <- getSentinel_preview(record = records[1,], username = vars.sentinel$user[i], password = vars.sentinel$pass[i], on_map = F, show_aoi = F))
  #   if(isTRUE(vars.sentinel$expect.prev[i])) expect_is(x <- recordPlot(), "recordedplot")
  #   if(isTRUE(test.run$downloads)) expect_is(x <- getSentinel_data(records[1,], dir_out = vars.global$dir.arc, username = vars.sentinel$user[i], password = vars.sentinel$pass[i], verbose = F), "character")
  # })
}


