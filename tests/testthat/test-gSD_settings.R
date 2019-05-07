context("gSD_login")
test_that("login_CopHub", {
  if(isTRUE(test.run$authentify)){
    expect_null(x <- login_CopHub(username = test.cred$dhus.user, password = test.cred$dhus.pass))
    expect_true(getOption("gSD.dhus_set"))
    expect_is(username <- getOption("gSD.dhus_user"), "character")
    expect_is(password <- getOption("gSD.dhus_pass"), "character")
  }
  expect_error(x <- login_CopHub(username = "", password = "abc"))
})

test_that("login_USGS", {
  if(isTRUE(test.run$authentify)){
    expect_null(x <- login_USGS(username = test.cred$ee.user, password = test.cred$ee.pass))
    expect_true(getOption("gSD.usgs_set"))
    expect_is(username <- getOption("gSD.usgs_user"), "character")
    expect_is(password <- getOption("gSD.usgs_pass"), "character")
  }
  expect_error(x <- login_USGS(username = "", password = "abc"))
})


context("gSD_settings")
test_that("set_archive", {
  expect_is(x <- set_archive(vars.global$dir.arc), "list")
  expect_true(getOption("gSD.archive_set"))
  expect_is(getOption("gSD.archive"), "character")
  expect_is(getOption("gSD.archive_get"), "character")
  expect_is(getOption("gSD.archive_prep"), "character")
})

test_that("set_aoi", {
  expect_silent(set_aoi(aoi = vars.global$aoi))
})

test_that("view_aoi", {
  expect_is(x <- view_aoi(), "mapview")
})

test_that("get_aoi", {
  expect_is(x <- get_aoi(), "sfc_POLYGON")
  expect_is(x <- get_aoi(type = "sp"), "SpatialPolygons")
  expect_is(x <- get_aoi(type = "matrix"), "matrix")
})

test_that("services_avail", {
  expect_null(x <- services_avail(verbose = F))
  expect_is(x <- services_avail(value = T, verbose = F), "data.frame")
})
