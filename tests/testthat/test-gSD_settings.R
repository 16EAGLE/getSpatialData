if(gSD_tests_auth){
  context("gSD_login")

  test_that("login_CopHub", {
    expect_is(x <- login_CopHub(username = vars.auth$dhus.user, password = vars.auth$dhus.pass), "list")
    expect_true(getOption("gSD.cophub_set"))
    expect_is(username <- getOption("gSD.cophub_user"), "character")
    expect_is(password <- getOption("gSD.cophub_pass"), "character")
  })

  test_that("login_USGS", {
    expect_is(x <- login_USGS(username = vars.auth$ee.user, password = vars.auth$ee.pass), "list")
    expect_true(getOption("gSD.usgs_set"))
    expect_is(username <- getOption("gSD.usgs_user"), "character")
    expect_is(password <- getOption("gSD.usgs_pass"), "character")
  })
}

context("gSD_settings")

test_that("set_archive", {
  expect_is(x <- set_archive(vars.global$dir.arc), "list")
  expect_true(getOption("gSD.archive_set"))
  expect_is(getOption("gSD.archive"), "character")
})
unlink(vars.global$dir.arc)

