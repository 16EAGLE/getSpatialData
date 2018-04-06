context("gSD_settings")

hub_user = getPass::getPass()
hub_pass = getPass::getPass() # make helper or setup file for this test variables
dir.arc <- tempdir()

test_that("set_login_CopHub", {
  expect_is(x <- set_login_CopHub(hub_user = hub_user, hub_pass = hub_pass), "list")
  expect_true(getOption("gSD.cophub_set"))
  expect_is(hub_user <- getOption("gSD.cophub_user"), "character")
  expect_is(hub_pass <- getOption("gSD.cophub_pass"), "character")
})

test_that("set_archive", {
  expect_is(x <- set_archive(dir.arc), "list")
  expect_true(getOption("gSD.archive_set"))
  expect_is(getOption("gSD.archive"), "character")
})

unlink(dir.arc)

## add set_aoi tests

