context("gSD_settings")

username = getPass::getPass("Enter username: ")
password = getPass::getPass("Enter password: ") # make helper or setup file for this test variables
dir.arc <- tempdir()

test_that("login_CopHub", {
  expect_is(x <- login_CopHub(username = username, password = password), "list")
  expect_true(getOption("gSD.cophub_set"))
  expect_is(username <- getOption("gSD.cophub_user"), "character")
  expect_is(password <- getOption("gSD.cophub_pass"), "character")
})

test_that("set_archive", {
  expect_is(x <- set_archive(dir.arc), "list")
  expect_true(getOption("gSD.archive_set"))
  expect_is(getOption("gSD.archive"), "character")
})

unlink(dir.arc)

## add set_aoi tests

