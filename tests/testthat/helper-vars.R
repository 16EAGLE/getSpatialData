# ------------------------------------------------------------------------------
# test directories
testthat_home <- getwd()
testthat_resources <- file.path(testthat_home, "resources")
dir_error <- "Cannot run tests because directory not found: "
if (!dir.exists(testthat_home)) stop(paste0(dir_error, testthat_home))
if (!dir.exists(testthat_resources)) stop(paste0(dir_error, testthat_resources))
# ------------------------------------------------------------------------------


data("aoi_data")

test.cred <- list(dhus.user = Sys.getenv("gSD_user"),
                  dhus.pass = Sys.getenv("gSD_pass"),
                  s5p.user = "s5pguest",
                  s5p.pass = "s5pguest",
                  gnss.user = "gnssguest",
                  gnss.pass = "gnssguest",
                  ee.user = Sys.getenv("gSD_user"),
                  ee.pass = Sys.getenv("gSD_pass"),
                  espa.user = Sys.getenv("gSD_user"),
                  espa.pass = Sys.getenv("gSD_pass"))

test.run <- list(authentify = if(test.cred$dhus.user == "") FALSE else TRUE, 
                 downloads = if(Sys.getenv("gSD_downtests") == "yes") TRUE else FALSE)

#if(vars.auth$dhus.user != "") runAuthTests <- TRUE else runAuthTests <- FALSE
#if(Sys.getenv("gSD_downtests") == "yes") runDownTests <- TRUE else runDownTests <- FALSE

vars.global <- list(dir.arc = tempdir(),
                    aoi = aoi_data[[1]],
                    time_range = c("2019-03-01", "2019-03-30")) #c("2017-08-01", "2017-08-30"))

vars.sentinel <- data.frame(platforms = c("Sentinel-1", "Sentinel-2", "Sentinel-3", "Sentinel-5P"),
                            expect.prev = c(T, T, T, F), stringsAsFactors = F,
                            user = c(test.cred$dhus.user, test.cred$dhus.user, test.cred$dhus.user, test.cred$s5p.user),
                            pass = c(test.cred$dhus.pass, test.cred$dhus.pass, test.cred$dhus.pass, test.cred$s5p.pass))
if(isFALSE(test.run$authentify)) vars.sentinel <- vars.sentinel[-c(1:2),]


# ---------------------------------------
# Variables for calc_cloudcov




