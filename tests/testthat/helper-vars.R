data("aoi_data")

#if(isTRUE(paste0(unname(Sys.info()["user"]), "_", unname(Sys.info()["nodename"])) == "jakob_JMBPlinux")){
#  Sys.setenv("gSD_user" =  getPass::getPass("Enter username: "))
#  Sys.setenv("gSD_pass" =  getPass::getPass("Enter password: "))
#}

vars.auth <- list(dhus.user = Sys.getenv("gSD_user"),
                  dhus.pass = Sys.getenv("gSD_pass"),
                  s3.user = "s3guest",
                  s3.pass = "s3guest",
                  ee.user = Sys.getenv("gSD_user"),
                  ee.pass = Sys.getenv("gSD_pass"),
                  espa.user = Sys.getenv("gSD_user"),
                  espa.pass = Sys.getenv("gSD_pass"))

if(vars.auth$dhus.user != "") runAuthTests <- TRUE else runAuthTests <- FALSE

#if(vars.auth$dhus.user != "") Sys.setenv("gSD_tests_auth" = "yes")
#if(Sys.getenv("gSD_tests_auth") == "yes") runAuthTests <- TRUE else runAuthTests <- FALSE

vars.global <-   list(dir.arc = tempdir(),
                      aoi = aoi_data[[1]],
                      time_range = c("2017-08-01", "2017-08-30"))

vars.sentinel <- list(platform.a = "Sentinel-1",
                      platform.b = "Sentinel-2",
                      platform.c = "Sentinel-3")

