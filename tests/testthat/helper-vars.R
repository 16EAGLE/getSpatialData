data("aoi_data")
vars.global <-   list(username = "", #getPass::getPass("Enter username: "),
                      password = "", #getPass::getPass("Enter password: "),
                      dir.arc = tempdir(),
                      aoi = aoi_data[[1]],
                      time_range = c("2017-08-01", "2017-08-30"))


vars.sentinel <- list(platform.a = "Sentinel-1",
                      platform.b = "Sentinel-2",
                      platform.c = "Sentinel-3")
