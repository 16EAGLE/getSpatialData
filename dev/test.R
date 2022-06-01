# authentificate
Sys.setenv(gSD_user = getPass::getPass("Enter gSD user for tests:"))
Sys.setenv(gSD_pass = getPass::getPass(paste0("Enter gSD password for user '", Sys.getenv("gSD_user"), "':")))
Sys.setenv(gSD_downtests = FALSE)

# recreate product list
source("dev/update_product_list.R")

# run tests
devtools::test()