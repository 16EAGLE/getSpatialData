# authentificate
Sys.setenv(gSD_user = getPass::getPass("Enter gSD user for tests:"))
Sys.setenv(gSD_pass = getPass::getPass(paste0("Enter gSD password for user '", Sys.getenv("gSD_user"), "':")))
Sys.setenv(gSD_downtests = "no")

Sys.setenv(gSD_updprod = "no")

# run tests
devtools::test()