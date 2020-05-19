#' test of is.sentinel3 checks from is_product_checker
# -----------------------------------------------------------------------------------------

s <- "Sentinel"
s3 <- "Sentinel-3"
dummy_olci <- "S3A_OL_2_LRR____20190904T094300_20190904T101456_20190905T143046_1915_049_022______LN1_O_NT_002"
dummy_slstr <- "S3B_SL_2_LST____20190903T193609_20190903T211708_20190905T014359_6059_029_256______LN2_O_NT_003"
dummy_syn <- "S3C_SY_2_VG1____20190901T163347_20190902T163347_20190907T165020_EUROPE____________LN2_O_NT_002"
dummy_sral <- "S3D_SR_2_LAN____20190903T202637_20190903T211703_20190928T232843_3026_029_256______LN3_O_NT_003"
dummy_records <- data.frame("product" = c(s3, s3, s3, s3, "LANDSAT_8_C1", "some_other_product"), 
                            "product_group" = c(s, s, s, s, "Landsat", "some_other_product_group"),
                            "record_id" = c(dummy_olci, dummy_slstr, dummy_syn, dummy_sral, 
                                            "somet_hi_ng", "som_ething_else"), stringsAsFactors = F)

# Test is.sentinel3
should_be <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
value <- is.sentinel3(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel3_olci
should_be <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
value <- is.sentinel3_olci(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel3_slstr
should_be <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
value <- is.sentinel3_slstr(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel3_synergy
should_be <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
value <- is.sentinel3_synergy(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel3_sral
should_be <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
value <- is.sentinel3_sral(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel3_S3A
should_be <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
value <- is.sentinel3_S3A(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel3_S3B
should_be <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
value <- is.sentinel3_S3B(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel3_S3C
should_be <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
value <- is.sentinel3_S3C(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel3_S3D
should_be <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
value <- is.sentinel3_S3D(dummy_records)
expect_equal(value, should_be)
