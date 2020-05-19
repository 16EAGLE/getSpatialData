#' test of is.sentinel1 checks from is_product_checker
# -----------------------------------------------------------------------------------------

s <- "Sentinel"
s1 <- "Sentinel-1"
dummy_iw_slc <- "S1A_IW_SLC__1SDV_20190801T050223_20190801T050250_028371_0334BC_B6FC"
dummy_iw_grdh <- "S1A_IW_GRDH_1SDV_20190801T050249_20190801T050314_028371_0334BC_A1DC"
dummy_iw_ocn <- "S1B_IW_OCN__2SDV_20190801T165734_20190801T165759_017395_020B63_BF13"
dummy_iw_raw <- "S1A_IW_RAW__0SDV_20190801T050220_20190801T050252_028371_0334BC_69DB"
dummy_records <- data.frame("product" = c(s1, s1, s1, s1, "LANDSAT_8_C1", "some_other_product"), 
                            "product_group" = c(s, s, s, s, "Landsat", "some_other_product_group"),
                            "record_id" = c(dummy_iw_slc, dummy_iw_grdh, dummy_iw_ocn, dummy_iw_raw, 
                                            "somet_hi_ng", "som_ething_else"), stringsAsFactors = F)

# Test is.sentinel1
should_be <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
value <- is.sentinel1(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel1_iw_slc
should_be <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
value <- is.sentinel1_iw_slc(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel1_iw_grdh
should_be <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
value <- is.sentinel1_iw_grdh(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel1_iw_ocn
should_be <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
value <- is.sentinel1_iw_ocn(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel1_iw_raw
should_be <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
value <- is.sentinel1_iw_raw(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel1_level0
should_be <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
value <- is.sentinel1_level0(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel1_level1
should_be <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
value <- is.sentinel1_level1(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel1_level2
should_be <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
value <- is.sentinel1_level2(dummy_records)
expect_equal(value, should_be)



