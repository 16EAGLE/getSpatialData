#' test of is.sentinel2 checks from is_product_checker
# -----------------------------------------------------------------------------------------

s <- "Sentinel"
s2 <- "Sentinel-2"
dummy_records <- data.frame("product" = c(s2, s2, s2, "LANDSAT_8_C1", "some_other_product"), 
                            "product_group" = c(s, s, s, "Landsat", "some_other_product_group"),
                            "record_id" = c("S2A_MSIL1C_20190905T095031_N0208_R079_T33TWL_20190905T110359", 
                                            "S2B_MSIL12A_20190905T095031_N0208_R079_T33TWL_20190905T110359",
                                            "S2B_MSIL2A_20190905T095031_N0208_R079_T33TWL_20190905T110359",
                                            "some_t_hing", 
                                            "s_omething_else"), stringsAsFactors = F)

# Test is.sentinel2
should_be <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
value <- is.sentinel2(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel2_L1C
should_be <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
value <- is.sentinel2_L1C(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel2_L2A
should_be <- c(FALSE, TRUE, TRUE, FALSE, FALSE)
value <- is.sentinel2_L2A(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel2_S2A
should_be <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
value <- is.sentinel2_S2A(dummy_records)
expect_equal(value, should_be)

# Test is.sentinel2_S2B
should_be <- c(FALSE, TRUE, TRUE, FALSE, FALSE)
value <- is.sentinel2_S2B(dummy_records)
expect_equal(value, should_be)
