#' test of is.landsat checks from is_product_checker
# -----------------------------------------------------------------------------------------

product_group <- "Landsat"
product1 <- "LANDSAT_MSS_C1"
product2 <- "LANDSAT_TM_C1"
product3 <- "LANDSAT_ETM_C1"
product4 <- "LANDSAT_8_C1"
dummy_records <- data.frame("product" = c(product1, product2, product3, product4, "Sentinel-2", "some_other_product"), 
                            "product_group" = c(product_group, product_group, product_group, product_group, 
                                                "Sentinel", "some_other_product_group"),
                            "record_id" = c("M04_L1GS_014054_19920514_20180318_01_T2",
                                            "LT05_L1TP_038037_20120505_20160830_01_T1",
                                            "LE07_L1TP_191035_20190906_20191002_01_T1", 
                                            "LC08_L1TP_192035_20190921_20190926_01_T1", 
                                            "S2B_MSIL12A_20190905T095031_N0208_R079_T33TWL_20190905T110359", 
                                            "s_omething_else"), stringsAsFactors = F)

# Test is.landsat
should_be <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
value <- is.landsat(dummy_records)
expect_equal(value, should_be)

# Test is.landsatMSS
should_be <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
value <- is.landsatMSS(dummy_records)
expect_equal(value, should_be)

# Test is.landsat5
should_be <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
value <- is.landsat5(dummy_records)
expect_equal(value, should_be)

# Test is.landsat7
should_be <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
value <- is.landsat7(dummy_records)
expect_equal(value, should_be)

# Test is.landsat8
should_be <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
value <- is.landsat8(dummy_records)
expect_equal(value, should_be)




