test_that("main", {
  
  res <- cdc(exampleAB_50)
  testsum <- sum(res$cdc_be + res$cdc_b)
  expect_equal(testsum, 1929)
  
  
  res <- cdc(exampleAB_50, trend_method = "trisplit")
  testsum <- sum(res$cdc_be + res$cdc_b)
  expect_equal(testsum, 1915)
  
})
