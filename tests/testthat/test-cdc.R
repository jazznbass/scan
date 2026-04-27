test_that("main", {
  
  res <- cdc(exampleABC)
  
  expect_true(sum(res$cdc_p) > 0.05)
  
  res <- cdc(exampleAB_50, trend_method = "trisplit")
  expect_true(sum(res$cdc_p) > 4)
})
