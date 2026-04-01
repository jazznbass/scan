test_that("main", {
  
  res <- cdc(exampleABC)
  expect_equal(object_checksum(res), '51.3037')
  
  
  res <- cdc(exampleAB_50, trend_method = "trisplit")
  expect_equal(object_checksum(res), '1970.1410')
  
})
