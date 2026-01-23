test_that("main", {
  
  res <- cdc(exampleABC)
  expect_equal(object_checksum(res), "08cbb78c")
  
  
  res <- cdc(exampleAB_50, trend_method = "trisplit")
  expect_equal(object_checksum(res), "7880665d")
  
})
