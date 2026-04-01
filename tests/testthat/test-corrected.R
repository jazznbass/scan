test_that("main", {
  
  res <- corrected_tau(exampleAB_score)
  expect_equal(object_checksum(res), '4243.6664')
  
})
