test_that("hplm", {
  
  scdf <- exampleA1B1A2B2
  result <- hplm(scdf)
  values <- round(sum(residuals(result$hplm)^2), 3)
  expect_equal(values, 3009.384)

  result <- hplm(scdf, contrast = "preceding")
  values <- round(sum(residuals(result$hplm)^2), 3)
  expect_equal(values, 3005.251)
  
  
  
})