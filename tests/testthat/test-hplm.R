test_that("hplm", {
  
  scdf <- exampleA1B1A2B2
  result <- hplm(scdf)
  values <- round(sum(residuals(result$hplm)^2), 3)
  expect_equal(values, 3009.384)

  result <- hplm(scdf, contrast = "preceding")
  values <- round(sum(residuals(result$hplm)^2), 3)
  expect_equal(values, 3005.251)
  
  result <- hplm(
    exampleAB_50, 
    model = "W",
    method = "REML",
    unequal_variances = TRUE,
    random_trend = TRUE,
    control = lmeControl(msMaxIter = 50, apVar = FALSE, returnObject = TRUE),
    ar = 1
  )
  values <- round(sum(residuals(result$hplm)^2))
  expect_equal(values, 34923)
  
})