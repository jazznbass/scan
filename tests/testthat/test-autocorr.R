test_that("main", {
  
  res <- autocorr(exampleABC)
  testsum <- sum(
    unlist(lapply(res$autocorr, function(x) unlist(x[,-1]))), na.rm = TRUE
  )
  
  expect_equal(round(testsum,4), -0.3649)
  
})
