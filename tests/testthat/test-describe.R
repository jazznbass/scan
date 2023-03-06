test_that("main", {
  
  scdf <- exampleABC
  res <- describe(scdf)
  
  a <- sum(res$descriptives[,-(1:2)])
  b <- 2615.03644924563
  expect_equal(a, b)
  
})
