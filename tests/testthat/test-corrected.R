test_that("main", {
  
  scdf <- exampleAB_score
  res <- corrected_tau(scdf)
  
  expect_equal(length(res$corrected_tau), length(scdf))
  a <- unlist(lapply(res$corrected_tau, function(x) sum(x[,-1])))
  b <- c(Christiano = 11.522481045063, Lionel = 11.9353973585274, 
         Neymar = 11.332898039478)
  expect_equal(a, b)
  
})
