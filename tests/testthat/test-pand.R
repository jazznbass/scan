test_that("pand", {
  
  data <- scdf(c(A = 20, 20, 26, 25, 22, 23, B = 28, 25, 24, 27, 30, 30, 29))
  result <- pand(data = data)
  expect_s3_class(result, "sc_pand")
  expect_identical(round(result$pand,5) ,84.61538)
  expect_equal(round(result$phi, 4) , 0.6905)
  
  result <- pand(Parker2009, method = "minimum")
  expect_equal(round(result$pand, 5) , 86.95652)
  result <- pand(Parker2009, method = "sort")
  expect_equal(result$pand , 100)
  
})
