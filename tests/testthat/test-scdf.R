test_that("scdf", {
  
  a <- scdf(values = c(A = 2,2,4,5, B = 8,7,6,9,8,7))
  b <- scdf(values = c(2,2,4,5,8,7,6,9,8,7), phase_starts = c(A = 1, B = 5))
  c <- scdf(values = c(2,2,4,5,8,7,6,9,8,7), phase_design = c(A = 4, B = 6))
  
  expect_s3_class(a, "scdf")
  
  expect_identical(a, b)
  expect_identical(a, c)
  
  expect_error(
    scdf(values = c(2,2,4,5,8,7,6,9,8,7), phase_starts = c(A = 2, B = 5))
  )
  
  expect_error(
    scdf(values = c(2,2,4,5,8,7,6,9,8,7), phase_starts = c(A = 1, B = 15))
  )
  
  d <- scdf(
    values = c(2,2,4,5,8,7,6,9,8,7), 
    phase_starts = c(A = 1, B = 5, C = 18), 
    mt = c(1,3,5,7,9,13,15,18,21,23)
  )

  expect_identical(
    as.data.frame(d)$phase |> table() |> as.numeric(),
    c(2,5,3)
  )
  
})
