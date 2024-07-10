test_that("tau-u", {
  
  data <- Grosche2011
  result <- tau_u(data = data)
  expect_s3_class(result, "sc_tauu")
  expect_identical(sum(round(result$Overall_tau_u$p,5)), 1.44159)
  expect_identical(sum(round(unlist(result$Overall_tau_u[, -1]),5)), 7.57242)
  expect_identical(round(result$table[[1]]|>sum(), 3), 5295.933)

  result <- tau_u(
    Huber2014, 
    method = "parker", ci = 0.9, ci_method = "tau"
  )
  
  expect_identical(unlist(result$table)|>sum(na.rm = TRUE)|>round(0), 439300)

  result <- tau_u(
    Huber2014, 
    meta_weight_method = "z"
  )
  expect_identical(sum(round(unlist(result$Overall_tau_u[, -1]),5)), 22.51747)
  
  
})
