test_that("estimate_design", {
  scdf <- exampleABC
  result <- estimate_design(scdf)
  expect_s3_class(result, "sc_design")
  expected_vars <- c(
    'phase', 'length', 'rtt', 'missing_prop', 'extreme_prop', 
    'extreme_low', 'extreme_high', 'trend', 'level', 'slope', 'start_value', 
    's', 'start', 'stop'
  )
  for (i in seq_along(result$cases)) {
    expect_equal(sort(names(result$cases[[i]])), sort(expected_vars))
  }
  
  value <- round(sum(unlist(lapply(result$cases, function(x)  unlist(x[-1])))), 4)
  expect_equal(value, 594.4769)  
})
