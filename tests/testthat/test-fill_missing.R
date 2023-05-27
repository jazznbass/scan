test_that("fill_missing", {
  scdf <- fill_missing(Grosche2011)
  values <- round(sum(unlist(lapply(scdf, function(x) x$values))),4)
  expect_equal(values, 1831.035)
})
