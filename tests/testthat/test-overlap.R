test_that("overlap", {
  data <- exampleAB
  
  result <- overlap(data = data)
  expect_s3_class(result, "sc_overlap")
  expect_s3_class(result$overlap, "data.frame")
  expect_identical(nrow(result$overlap), 3L)
  expect_identical(ncol(result$overlap), 16L)
  expect_identical(names(result$overlap), 
                   c("Case", "Design", "PND", "PEM", "PET", "NAP", "NAP rescaled", 
                     "PAND", "IRD",  "Tau_U(A)", "Tau_U(BA)", "Base_Tau", "Diff_mean", 
                     "Diff_trend", "SMD", "Hedges_g"))
  expect_equal(round(result$overlap[1, "Diff_mean"], 4), 19.5333)
  expect_equal(round(result$overlap[1, "SMD"], 4), 8.1108)
  expect_equal(round(result$overlap[1, "Hedges_g"], 4), 2.3478)
  expect_equal(round(result$overlap[1, "Diff_trend"], 3), 1.525)
  expect_equal(
    round(sum(apply(result$overlap[,3:15], 2, function(x) sum(x), simplify = TRUE)), 3),
    1829.948
  )
})
