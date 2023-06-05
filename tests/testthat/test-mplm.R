test_that("mplm", {
  data <- Leidig2018$`1a1`
  result <- mplm(
    data,
    dvar = c("academic_engagement", "disruptive_behavior")
  )

  expect_s3_class(result, "sc_mplm")
 
  expect_equal(
    round(sum(coef(result$full.model)), 3),
    4.575
  )
  
  expect_equal(
    round(sum(result$full.model$coef_std), 4),
    -0.0102
  )
  
})
