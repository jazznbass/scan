test_that("between_smd errors for a single case", {
  expect_error(
    between_smd(exampleAB$Johanna, method = "REML"),
    "with one case"
  )
})

test_that("between_smd returns sc_bcsmd with REML models and expected columns", {
  res <- between_smd(exampleAB[1:3], method = "REML", ci = 0.90, include_residuals = TRUE)
  res_no_resid <- between_smd(exampleAB[1:3], method = "REML", ci = 0.90, include_residuals = FALSE)
  
  expect_equal(object_checksum(res), '15.3268')
  expect_equal(object_checksum(res_no_resid), '1260.1821')
  
  # BC-SMD should increase when residual variance is excluded
  base_with <- res$models[["Base model"]][["BC-SMD"]]
  base_without <- res_no_resid$models[["Base model"]][["BC-SMD"]]
  expect_gt(mean(base_without), mean(base_with))
})
