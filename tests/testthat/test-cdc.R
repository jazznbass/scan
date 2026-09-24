test_that("main", {
  
  res <- cdc(exampleABC)
  
  expect_true(sum(res$cdc_p) > 0.05)
  
  res <- cdc(exampleAB_50, trend_method = "trisplit")
  expect_true(sum(res$cdc_p) > 4)
})

test_that(".output_cdc builds one table for print and export", {
  res <- cdc(exampleAB)
  out <- scan:::.output_cdc(res)

  expect_identical(
    names(out$table),
    c("Case", "nB improve", "nB", "binom p", "CDC Evaluation")
  )
  expect_identical(out$table$Case, res$case_names)
  expect_identical(out$table$"binom p", scan:::.nice_p(res$cdc_p))
  expect_equal(scan:::.output_cdc(res, nice = FALSE)$table$"binom p", res$cdc_p)
})

test_that("the hypothesis reaches the console and the footnote", {
  res <- cdc(exampleAB)
  out <- scan:::.output_cdc(res)
  expect_true(any(grepl("increase", out$hypothesis)))
  expect_true(any(grepl("> 50%", out$hypothesis)))

  txt <- paste(capture.output(print(res)), collapse = "\n")
  tab <- render_table(export(res))
  for (line in out$hypothesis) {
    expect_true(grepl(line, txt, fixed = TRUE))
    expect_true(grepl(line, tab, fixed = TRUE))
  }

  decreasing <- scan:::.output_cdc(cdc(exampleAB, decreasing = TRUE))
  expect_true(any(grepl("decrease", decreasing$hypothesis)))
  expect_true(any(grepl("< 50%", decreasing$hypothesis)))
})

test_that("the overall evaluation is reported only for several cases", {
  expect_null(scan:::.output_cdc(cdc(exampleAB$Johanna))$overall)
  out <- scan:::.output_cdc(cdc(exampleAB))
  expect_true(grepl("Overall evaluation", out$overall))
  expect_true(grepl(out$overall, render_table(export(cdc(exampleAB))), fixed = TRUE))
})
