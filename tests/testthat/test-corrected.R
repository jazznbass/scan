test_that("main", {
  
  res <- corrected_tau(exampleAB_score)
  expect_all_true(sum(res$tau) > 2.12)
})

test_that(".output_bctau names the regression that is actually used", {
  res <- corrected_tau(exampleAB)
  out <- scan:::.output_bctau(res)

  expect_identical(out$notes[1], "Method: Theil-Sen regression")
  expect_false(any(grepl("Siegel", out$notes)))
  expect_true(any(grepl("Kendall's tau", out$notes)))

  txt <- paste(capture.output(print(res)), collapse = "\n")
  tab <- render_table(export(res))
  for (line in out$notes) {
    expect_true(grepl(line, txt, fixed = TRUE))
    expect_true(grepl(line, tab, fixed = TRUE))
  }
})

test_that(".output_bctau stacks the case tables without gaps", {
  res <- corrected_tau(exampleAB)
  out <- scan:::.output_bctau(res)

  expect_equal(nrow(out$stacked), sum(vapply(out$tables, nrow, 1L)))
  expect_true("Correction recommended?" %in% names(out$stacked))
  expect_identical(names(out$row_group), names(res$corrected_tau))
  expect_identical(
    sort(unlist(out$row_group, use.names = FALSE)),
    seq_len(nrow(out$stacked))
  )
  # the recommendation is given once per case, in its first row
  first_rows <- vapply(out$row_group, min, 1L)
  expect_true(all(out$stacked$"Correction recommended?"[first_rows] %in% c("Yes", "No")))
})
