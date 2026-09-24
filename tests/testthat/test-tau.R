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

test_that(".output_tauu stacks the case tables without gaps", {
  res <- tau_u(exampleAB)
  out <- scan:::.output_tauu(res)

  expect_equal(nrow(out$stacked), sum(vapply(out$tables, nrow, 1L)))
  expect_true("Model" %in% names(out$stacked))
  expect_identical(names(out$row_group), names(res$table))
  expect_identical(
    sort(unlist(out$row_group, use.names = FALSE)),
    seq_len(nrow(out$stacked))
  )
  expect_length(out$main_models, 4)
  expect_true(all(out$main_models %in% rownames(res$table[[1]])))
})

test_that("the export footnote names a confidence interval only if there is one", {
  tab <- render_table(export(tau_u(exampleAB)))
  expect_true(grepl("CIs for tau are reported", tab, fixed = TRUE))
  expect_false(grepl("NA%", tab, fixed = TRUE))

  tab <- render_table(export(tau_u(exampleAB, ci = NULL)))
  expect_false(grepl("CIs for tau are reported", tab, fixed = TRUE))
  expect_false(grepl("NA%", tab, fixed = TRUE))
  expect_true(grepl("Theil|Kendall", tab))
})
