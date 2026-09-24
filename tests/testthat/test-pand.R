test_that("pand", {
  
  data <- scdf(c(A = 20, 20, 26, 25, 22, 23, B = 28, 25, 24, 27, 30, 30, 29))
  result <- pand(data = data)
  expect_s3_class(result, "sc_pand")
  expect_identical(round(result$pand,5) ,84.61538)
  expect_equal(round(result$phi, 4) , 0.6905)
  
  result <- pand(Parker2009, method = "minimum")
  expect_equal(round(result$pand, 5) , 86.95652)
  result <- pand(Parker2009, method = "sort")
  expect_equal(result$pand , 100)
  
})

test_that(".output_pand builds the two matrices from one source", {
  res <- pand(exampleAB, method = "sort")
  out <- scan:::.output_pand(res)

  m <- out$matrix_counts
  expect_identical(rownames(m), c("A", "B", "Total"))
  expect_identical(colnames(m), c("A", "B", "Total"))
  expect_equal(unname(m["Total", "Total"]), out$n)

  # the total column holds the row sums, the total row the column sums
  expect_equal(unname(m[, "Total"]), unname(m[, "A"] + m[, "B"]))
  expect_equal(unname(m["Total", ]), unname(m["A", ] + m["B", ]))

  # the percentages are the same matrix, not a second computation
  expect_equal(out$matrix_percent, m / out$n * 100)

  expect_equal(out$phi_squared, res$phi^2)
})

test_that("print and export of pand report the same values", {
  res <- pand(exampleAB, method = "sort")
  out <- scan:::.output_pand(res)
  txt <- paste(capture.output(print(res)), collapse = "\n")
  tab <- render_table(export(res))

  # the test statistics are reported with three decimals in both
  chi <- scan:::.pand_chi_line(out)
  expect_match(chi, "= [0-9]+\\.[0-9]{3},")
  expect_true(grepl(chi, txt, fixed = TRUE))
  expect_true(grepl(chi, tab, fixed = TRUE))
  expect_true(grepl(scan:::.pand_fisher_line(out), txt, fixed = TRUE))

  # the printed matrix carries its totals
  expect_true(any(grepl("^Total", capture.output(print(res)))))
})

test_that("pand with method minimum has no matrices", {
  out <- scan:::.output_pand(pand(exampleAB, method = "minimum"))
  expect_null(out$matrix_counts)
  expect_null(out$chi)
  expect_no_error(capture.output(print(pand(exampleAB, method = "minimum"))))
  expect_no_error(export(pand(exampleAB, method = "minimum")))
})
