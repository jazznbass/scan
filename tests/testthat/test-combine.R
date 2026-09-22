# combine() / c(): case names must stay unique

mk <- function(name) {
  scdf(
    values = c(1, 2, 3, 4, 7, 8, 9, 10),
    phase_design = c(A = 4, B = 4), name = name
  )
}

test_that("different case names are kept", {
  expect_no_warning(s <- c(mk("anna"), mk("bert")))
  expect_identical(names(s), c("anna", "bert"))
  expect_s3_class(s, "scdf")
  expect_length(s, 2)
})

test_that("duplicated case names are made unique with a warning", {
  expect_warning(d <- c(mk("anna"), mk("anna")), "anna")
  expect_false(anyDuplicated(names(d)) > 0)
  expect_identical(names(d)[1], "anna")
  expect_length(d, 2)
  expect_equal(
    lapply(d, as.data.frame),
    lapply(c(mk("x"), mk("y")), as.data.frame),
    ignore_attr = TRUE
  )
  expect_length(select_cases(d, names(d)[2]), 1)

  expect_warning(dd <- c(exampleAB, exampleAB))
  expect_length(dd, 6)
  expect_false(anyDuplicated(names(dd)) > 0)

  expect_warning(a <- c(A = mk("anna"), A = mk("bert")))
  expect_false(anyDuplicated(names(a)) > 0)
})

test_that("cases without a name are combined silently", {
  un <- scdf(values = c(1, 2, 3, 4, 7, 8, 9, 10), phase_design = c(A = 4, B = 4))
  expect_no_warning(u <- c(un, un))
  expect_length(u, 2)
  expect_no_error(capture.output(print(u)))
})

test_that("combine keeps info, author and the variable names", {
  expect_error(combine())
  x <- combine(mk("anna"), mk("bert"), info = "an info", author = "JW")
  expect_identical(scdf_attr(x, "info"), "an info")
  expect_identical(scdf_attr(x, "author"), "JW")
  expect_identical(
    c(dv(x), phase(x), mt(x)),
    c(dv(mk("a")), phase(mk("a")), mt(mk("a")))
  )
})
