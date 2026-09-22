# rescale(): variable names, target mean and standard deviation

vals_of <- function(x, var = "values") {
  unlist(lapply(x, function(case) case[[var]]), use.names = FALSE)
}

study <- exampleAB

test_that("rescale standardizes across all cases", {
  r1 <- rescale(study, values, mt)
  expect_equal(c(mean(vals_of(r1)), sd(vals_of(r1))), c(0, 1))
  expect_equal(c(mean(vals_of(r1, "mt")), sd(vals_of(r1, "mt"))), c(0, 1))
  expect_equal(vals_of(r1), as.numeric(scale(vals_of(study))))
  expect_identical(as.character(vals_of(r1, "phase")),
                   as.character(vals_of(study, "phase")))
  expect_s3_class(r1, "scdf")
  expect_identical(names(r1), names(study))

  # without any variable given, every numeric variable is standardized
  expect_identical(rescale(study), r1)
})

test_that("variables can be named as objects, characters or variables", {
  r1 <- rescale(study, values, mt)
  expect_identical(rescale(study, "values", "mt"), r1)
  v <- c("values", "mt")
  expect_identical(rescale(study, v), r1)
  f <- function() {
    w <- c("values", "mt")
    rescale(study, w)
  }
  expect_identical(f(), r1)
  g <- function(x) rescale(study, x)
  expect_identical(g(v), r1)
})

test_that("unknown or non numeric variables are rejected", {
  e <- tryCatch(rescale(study, "valeus"), error = conditionMessage)
  expect_true(grepl("valeus", e, fixed = TRUE))
  e <- tryCatch(rescale(study, phase), error = conditionMessage)
  expect_true(grepl("phase", e, fixed = TRUE))
  mixed <- study
  mixed[[1]]$extra <- seq_len(nrow(mixed[[1]]))
  expect_error(rescale(mixed, extra))
})

test_that("m and sd set the target values", {
  ref <- vals_of(study)
  r <- rescale(study, values, m = 50, sd = 10)
  expect_equal(c(mean(vals_of(r)), sd(vals_of(r))), c(50, 10))
  r <- rescale(study, values, sd = NULL)
  expect_equal(sd(vals_of(r)), sd(ref))
  expect_equal(mean(vals_of(r)), 0)
  r <- rescale(study, values, m = NULL)
  expect_equal(mean(vals_of(r)), mean(ref))
  expect_equal(sd(vals_of(r)), 1)
})

test_that("rescale works in a pipe", {
  expect_no_error(exampleAB_50 |> rescale(values, mt) |> hplm())
  expect_no_error(exampleAB_50 |> rescale("values") |> summary())
})
