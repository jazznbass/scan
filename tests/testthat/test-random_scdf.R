# random_scdf(): count distributions, extreme values, names and arguments

vals_of <- function(r) unlist(lapply(r, function(x) x$values))
is_int <- function(v) {
  v <- v[!is.na(v)]
  all(v == round(v))
}
count_warnings <- function(expr) {
  w <- character()
  withCallingHandlers(expr, warning = function(x) {
    w <<- c(w, conditionMessage(x))
    invokeRestart("muffleWarning")
  })
  length(w)
}

test_that("binomial data stay counts within the number of trials", {
  for (rng in list(c(5, 8), c(-8, -5))) {
    r <- random_scdf(design(
      n = 2, distribution = "binomial", start_value = 0.5, n_trials = 10,
      extreme_prop = 0.5, extreme_range = rng
    ), seed = 1)
    v <- vals_of(r)
    expect_true(is_int(v))
    expect_true(all(v >= 0 & v <= 10, na.rm = TRUE))
    expect_true("trials" %in% names(r[[1]]))
  }
})

test_that("poisson data stay non-negative counts", {
  for (rng in list(c(5, 8), c(-20, -15), c(-3, 3))) {
    v <- vals_of(random_scdf(design(
      n = 2, distribution = "poisson", start_value = 5,
      extreme_prop = 0.5, extreme_range = rng
    ), seed = 1))
    expect_true(is_int(v))
    expect_true(all(v >= 0, na.rm = TRUE))
  }
})

test_that("missing values survive the clipping of count data", {
  v <- vals_of(random_scdf(design(
    n = 2, distribution = "binomial", start_value = 0.5, n_trials = 10,
    extreme_prop = 0.5, extreme_range = c(5, 8), missing_prop = 0.2
  ), seed = 1))
  expect_gt(sum(is.na(v)), 0)
  expect_true(is_int(v))
  expect_true(all(v >= 0 & v <= 10, na.rm = TRUE))

  v <- vals_of(random_scdf(design(
    n = 2, distribution = "poisson", start_value = 5, extreme_prop = 0.5,
    extreme_range = c(-20, -15), missing_prop = 0.2
  ), seed = 1))
  expect_gt(sum(is.na(v)), 0)
  expect_true(is_int(v))
  expect_true(all(v >= 0, na.rm = TRUE))
})

test_that("gaussian data stay continuous and honour round", {
  g <- vals_of(random_scdf(design(n = 2, extreme_prop = 0.5), seed = 1))
  expect_false(is_int(g))
  g <- vals_of(random_scdf(design(n = 2), round = 1, seed = 1))
  expect_true(all(abs(g * 10 - round(g * 10)) < 1e-9))
})

test_that("simulated count data can be analysed", {
  d <- random_scdf(design(
    n = 3, distribution = "binomial", start_value = 0.5, n_trials = 10,
    extreme_prop = 0.3, extreme_range = c(3, 6)
  ), seed = 1)
  expect_no_error(m <- plm(d[1], family = "binomial", var_trials = "trials"))
  expect_true(all(m$data$values <= 1, na.rm = TRUE))

  d <- random_scdf(design(
    n = 3, distribution = "poisson", start_value = 10, extreme_prop = 0.3,
    extreme_range = c(-6, -3)
  ), seed = 1)
  expect_no_error(plm(d[1], family = "poisson"))
  expect_no_error(describe(d))
})

test_that("a malformed extreme_range is rejected", {
  bad <- list(c(-3, -4), c(4, 3), c(0, 0), c(-4), c(-4, -3, -2), c(NA, -3))
  for (rng in bad) {
    expect_error(design(n = 2, extreme_prop = 0.5, extreme_range = rng))
  }
  expect_error(design(
    n = 2, extreme_prop = 0.5, extreme_range = list(c(-8, -7), c(4, 3))
  ))
  for (rng in list(c(-4, -3), c(-10, 10), c(2, 6), c(0, 1))) {
    expect_no_error(design(n = 2, extreme_prop = 0.5, extreme_range = rng))
  }
  expect_no_error(design(
    n = 2, extreme_prop = 0.5, extreme_range = list(c(-8, -7), c(3, 4))
  ))

  # no silent NA and no warning in the simulated data
  r <- NULL
  expect_equal(
    count_warnings(r <- random_scdf(
      design(n = 2, extreme_prop = 0.5, extreme_range = c(-4, -3)), seed = 1
    )),
    0
  )
  expect_equal(sum(is.na(vals_of(r))), 0)
})

test_that("a number as the first argument is used, not discarded", {
  for (k in c(1, 3, 5)) {
    x <- NULL
    expect_equal(count_warnings(x <- random_scdf(k, seed = 1)), 1)
    expect_length(x, k)
  }
  x <- suppressWarnings(random_scdf(3, phase_design = list(A = 4, B = 6), seed = 1))
  expect_length(x, 3)
  expect_true(all(vapply(x, nrow, 1L) == 10L))
})

test_that("the documented ways of calling random_scdf give no warning", {
  x <- NULL
  expect_equal(count_warnings(x <- random_scdf(n = 3, seed = 1)), 0)
  expect_length(x, 3)
  expect_equal(count_warnings(x <- random_scdf(design(n = 4), seed = 1)), 0)
  expect_length(x, 4)
  expect_length(random_scdf(seed = 1), 1)
  expect_length(random_scdf(design = design(n = 2), seed = 1), 2)
})

test_that("random_names accepts the documented values only", {
  for (v in list(TRUE, "male", "female", "neutral")) {
    x <- random_scdf(design(n = 3), random_names = v, seed = 1)
    expect_length(names(x), 3)
    expect_true(all(nzchar(names(x))))
  }
  expect_null(names(random_scdf(design(n = 3), random_names = FALSE, seed = 1)))
  for (v in c("nonsense", "Male", "maleX", "", "m")) {
    expect_error(random_scdf(design(n = 3), random_names = v, seed = 1))
  }
})
