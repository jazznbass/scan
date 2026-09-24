# power_test(): the reported statistics, the binomial tests and the intervals

# A design with a clear level effect. The tests below mostly use dummy methods
# that return a fixed p value, so the result is deterministic and no model is
# fitted; the last block runs one real method as a smoke test.
ab_design <- function() {
  design(n = 1, phase_design = list(A = 6, B = 9), level = 1.4, s = 1)
}

always_significant <- function(x) 0.01
never_significant <- function(x) 0.5

test_that("power and alpha error are the proportion of significant tests", {
  res <- power_test(
    ab_design(), n_sim = 5, method = list(significant = always_significant)
  )
  expect_s3_class(res, "sc_power")
  expect_false(inherits(res, "data.frame"))
  expect_identical(res$Method, "significant")
  expect_equal(res$Power, 100)
  expect_equal(res$`Alpha Error`, 100)
  expect_equal(res$Correct, 50)

  res <- power_test(
    ab_design(), n_sim = 5, method = list(flat = never_significant)
  )
  expect_equal(res$Power, 0)
  expect_equal(res$`Alpha Error`, 0)
  expect_equal(res$Correct, 50)
  # a ratio of alpha to beta is not defined without an alpha error
  expect_true(is.na(res$`Alpha:Beta`))
})

test_that("an unnamed function in method gets a placeholder name", {
  res <- power_test(ab_design(), n_sim = 3, method = list(always_significant))
  expect_identical(res$Method, "function1")
  res <- power_test(
    ab_design(), n_sim = 3,
    method = list(always_significant, named = never_significant)
  )
  expect_identical(res$Method, c("function1", "named"))
})

test_that("a switched off test leaves its statistics missing", {
  res <- power_test(
    ab_design(), n_sim = 3, method = list(f = always_significant),
    alpha_test = FALSE
  )
  expect_true(is.na(res$`Alpha Error`))
  expect_true(is.na(res$`Alpha:Beta`))

  res <- power_test(
    ab_design(), n_sim = 3, method = list(f = always_significant),
    power_test = FALSE
  )
  expect_true(is.na(res$Power))
  expect_true(is.na(res$`Alpha:Beta`))

  # a binomial test against a switched off statistic does not stop the call
  expect_no_error(power_test(
    ab_design(), n_sim = 3, method = list(f = always_significant),
    power_test = FALSE, binom_test = TRUE
  ))
  expect_no_error(power_test(
    ab_design(), n_sim = 3, method = list(f = always_significant),
    alpha_test = FALSE, binom_test = TRUE
  ))
})

test_that("the binomial tests report a p value, not a rounded 0 or 1", {
  # every second simulation is significant, so power and alpha error are 50
  # percent and the binomial tests give p values that are neither 0 nor 1
  mixed <- local({
    i <- 0
    function(x) {
      i <<- i + 1
      if (i %% 2 == 0) 0.01 else 0.5
    }
  })
  res <- power_test(
    ab_design(), n_sim = 10, method = list(f = mixed), binom_test = TRUE
  )
  expect_equal(res$Power, 50)
  expect_equal(res$`Alpha Error`, 50)
  for (column in c("p_power", "p_alpha", "p_correct")) {
    expect_true(column %in% names(res))
    expect_false(is.na(res[[column]]))
    expect_true(res[[column]] >= 0 && res[[column]] <= 1)
  }
  # this one was rounded to a whole number before, which could only be 0 or 1
  expect_gt(res$p_power, 0)
  expect_lt(res$p_power, 1)

  # each threshold can be asked for on its own
  res <- power_test(
    ab_design(), n_sim = 10, method = list(f = always_significant),
    binom_test_correct = 0.875
  )
  expect_true("p_correct" %in% names(res))
  expect_false("p_power" %in% names(res))
  expect_false("p_alpha" %in% names(res))
  out <- capture.output(print(res))
  expect_true(any(grepl("Correct", out)))
})

test_that("all three confidence intervals follow the requested level", {
  res_90 <- power_test(
    ab_design(), n_sim = 20, method = list(f = never_significant), ci = 0.90
  )
  res_99 <- power_test(
    ab_design(), n_sim = 20, method = list(f = never_significant), ci = 0.99
  )
  for (what in c("Power", "Alpha Error", "Correct")) {
    width_90 <- res_90[[paste(what, "upper")]] - res_90[[paste(what, "lower")]]
    width_99 <- res_99[[paste(what, "upper")]] - res_99[[paste(what, "lower")]]
    expect_gt(width_99, width_90)
  }
  expect_no_error(capture.output(print(res_99)))
})

test_that("print and export of a power_test result run", {
  res <- power_test(
    ab_design(), n_sim = 5, method = list(f = always_significant),
    binom_test = TRUE, ci = 0.95
  )
  expect_no_error(capture.output(print(res)))
  expect_no_error(capture.output(print(res, duration = TRUE)))
  expect_no_error(capture.output(print(res, digits = 3)))
  expect_no_error(export(res))
  expect_false(is.null(attr(res, "computation_duration")))
})

test_that("the built in methods run", {
  skip_on_cran()
  set.seed(1)
  res <- power_test(
    ab_design(), n_sim = 10, method = list("plm_level", "rand", "tauU")
  )
  expect_identical(res$Method, c("plm_level", "rand", "tauU"))
  expect_true(all(res$Power >= 0 & res$Power <= 100))
  expect_true(all(res$`Alpha Error` >= 0 & res$`Alpha Error` <= 100))

  # the falling direction uses its own statistic
  set.seed(2)
  res <- power_test(
    design(n = 1, phase_design = list(A = 6, B = 9), level = -1.4, s = 1),
    n_sim = 10, method = list("rand", "rand_decrease")
  )
  expect_gt(res$Power[2], res$Power[1])
})
