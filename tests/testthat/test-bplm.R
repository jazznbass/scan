# bplm(): the bayesian multilevel model
#
# The sampler is run with very few iterations throughout. These tests check the
# structure of the result and the arguments, not the estimates.

fit_bplm <- function(data, ...) {
  do.call(bplm, c(list(data), list(nitt = 600, burnin = 100, thin = 1), list(...)))
}

test_that("bplm returns a model with the expected elements", {
  skip_on_cran()
  set.seed(1)
  res <- fit_bplm(example_A24)
  expect_s3_class(res, "sc_bplm")
  expect_true(all(c("model", "N", "formula", "mcmcglmm", "contrast") %in%
                    names(res)))
  expect_equal(res$N, length(example_A24))
  expect_s3_class(res$mcmcglmm, "MCMCglmm")
  expect_identical(fetch(res), res$mcmcglmm)
  expect_identical(res$formula$fixed, res$model$fixed)
  expect_identical(res$formula$random, res$model$random)
  # a single case has no random part
  expect_null(res$model$random)
  expect_identical(res$contrast, list(level = "first", slope = "first"))
})

test_that("several cases get a random intercept per case", {
  skip_on_cran()
  set.seed(1)
  res <- fit_bplm(exampleAB_50)
  expect_equal(res$N, length(exampleAB_50))
  expect_identical(deparse(res$model$random), "~case")
})

test_that("a random formula given by hand is not replaced by the switches", {
  skip_on_cran()
  set.seed(1)
  own <- ~case
  res <- fit_bplm(exampleAB_50, random = own, random_level = TRUE)
  expect_identical(deparse(res$model$random), deparse(own))
})

test_that("the switches build the random formula", {
  # the formula is built by a helper, which is checked here without fitting:
  # a random level effect on one case is not identifiable
  random <- scan:::.create_random_formula(
    mvar = "mt", slope = FALSE, level = TRUE, trend = FALSE,
    var_phase = "phaseB", var_inter = "interB", syntax = "mcmc"
  )
  expect_true(grepl("phaseB", deparse(random)))
  random <- scan:::.create_random_formula(
    mvar = "mt", slope = FALSE, level = FALSE, trend = TRUE,
    var_phase = "phaseB", var_inter = "interB", syntax = "mcmc"
  )
  expect_true(grepl("mt", deparse(random)))
})

test_that("the fixed part follows trend, level and slope", {
  skip_on_cran()
  set.seed(1)
  default <- deparse(fit_bplm(example_A24)$model$fixed)

  res <- fit_bplm(example_A24, slope = FALSE)
  expect_false(grepl("inter", deparse(res$model$fixed)))

  res <- fit_bplm(example_A24, trend = FALSE)
  expect_false(grepl("\\bmt\\b", deparse(res$model$fixed)))

  updated <- deparse(fit_bplm(example_A24, update_fixed = ". ~ . + 0")$model$fixed)
  expect_false(identical(default, updated))
})

test_that("print and export of a bplm result run", {
  skip_on_cran()
  set.seed(1)
  res <- fit_bplm(example_A24)
  expect_no_error(capture.output(print(res)))
  for (engine in c("gt", "kable")) {
    old <- getOption("scan.export.engine")
    options(scan.export.engine = engine)
    expect_no_error(export(res))
    options(scan.export.engine = old)
  }
})
