# the print methods in edge configurations

expect_prints <- function(object, ...) {
  expect_no_error(capture.output(print(object, ...)))
}

test_that("print.sc_design covers the parameter variations", {
  designs <- list(
    "one case" = design(n = 1),
    "three cases" = design(n = 3),
    "extreme_prop" = design(n = 3, extreme_prop = 0.1),
    "extreme_prop per case" = design(n = 3, extreme_prop = list(0, 0.1, 0.2)),
    "extreme_prop all zero" = design(n = 3, extreme_prop = list(0, 0, 0)),
    "extreme_range per case" = design(
      n = 2, extreme_prop = 0.1, extreme_range = list(c(-4, -3), c(3, 4))
    ),
    "s per case" = design(n = 3, s = c(5, 10, 15)),
    "rtt per case" = design(n = 3, rtt = c(.6, .7, .8)),
    "poisson" = design(n = 2, distribution = "poisson"),
    "binomial" = design(
      n = 2, distribution = "binomial", n_trials = 10, start_value = 0.4
    ),
    "different phase designs" = design(n = 2, B_start = c(5, 8), mt = c(20, 25))
  )
  for (nm in names(designs)) expect_prints(designs[[nm]])
})

test_that("the effect size print methods run for several cases", {
  ab <- exampleAB
  one <- exampleAB$Johanna
  objects <- list(
    describe = describe(ab), overlap = overlap(ab), nap = nap(ab),
    pnd = pnd(ab), pem = pem(ab), pet = pet(ab), ird = ird(ab),
    smd = smd(ab), trend = trend(one), rci = rci(one, rel = 0.8),
    tau_u = tau_u(ab), corrected_tau = corrected_tau(one),
    between_smd = between_smd(ab), autocorr = autocorr(ab),
    cdc = cdc(ab), pand = pand(ab), outlier = outlier(ab, method = "SD")
  )
  for (nm in names(objects)) expect_prints(objects[[nm]])
})

test_that("the same methods run for a single case", {
  one <- exampleAB$Johanna
  objects <- list(
    describe = describe(one), overlap = overlap(one), nap = nap(one),
    pnd = pnd(one), pem = pem(one), pet = pet(one), ird = ird(one),
    smd = smd(one), tau_u = tau_u(one), cdc = cdc(one), pand = pand(one)
  )
  for (nm in names(objects)) expect_prints(objects[[nm]])
})

test_that("the print methods cope with missing values", {
  na_case <- exampleAB$Johanna
  na_case[[1]]$values[c(3, 12)] <- NA
  objects <- list(
    describe = describe(na_case), nap = nap(na_case), pnd = pnd(na_case),
    smd = smd(na_case), tau_u = tau_u(na_case), trend = trend(na_case),
    overlap = overlap(na_case)
  )
  for (nm in names(objects)) expect_prints(objects[[nm]])
})

test_that("the arguments of the print methods are accepted", {
  ab <- exampleAB
  expect_prints(tau_u(ab), complete = TRUE)
  expect_prints(tau_u(ab, ci = NULL))
  expect_prints(tau_u(ab, meta_analyses = FALSE))
  expect_prints(nap(ab), complete = TRUE)
  expect_prints(describe(ab), digits = 1)
  expect_prints(overlap(ab), digits = 4)
  expect_prints(smd(ab), digits = "auto")
})

test_that("the regression models print", {
  skip_on_cran()
  expect_prints(plm(exampleAB$Johanna))
  expect_prints(plm(exampleAB$Johanna, family = "poisson"))
  expect_prints(hplm(exampleAB))
  expect_prints(hplm(exampleAB), casewise = TRUE)
  expect_prints(mplm(exampleAB_add, dvar = c("wellbeing", "cigarrets")))
  set.seed(1234)
  expect_prints(rand_test(exampleAB, number = 100))
})

test_that("the summary of an scdf is written by its print method", {
  expect_silent(s <- summary(exampleAB))
  expect_s3_class(s, "scdf_summary")
  expect_output(print(s), "single-case data frame")
  # the combination with export stays silent
  expect_silent(export(summary(exampleAB)))
  expect_silent(export(s))

  # all_cases is kept by summary and can be overridden when printing
  s50 <- summary(exampleAB_50)
  expect_true(any(grepl("skipped", capture.output(print(s50)))))
  expect_false(any(grepl("skipped", capture.output(print(s50, all_cases = TRUE)))))
  expect_false(any(grepl(
    "skipped",
    capture.output(print(summary(exampleAB_50, all_cases = TRUE)))
  )))
})
