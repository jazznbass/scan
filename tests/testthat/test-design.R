test_that("design function returns expected output", {
  # Test case 1
  result <- design()
  expect_s3_class(result, "sc_design")
  expect_equal(length(result$cases), 1)
  expect_equal(result$distribution, "normal")
  
  # Test case 2
  result <- design(n = 3, level = list(0, 1))
  expect_s3_class(result, "sc_design")
  expect_equal(length(result$cases), 3)
  expect_equal(result$distribution, "normal")
  expect_equal(result$cases[[1]]$level, c(0, 1))
  expect_equal(result$cases[[2]]$level, c(0, 1))
  
  # Test case 3
  result <- design(n = 2, trend = c(0, 1), start_value = c(50, 60))
  expect_s3_class(result, "sc_design")
  expect_equal(length(result$cases), 2)
  expect_equal(result$distribution, "normal")
  expect_equal(result$cases[[1]]$trend, c(0))
  expect_equal(result$cases[[1]]$start_value, 50)
  expect_equal(result$cases[[2]]$trend, c(1))
  expect_equal(result$cases[[2]]$start_value, 60)
  
})

rtt_of <- function(d) unname(sapply(d$cases, function(x) x$rtt[1]))
s_of <- function(d) unname(sapply(d$cases, function(x) x$s[1]))
lens_of <- function(d) lapply(d$cases, function(x) as.numeric(x$length))
range_of <- function(d) {
  lapply(d$cases, function(x) as.numeric(c(x$extreme_low, x$extreme_high)))
}

test_that("the error argument is translated into rtt", {
  d <- design(n = 3, s = 10, error = 5)
  expect_equal(rtt_of(d), rep(0.8, 3))
  expect_true(all(s_of(d) == 10))
  expect_identical(design(n = 3, s = 10, rtt = 0.8)$cases, d$cases)
  expect_equal(rtt_of(design(n = 2, s = 5, error = 5)), rep(0.5, 2))

  expect_equal(rtt_of(design(n = 4, s = 10, error = c(5, 10))),
               c(0.8, 0.5, 0.8, 0.5))
  expect_equal(rtt_of(design(n = 3, s = c(5, 10, 20), error = 5)),
               c(25, 100, 400) / (c(25, 100, 400) + 25))
  expect_equal(rtt_of(design(n = 2, s = 10, error = list(5, 10))), c(0.8, 0.5))

  expect_error(design(n = 2, rtt = 0.8, error = 5))
  expect_error(design(n = 2, error = -1))
  expect_error(design(n = 2, error = 0))
  expect_error(design(n = 2, error = "5"))
  expect_error(design(n = 2, error = NA))
  expect_no_error(design(n = 2, rtt = 0.6))
  expect_equal(rtt_of(design(n = 2)), rep(0.8, 2))
})

test_that("the simulated error has the requested size", {
  skip_on_cran()
  for (err in c(2, 5, 10)) {
    set.seed(123)
    x <- random_scdf(design(
      n = 60, phase_design = list(A = 20, B = 40), level = 1,
      s = 10, error = err
    ))
    resid_sd <- sqrt(mean(sapply(seq_along(x), function(i) {
      m <- plm(x[i])$full.model
      sum(residuals(m)^2) / (nrow(x[[i]]) - length(coef(m)))
    })))
    expect_lt(abs(resid_sd - err) / err, 0.05)
  }
})

test_that("phase lengths must be whole numbers of at least one", {
  expect_error(design(phase_design = list(A = 0, B = 20)))
  expect_error(design(phase_design = list(A = 20, B = 0)))
  expect_error(design(phase_design = list(A = 10, B = 0, C = 10)))
  expect_error(design(phase_design = list(A = -5, B = 20)))
  expect_error(design(phase_design = list(A = 5.5, B = 14.5)))
  expect_error(design(phase_design = list(A = NA, B = 20)))
  expect_error(design(n = 2, phase_design = list(A = c(5, 0), B = c(15, 20))))
  expect_error(design(B_start = 1))
  expect_error(design(B_start = 21, mt = 20))

  good <- list(
    list(A = 1, B = 19), list(A = 5, B = 15), list(A = 10, B = 20, C = 10),
    list(A1 = 5, B1 = 5, A2 = 5, B2 = 5), list(A = 50, B = 100)
  )
  for (g in good) {
    expect_no_error(d <- design(phase_design = g))
    expect_identical(lens_of(d)[[1]], as.numeric(unlist(g)))
    expect_no_error(random_scdf(d, seed = 1))
  }
  d <- design(n = 2, phase_design = list(A = c(5, 8), B = c(10, 12)))
  expect_identical(lens_of(d)[[2]], c(8, 12))
  expect_no_error(design(B_start = 2))
  expect_no_error(design(B_start = 20, mt = 20))
})

test_that("mt defaults to 20 and B_start uses it", {
  expect_identical(formals(design)$mt, 20)
  expect_identical(attr(design(n = 2), "call")$mt, 20)

  expect_no_warning(d <- design(B_start = 6))
  expect_identical(lens_of(d)[[1]], c(5, 15))

  d <- design(n = 3, B_start = c(6, 10, 14))
  expect_identical(lens_of(d)[[1]], c(5, 15))
  expect_identical(lens_of(d)[[2]], c(9, 11))
  expect_identical(lens_of(d)[[3]], c(13, 7))

  d <- design(n = 3, B_start = c(6, 10, 14), mt = 30)
  expect_true(all(vapply(lens_of(d), sum, 0) == 30))
  d <- design(n = 3, B_start = 6, mt = c(20, 25, 30))
  expect_identical(vapply(lens_of(d), sum, 0), c(20, 25, 30))
  expect_equal(sum(lens_of(design(B_start = 0.3, mt = 40))[[1]]), 40)

  # mt is ignored when phase_design is given
  d <- design(phase_design = list(A = 7, B = 13), mt = 100)
  expect_identical(lens_of(d)[[1]], c(7, 13))

  r <- random_scdf(design(n = 3, B_start = c(6, 10, 14)), seed = 1)
  expect_true(all(vapply(r, nrow, 1L) == 20L))
})

test_that("extreme_range is a pair given to every case", {
  for (n in 1:6) {
    d <- design(n = n, extreme_prop = 0.2)
    expect_true(all(vapply(range_of(d), identical, TRUE, c(-4, -3))))
    r <- random_scdf(d, seed = 1)
    expect_false(any(is.na(unlist(lapply(r, function(x) x$values)))))
  }
  for (n in 1:4) {
    d <- design(n = n, extreme_prop = 0.2, extreme_range = c(-5, -2))
    expect_true(all(vapply(range_of(d), identical, TRUE, c(-5, -2))))
  }
  d <- design(n = 4, extreme_prop = 0.2,
              extreme_range = list(c(-8, -7), c(3, 4)))
  expect_identical(range_of(d)[[1]], c(-8, -7))
  expect_identical(range_of(d)[[2]], c(3, 4))
  expect_identical(range_of(d)[[3]], c(-8, -7))
  expect_identical(range_of(d)[[4]], c(3, 4))

  # every measurement extreme and far below the start value
  d <- design(n = 2, extreme_prop = 1, extreme_range = c(-10, -9), s = 1,
              start_value = 50, trend = 0, level = list(0), slope = list(0))
  v <- unlist(lapply(random_scdf(d, seed = 1), function(x) x$values))
  expect_true(all(v < 45))
  expect_false(any(is.na(v)))
})

test_that("proportions are checked per case", {
  d <- design(n = 3, extreme_prop = c(0.1, 0.3, 0.5), extreme_range = c(-4, -3))
  expect_identical(vapply(d$cases, function(x) x$extreme_prop, 0),
                   c(0.1, 0.3, 0.5))
  d <- design(n = 3, missing_prop = c(0.1, 0.2, 0.3))
  expect_identical(vapply(d$cases, function(x) x$missing_prop, 0),
                   c(0.1, 0.2, 0.3))
  expect_identical(
    vapply(design(n = 3, extreme_prop = 0.2)$cases, function(x) x$extreme_prop, 0),
    rep(0.2, 3)
  )
  expect_no_error(design(n = 5, extreme_prop = c(0.1, 0.2)))
  expect_no_error(design(n = 2, extreme_prop = c(0, 1), missing_prop = c(1, 0)))
  for (bad in list(c(0.1, 1.5), c(-0.2, 0.3), 1.5, -0.1, c(0.1, NA))) {
    expect_error(design(n = 2, extreme_prop = bad))
  }
  # one message, not one per case
  m <- tryCatch(design(n = 3, missing_prop = c(0.1, 0.2, 1.4)),
                error = conditionMessage)
  expect_length(m, 1L)

  # the same check in the other functions, where it applies to scalars
  expect_error(corrected_tau(exampleAB$Johanna, alpha = 1.5))
  expect_no_error(corrected_tau(exampleAB$Johanna, alpha = 0.05))
  expect_error(cdc(exampleAB, conservative = 2))
  expect_no_error(cdc(exampleAB, conservative = 0.25))
  expect_error(rci(exampleAB$Johanna, rel = 1.2))
  expect_no_error(rci(exampleAB$Johanna, rel = 0.8))
  expect_error(between_smd(exampleAB, ci = 1.5))
  expect_no_error(between_smd(exampleAB, ci = 0.95))
})
