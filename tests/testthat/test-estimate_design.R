test_that("estimate_design", {
  scdf <- exampleABC
  result <- estimate_design(scdf)
  expect_s3_class(result, "sc_design")
  expected_vars <- c(
    'phase', 'length', 'rtt', 'missing_prop', 'extreme_prop', 
    'extreme_low', 'extreme_high', 'trend', 'level', 'slope', 'start_value', 
    's', 'start', 'stop'
  )
  for (i in seq_along(result$cases)) {
    expect_equal(sort(names(result$cases[[i]])), sort(expected_vars))
  }
  
  value <- round(sum(unlist(lapply(result$cases, function(x)  unlist(x[-1])))), 4)
  expect_equal(value, 591.4265)  
})

rtt_of <- function(d) unname(sapply(d$cases, function(x) x$rtt[1]))

test_that("a given rtt or error is kept", {
  set.seed(4711)
  x <- random_scdf(design(
    n = 10, phase_design = list(A = 10, B = 15), level = 1, rtt = 0.8, s = 10
  ))
  for (o_rtt in c(TRUE, FALSE)) {
    e <- estimate_design(x, s = 10, rtt = 0.7, overall_rtt = o_rtt)
    expect_true(all(rtt_of(e) == 0.7))
  }
  e_err <- estimate_design(x, s = 10, error = 5)
  expect_equal(rtt_of(e_err), rep(0.8, 10))
  expect_identical(estimate_design(x, s = 10, rtt = 0.8)$cases, e_err$cases)

  expect_equal(rtt_of(estimate_design(x, s = 10, error = c(5, 10)))[1:4],
               c(0.8, 0.5, 0.8, 0.5))
  expect_equal(rtt_of(estimate_design(x, s = 10, rtt = c(0.6, 0.9)))[1:4],
               c(0.6, 0.9, 0.6, 0.9))

  expect_error(estimate_design(x, s = 10, rtt = 0.8, error = 5))
  expect_error(estimate_design(x, s = 10, error = -2))
  expect_error(estimate_design(x, s = 10, error = "5"))
  expect_error(estimate_design(x, s = 0))
  expect_error(estimate_design(x, s = "10"))
})

test_that("the reliability of the generating design is recovered", {
  skip_on_cran()
  for (r in c(0.5, 0.7, 0.8, 0.95)) {
    set.seed(1234)
    xx <- random_scdf(design(
      n = 30, phase_design = list(A = 15, B = 25), level = 1, trend = 0.02,
      rtt = r, s = 10
    ))
    expect_lt(abs(rtt_of(estimate_design(xx, s = 10, overall_rtt = TRUE))[1] - r), 0.05)
  }
})

test_that("the estimate does not follow the effect size", {
  skip_on_cran()
  res <- sapply(c(0, 0.5, 1, 2), function(lv) {
    set.seed(99)
    xx <- random_scdf(design(
      n = 30, phase_design = list(A = 15, B = 25), level = lv, rtt = 0.8, s = 10
    ))
    rtt_of(estimate_design(xx, s = 10))[1]
  })
  expect_true(all(abs(res - 0.8) < 0.05))
  expect_gt(res[1], 0.5)
})

test_that("without s the fallback warns and fixes the reliability at 0.5", {
  set.seed(7)
  studies <- list(
    one = random_scdf(design(n = 1, phase_design = list(A = 10, B = 15),
                             level = 1, s = 10)),
    two = random_scdf(design(n = 2, phase_design = list(A = 10, B = 15),
                             level = 1, s = 10))
  )
  for (obj in studies) {
    w <- character()
    d_fb <- withCallingHandlers(
      estimate_design(obj),
      warning = function(x) {
        w <<- c(w, conditionMessage(x))
        invokeRestart("muffleWarning")
      }
    )
    expect_gte(length(w), 2)
    expect_equal(rtt_of(d_fb), rep(0.5, length(d_fb$cases)))
    y <- suppressWarnings(random_scdf(d_fb))
    expect_false(any(is.na(y[[1]]$values)))
    expect_gt(sd(y[[1]]$values), 0)
  }

  d1 <- estimate_design(studies$one, s = 10)
  expect_equal(d1$cases[[1]]$s[1], 10)
  expect_length(d1$cases[[1]]$level, 2)
  expect_true(all(is.finite(d1$cases[[1]]$level)))
  expect_equal(nrow(random_scdf(d1)[[1]]), 25)
})

test_that("identical start values are warned about but do not stop the estimate", {
  set.seed(5)
  flat <- random_scdf(design(
    n = 4, phase_design = list(A = 10, B = 15), level = 1, s = 10, rtt = 0.5
  ))
  count_warnings <- function(expr) {
    w <- character()
    withCallingHandlers(expr, warning = function(x) {
      w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
    length(w)
  }
  expect_equal(count_warnings(d_flat <- estimate_design(flat)), 3)
  expect_equal(rtt_of(d_flat), rep(0.5, 4))
  expect_equal(count_warnings(d2 <- estimate_design(flat, rtt = 0.9)), 2)
  expect_true(all(rtt_of(d2) == 0.9))
  expect_equal(count_warnings(estimate_design(flat, s = 10)), 0)
})

test_that("the remaining parameters of the design are estimated", {
  skip_on_cran()
  set.seed(123)
  x <- random_scdf(design(
    n = 5, phase_design = list(A = 10, B = 15), level = 1, trend = 0.02,
    s = 10, rtt = 0.8
  ))
  e <- estimate_design(x, s = 10)
  expect_length(e$cases, 5)
  expect_s3_class(e, "sc_design")
  expect_true(all(sapply(e$cases, function(cs) {
    identical(as.numeric(cs$length), c(10, 15))
  })))
  expect_false(any(sapply(e$cases, function(cs) "var_residuals" %in% names(cs))))
  expect_false(any(sapply(e$cases, function(cs) any(is.na(names(cs))))))
  expect_lt(abs(mean(sapply(e$cases, function(cs) cs$level[2])) - 1), 0.3)
  expect_lt(abs(mean(sapply(e$cases, function(cs) cs$trend[1])) - 0.02), 0.05)

  e2 <- estimate_design(x, s = 10, overall_effects = TRUE)
  lv <- sapply(e2$cases, function(cs) cs$level[2])
  expect_length(unique(round(lv, 10)), 1)
})

test_that("the estimated s corrects for the standard error of the start value", {
  skip_on_cran()
  set.seed(2024)
  xtrue <- random_scdf(design(
    n = 40, phase_design = list(A = 15, B = 25), level = 1, trend = 0.02,
    rtt = 0.8, s = 10, random_start_value = TRUE
  ))
  e_free <- estimate_design(xtrue)
  b0 <- sapply(seq_along(xtrue), function(i) {
    coef(plm(xtrue[i], model = "JW")$full.model)[1]
  })
  expect_lte(abs(e_free$cases[[1]]$s[1] - 10), abs(sd(b0) - 10))
  expect_lt(abs(rtt_of(e_free)[1] - 0.8), 0.05)
})

test_that("the estimated error reaches the simulated data", {
  skip_on_cran()
  set.seed(31)
  xx <- random_scdf(design(
    n = 20, phase_design = list(A = 15, B = 25), level = 1, s = 10, rtt = 0.8,
    random_start_value = TRUE
  ))
  y <- random_scdf(estimate_design(xx, s = 10, error = 4))
  resid_sd <- sqrt(mean(sapply(seq_along(y), function(i) {
    m <- plm(y[i])$full.model
    sum(residuals(m)^2) / (nrow(y[[i]]) - length(coef(m)))
  })))
  expect_lt(abs(resid_sd - 4) / 4, 0.1)
})
