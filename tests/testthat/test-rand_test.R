test_that("rand_test", {
  
  scdf <- exampleAB
  
  results <- rand_test(scdf, complete = TRUE)

  expect_identical(
    round(
      with(
        results, 
        p.value + observed.statistic + Z + p.Z.single + mean(distribution)
      ), 
    8),
    40.97299008
  )
  
  userstat <- list(
    statistic = function(a, b) sum(b)/length(b) - sum(a)/length(a), 
    aggregate = function(x) sum(x)/length(x),
    name = "mean B - A"
  )
  
  results_user <- rand_test(
    exampleAB, statistic_function = userstat , complete = TRUE
  )
  
  results_user$statistic <- results$statistic
  expect_identical(results_user, results)
  
})

test_that("all statistics run and the mirrored ones mirror each other", {
  stats <- c("Mean B-A", "Mean A-B", "Median B-A", "Median A-B",
             "Mean |A-B|", "Median |A-B|", "SMD hedges", "SMD glass",
             "W-test", "T-test", "NAP", "NAP decreasing",
             "Slope B-A", "Slope A-B")
  res <- list()
  for (s in stats) {
    res[[s]] <- rand_test(exampleAB, statistic = s, number = 100, seed = 1)
    expect_true(is.finite(res[[s]]$p.value))
  }
  for (p in list(c("Mean B-A", "Mean A-B"), c("Median B-A", "Median A-B"),
                 c("Slope B-A", "Slope A-B"))) {
    expect_equal(res[[p[1]]]$observed.statistic,
                 -res[[p[2]]]$observed.statistic)
  }
  # a rising series: B-A is the significant direction
  expect_lt(res[["Slope B-A"]]$p.value, res[["Slope A-B"]]$p.value)
  expect_equal(res[["Slope B-A"]]$observed.statistic, 1.521429,
               tolerance = 1e-6)

  # a falling series turns it around
  rb <- rand_test(exampleAB_decreasing, statistic = "Slope B-A",
                  number = 100, seed = 1)
  ra <- rand_test(exampleAB_decreasing, statistic = "Slope A-B",
                  number = 100, seed = 1)
  expect_equal(rb$observed.statistic, -ra$observed.statistic)
  expect_lt(ra$p.value, rb$p.value)
})

test_that("the slope statistic is registered under the name that is used", {
  expect_true(is.function(scan:::.opt$rand_test$slope))
  expect_null(scan:::.opt$rand_test$statistic_slope)
  expect_false(exists("statistic", envir = asNamespace("scan"), inherits = FALSE))
  expect_error(scan:::rand_test_statistic(
    rnd_a = list(list(1:5)), rnd_b = list(list(6:10)),
    a = list(1:5), b = list(6:10),
    statistic = NULL, args_statistic = list(method = "B-A"),
    aggregate = function(x) sum(x) / length(x)
  ))
})

test_that("a degenerate randomization distribution gives NA, not a p value", {
  flat <- scdf(values = rep(5, 20), phase_design = c(A = 10, B = 10),
               name = "flat")
  for (s in c("SMD hedges", "SMD glass")) {
    expect_warning(
      r <- rand_test(flat, statistic = s, number = 100, seed = 1),
      "finite"
    )
    expect_true(is.na(r$p.value))
  }

  const_a <- scdf(values = c(rep(5, 10), 6, 8, 7, 9, 10, 8, 11, 9, 12, 10),
                  phase_design = c(A = 10, B = 10), name = "constA")
  expect_warning(
    r <- rand_test(const_a, statistic = "SMD glass", number = 100, seed = 1),
    "finite"
  )
  expect_true(is.na(r$p.value))
  expect_length(r$distribution, r$number)

  # a finite observed statistic with degenerate permutations
  mix <- scdf(
    values = c(rep(5, 5), 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
    phase_design = c(A = 10, B = 10), name = "mix"
  )
  expect_warning(
    r <- rand_test(mix, statistic = "SMD glass", number = 100, seed = 1)
  )
  expect_true(is.finite(r$observed.statistic))
  expect_gt(sum(!is.finite(r$distribution)), 0)
  expect_true(is.na(r$p.value))

  # the T-test no longer aborts on constant data
  expect_no_error(
    r <- suppressWarnings(rand_test(flat, statistic = "T-test", number = 100,
                                    seed = 1))
  )
  expect_true(is.na(r$p.value))

  # a constant distribution gives NA, not NaN
  r <- rand_test(flat, number = 100, seed = 1)
  expect_equal(r$p.value, 1)
  expect_true(is.na(r$Z))
  expect_false(is.nan(r$Z))
  expect_true(is.na(r$p.Z.single))
  expect_false(is.nan(r$p.Z.single))

  for (s in c("Mean B-A", "SMD hedges", "SMD glass", "T-test")) {
    r <- suppressWarnings(rand_test(flat, statistic = s, number = 100, seed = 1))
    expect_no_error(capture.output(print(r)))
    # a distribution without a single finite value leaves the cells empty
    # instead of reporting Inf, NaN or a warning from min() and max()
    expect_no_warning(tab <- export(r))
    txt <- as.character(gt::as_raw_html(tab))
    for (bad in c("Inf", "NaN", "NA")) {
      expect_false(grepl(paste0(">", bad, "<"), txt, fixed = TRUE))
    }
  }
})

test_that("limit and startpoints are validated", {
  short <- scdf(values = 1:8, phase_design = c(A = 3, B = 5), name = "short")
  ten <- scdf(values = 1:10, phase_design = c(A = 4, B = 6), name = "ten")
  d2 <- exampleAB[1:2]
  pts <- function(r, case = 1) {
    sort(unique(as.matrix(r$distribution_startpoints)[, case]))
  }

  # 8 measurements are not enough for these limits
  for (l in list(5, c(2, 7), c(7, 2))) {
    expect_error(rand_test(short, limit = l, number = 50, seed = 1))
  }
  # limit = 4 needs exactly the 8 measurements that are there
  expect_no_error(rand_test(short, limit = 4, number = 50, seed = 1))
  r <- rand_test(short, limit = 3, number = 50, seed = 1)
  expect_identical(as.numeric(pts(r)), c(4, 5, 6))
  r <- rand_test(ten, limit = 5, number = 50, seed = 1)
  expect_identical(as.numeric(pts(r)), 6)

  for (l in list(0, -1, c(0, 5))) {
    expect_error(rand_test(exampleAB$Johanna, limit = l, number = 50, seed = 1))
  }

  r <- rand_test(d2, startpoints = list(4:9, 12:16), number = 50, seed = 1)
  expect_true(all(pts(r, 1) %in% 4:9))
  expect_true(all(pts(r, 2) %in% 12:16))
  expect_false(identical(pts(r, 1), pts(r, 2)))
  expect_error(rand_test(d2, startpoints = list(4:9), number = 50, seed = 1))
  expect_error(rand_test(d2, startpoints = list(4:9, 5:10, 6:11),
                         number = 50, seed = 1))
  for (sp in list(c(3, 25), 1, 0, 21, c(2, 21))) {
    expect_error(rand_test(d2, startpoints = sp, number = 50, seed = 1))
  }
  r <- rand_test(d2, startpoints = 4:9, number = 50, seed = 1)
  expect_true(all(pts(r, 1) %in% 4:9) && all(pts(r, 2) %in% 4:9))
  expect_no_error(rand_test(d2, startpoints = c(2, 20), number = 50, seed = 1))
})

test_that("sampled start points respect a single admissible point", {
  ten <- scdf(values = 1:10, phase_design = c(A = 4, B = 6), name = "ten")
  mix <- c(ten, exampleAB$Johanna, exampleAB$Karolina)
  names(mix) <- c("ten", "a", "b")
  r <- rand_test(mix, limit = 5, number = 50, seed = 1, complete = FALSE)
  pts <- function(case) sort(unique(as.matrix(r$distribution_startpoints)[, case]))
  expect_false(r$complete)
  expect_identical(as.numeric(pts(1)), 6)
  expect_false(any(pts(1) %in% 1:5))
  expect_gt(length(pts(2)), 1)
  expect_gt(length(pts(3)), 1)
  expect_true(all(pts(2) %in% 6:16))
})

test_that("the ordinary results are unchanged", {
  r <- rand_test(exampleAB, number = 100, seed = 1)
  expect_equal(r$p.value, 0.03)
  expect_equal(r$observed.statistic, 20.555556, tolerance = 1e-5)
  expect_equal(round(r$Z, 4), 1.7122)
  expect_equal(round(r$p.Z.single, 4), 0.0434)
  expect_no_error(rand_test(exampleAB, complete = TRUE))
  expect_no_error(rand_test(exampleAB$Johanna, number = 100, seed = 1))
  expect_no_error(rand_test(exampleAB, exclude.equal = TRUE, number = 100, seed = 1))
  expect_no_error(rand_test(exampleAB, exclude.equal = "auto", number = 100, seed = 1))
  expect_no_error(rand_test(exampleABC, phases = c(1, 3), number = 100, seed = 1))
  expect_no_error(rand_test(exampleAB_score$Christiano, statistic = "W-test"))
})
