# outlier(): phase runs, missing values, the reported matrices

# reference implementation: bounds per phase run, missing values ignored
ref_drop <- function(case, method, criteria) {
  runs <- rle(as.character(case$phase))
  id <- rep(seq_along(runs$values), runs$lengths)
  drop <- logical(nrow(case))
  for (p in seq_along(runs$values)) {
    x <- case$values[id == p]
    if (method == "SD") {
      lo <- mean(x, na.rm = TRUE) - criteria * sd(x, na.rm = TRUE)
      hi <- mean(x, na.rm = TRUE) + criteria * sd(x, na.rm = TRUE)
    }
    if (method == "MAD") {
      lo <- median(x, na.rm = TRUE) - criteria * mad(x, na.rm = TRUE)
      hi <- median(x, na.rm = TRUE) + criteria * mad(x, na.rm = TRUE)
    }
    if (method == "CI") {
      fac <- qnorm((1 - criteria) / 2, lower.tail = FALSE)
      se <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
      lo <- mean(x, na.rm = TRUE) - fac * se
      hi <- mean(x, na.rm = TRUE) + fac * se
    }
    d <- x < lo | x > hi
    d[is.na(d)] <- FALSE
    drop[id == p] <- d
  }
  case$mt[drop]
}

expect_outlier_consistent <- function(res, study, i, method, criteria) {
  case <- study[[i]]
  got <- res$dropped.mt[[i]]
  want <- ref_drop(case, method, criteria)
  kept <- res$data[[i]]
  expect_false(is.na(res$dropped.n[[i]]))
  expect_equal(nrow(kept) + res$dropped.n[[i]], nrow(case))
  expect_equal(res$dropped.n[[i]], length(got))
  expect_false(any(is.na(got)))
  expect_false(any(is.na(kept$mt)))
  expect_equal(as.numeric(got), as.numeric(want))
  expect_equal(as.numeric(kept$mt), as.numeric(setdiff(case$mt, want)))
}

abab_study <- function(v1, v2) {
  c(
    scdf(values = v1, phase_design = c(A = 6, B = 6, A = 6, B = 6), name = "c1"),
    scdf(values = v2, phase_design = c(A = 6, B = 6, A = 6, B = 6), name = "c2")
  )
}

v1 <- c(10, 11, 9, 10, 12, 30,  20, 21, 19, 22, 20, 21,
        10, 9, 11, 10, 10, 11,  21, 20, 22, 20, 1, 21)
v2 <- c(14, 15, 13, 15, 14, 15,  25, 40, 24, 26, 25, 24,
        14, 13, 15, 14, 15, 14,  26, 25, 24, 26, 25, 24)

test_that("outlier splits by phase run, not by phase label", {
  study <- abab_study(v1, v2)

  # the test data really do repeat phase labels
  expect_equal(length(rle(as.character(study[[1]]$phase))$values), 4)
  expect_equal(length(unique(as.character(study[[1]]$phase))), 2)

  for (m in c("SD", "MAD", "CI")) {
    crit <- switch(m, SD = 1.5, MAD = 3.5, CI = 0.99)
    res <- outlier(study, method = m, criteria = crit)
    for (i in 1:2) expect_outlier_consistent(res, study, i, m, crit)
  }

  res <- outlier(study, method = "SD", criteria = 1.5)
  expect_true(sum(unlist(res$dropped.n)) > 0)
  expect_equal(nrow(res$sd.matrix[[1]]), 4)
  expect_identical(as.character(res$sd.matrix[[1]]$phase), c("A", "B", "A", "B"))
  expect_equal(nrow(outlier(study, method = "CI", criteria = 0.99)$ci.matrix[[1]]), 4)
  expect_equal(nrow(outlier(study, method = "MAD", criteria = 3.5)$mad.matrix[[1]]), 4)
  expect_identical(res$method, "SD")
})

test_that("outlier handles twelve phase runs in the right order", {
  set.seed(1)
  v <- as.numeric(replicate(12, rnorm(5, mean = 10)))
  v[58] <- 40
  many <- scdf(
    values = v,
    phase_design = setNames(rep(5, 12), rep(c("A", "B"), 6))
  )
  res <- outlier(many, method = "SD", criteria = 1.5)
  expect_outlier_consistent(res, many, 1, "SD", 1.5)
  expect_equal(nrow(res$sd.matrix[[1]]), 12)
  expect_identical(as.character(res$sd.matrix[[1]]$phase), rep(c("A", "B"), 6))
})

test_that("a plain AB design is unaffected", {
  ab <- scdf(values = v1[1:12], phase_design = c(A = 6, B = 6), name = "ab")
  for (m in c("SD", "MAD", "CI")) {
    crit <- switch(m, SD = 1.5, MAD = 3.5, CI = 0.99)
    res <- outlier(ab, method = m, criteria = crit)
    expect_outlier_consistent(res, ab, 1, m, crit)
  }
  expect_no_error(outlier(ab, criteria = c("SD", 2)))
})

test_that("outlier ignores missing values but keeps them in the data", {
  na_study <- abab_study(replace(v1, c(3, 10), NA), replace(v2, 11, NA))

  for (m in c("SD", "MAD", "CI")) {
    crit <- switch(m, SD = 1.5, MAD = 3.5, CI = 0.99)
    res <- outlier(na_study, method = m, criteria = crit)
    for (i in 1:2) {
      expect_outlier_consistent(res, na_study, i, m, crit)
      expect_equal(
        sum(is.na(res$data[[i]]$values)),
        sum(is.na(na_study[[i]]$values))
      )
    }
  }
})

test_that("a phase without any valid value does not break outlier", {
  dead <- scdf(
    values = c(10, 11, 9, 10, 12, 30, NA, NA, NA, NA, NA, NA),
    phase_design = c(A = 6, B = 6), name = "dead"
  )
  for (m in c("SD", "MAD", "CI")) {
    crit <- switch(m, SD = 1.5, MAD = 3.5, CI = 0.99)
    expect_no_error(res <- outlier(dead, method = m, criteria = crit))
    expect_equal(sum(is.na(res$data[[1]]$values)), 6)
    expect_false(is.na(res$dropped.n[[1]]))
  }
})

test_that("the sd matrix reproduces mean, sd and bounds of a phase run", {
  study <- abab_study(v1, v2)
  res <- outlier(study, method = "SD", criteria = 1.5)
  m1 <- res$sd.matrix[[1]]
  x1 <- study[[1]]$values[1:6]
  expect_equal(m1$m[1], mean(x1))
  expect_equal(m1$sd[1], sd(x1))
  expect_equal(
    c(m1$lower[1], m1$upper[1]),
    c(mean(x1) - 1.5 * sd(x1), mean(x1) + 1.5 * sd(x1))
  )
})

test_that("the mad matrix holds the median and the scaled mad", {
  studies <- list(
    complete = abab_study(v1, v2),
    missing = abab_study(replace(v1, c(3, 10), NA), replace(v2, 11, NA))
  )
  for (study in studies) {
    for (criteria in c(2, 3.5, 5)) {
      res <- outlier(study, method = "MAD", criteria = criteria)
      for (i in seq_along(study)) {
        case <- study[[i]]
        r <- rle(as.character(case$phase))
        id <- rep(seq_along(r$values), r$lengths)
        mat <- res$mad.matrix[[i]]
        expect_equal(
          as.numeric(mat$md),
          as.numeric(tapply(case$values, id, median, na.rm = TRUE))
        )
        expect_equal(
          as.numeric(mat$mad),
          as.numeric(tapply(case$values, id, mad, na.rm = TRUE))
        )
        expect_equal(
          c(mat$lower, mat$upper),
          c(mat$md - criteria * mat$mad, mat$md + criteria * mat$mad)
        )
      }
    }
  }
})

test_that("Cook distances line up with the measurements", {
  na_study <- abab_study(replace(v1, c(3, 10), NA), replace(v2, 11, NA))
  expect_no_error(res <- outlier(na_study, method = "Cook", criteria = "4/n"))
  for (i in 1:2) {
    n <- nrow(na_study[[i]])
    cd <- res$cook[[i]]
    expect_equal(nrow(cd), n)
    expect_false(any(is.na(cd$MT)))
    expect_identical(is.na(cd$Cook), is.na(na_study[[i]]$values))
    want <- na_study[[i]]$mt[which(!is.na(cd$Cook) & cd$Cook >= 4 / n)]
    expect_equal(as.numeric(res$dropped.mt[[i]]), as.numeric(want))
    expect_equal(nrow(res$data[[i]]) + res$dropped.n[[i]], n)
  }

  full <- abab_study(replace(v1, is.na(v1), 11), v2)
  res <- outlier(full, method = "Cook", criteria = "4/n")
  for (i in 1:2) {
    n <- nrow(full[[i]])
    cd <- cooks.distance(plm(full[i])$full.model)
    expect_equal(as.numeric(res$cook[[i]]$Cook), as.numeric(round(cd, 2)))
    expect_equal(as.numeric(res$dropped.mt[[i]]), as.numeric(full[[i]]$mt[cd >= 4 / n]))
  }
})

test_that("print and export of an outlier object run", {
  study <- abab_study(replace(v1, c(3, 10), NA), v2)
  for (m in c("SD", "MAD", "CI", "Cook")) {
    crit <- switch(m, SD = 1.5, MAD = 3.5, CI = 0.99, Cook = "4/n")
    expect_no_error(capture.output(print(outlier(study, method = m, criteria = crit))))
  }
  expect_no_error(export(outlier(exampleAB, method = "SD")))
})

test_that("the criterion is worded once for print and export", {
  for (m in c("SD", "MAD", "CI", "Cook")) {
    crit <- switch(m, SD = 1.5, MAD = 3.5, CI = 0.99, Cook = "4/n")
    res <- outlier(exampleAB, method = m, criteria = crit)
    out <- scan:::.output_outlier(res)

    expect_match(out$criterion, "^Criterion: ")
    txt <- paste(capture.output(print(res)), collapse = "\n")
    expect_true(grepl(out$criterion, txt, fixed = TRUE))
    expect_true(grepl(out$criterion, render_table(export(res)), fixed = TRUE))

    # the matrix of bounds belongs to the method that has one
    if (identical(m, "Cook")) {
      expect_null(out$matrix)
    } else {
      expect_identical(names(out$matrix), names(exampleAB))
    }

    expect_identical(out$dropped$Case, res$case.names)
    expect_equal(out$dropped$Dropped, unlist(res$dropped.n))
  }
})
