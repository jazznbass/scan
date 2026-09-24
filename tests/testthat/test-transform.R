# transform.scdf() and its helper functions

test_that("transform adds and replaces variables case by case", {
  x <- transform(exampleAB, doubled = values * 2)
  expect_s3_class(x, "scdf")
  expect_true("doubled" %in% names(x[[1]]))
  expect_equal(x[[1]]$doubled, exampleAB[[1]]$values * 2)

  x <- transform(exampleAB, mean_values = mean(values))
  expect_equal(x[[1]]$mean_values[1], mean(exampleAB[[1]]$values))
  expect_false(isTRUE(all.equal(x[[1]]$mean_values[1], x[[2]]$mean_values[1])))

  # a variable created in one expression is available in the next
  x <- transform(exampleAB, a = values * 2, b = a + 1)
  expect_equal(x[[1]]$b, exampleAB[[1]]$values * 2 + 1)
})

test_that("the expression is evaluated in the calling environment", {
  f <- function() {
    factor <- 3
    transform(exampleAB, scaled = values * factor)
  }
  expect_equal(f()[[1]]$scaled, exampleAB[[1]]$values * 3)
})

test_that("across_cases works on the values of all cases", {
  x <- transform(exampleABC, across_cases(ranks = rank(values, na.last = "keep")))
  all_values <- unlist(lapply(exampleABC, function(case) case$values))
  all_ranks <- unlist(lapply(x, function(case) case$ranks), use.names = FALSE)
  expect_equal(all_ranks, unname(rank(all_values, na.last = "keep")))
})

test_that("the documented examples run", {
  expect_no_error(
    exampleAB |> transform(values = scale(values), sd_values = sd(values))
  )
  expect_no_error(
    Huber2014$Berta |> transform(
      smooth_median = moving_median(compliance),
      smooth_mean = moving_mean(compliance),
      smooth_local = local_regression(compliance, mt)
    )
  )
  expect_no_error(
    byHeart2011 |> transform(
      values = set_na_at(values, phase == "A", 0:1),
      values = set_na_at(values, phase == "B", -1:0)
    )
  )
})

test_that("set_na_at sets the positions it is given", {
  x <- c(10, 20, 30, 40, 50)
  at <- c(FALSE, TRUE, FALSE, FALSE, FALSE)
  expect_equal(set_na_at(x, at), c(10, NA, 30, 40, 50))
  expect_equal(set_na_at(x, at, positions = 0:1), c(10, NA, NA, 40, 50))
  expect_equal(set_na_at(x, at, positions = -1:0), c(NA, NA, 30, 40, 50))
  # positions outside the series are dropped instead of extending it
  expect_equal(set_na_at(x, c(TRUE, rep(FALSE, 4)), positions = c(0, 9)),
               c(NA, 20, 30, 40, 50))
  expect_equal(set_na_at(x, c(TRUE, rep(FALSE, 4)), positions = c(-2, 0)),
               c(NA, 20, 30, 40, 50))
  expect_length(set_na_at(x, at, positions = 0:1), 5)
})

test_that("center_at centres at the position it is told to", {
  x <- c(10, 12, 14)
  expect_equal(center_at(x, at = c(TRUE, FALSE, FALSE)), c(0, 2, 4))
  expect_equal(center_at(x, at = c(TRUE, TRUE, TRUE), part = 1), c(-4, -2, 0))
  expect_equal(center_at(x, at = c(TRUE, TRUE, TRUE), part = 0.5), c(-2, 0, 2))
  expect_equal(center_at(x, at = c(TRUE, FALSE, FALSE), shift = 1), c(-2, 0, 2))
})

test_that("a series shorter than the smoothing window is returned unchanged", {
  expect_warning(v <- moving_mean(c(1, 2), lag = 3), "too short")
  expect_equal(v, c(1, 2))
  expect_warning(v <- moving_median(c(1, 2, 3), lag = 2), "too short")
  expect_equal(v, c(1, 2, 3))
})

test_that("the smoothing window averages the original series", {
  x <- c(0, 0, 3, 0, 0)
  expect_equal(moving_mean(x, lag = 1), c(0, 1, 1, 1, 0))
  expect_equal(moving_median(x, lag = 1), c(0, 0, 0, 0, 0))

  # the first and last lag values are left unchanged
  expect_equal(moving_mean(c(1, 2, 3, 4, 5, 6, 7), lag = 2),
               c(1, 2, 3, 4, 5, 6, 7))
  expect_equal(moving_mean(c(1, NA, 3, 4, 5), lag = 1), c(1, 2, 3.5, 4, 5))

  # a window that reads its own results is not symmetric: smoothing a reversed
  # series must give the reversed smoothed series
  y <- c(0, 0, 3, 0, 0, 7, 1, 2, 2, 9)
  expect_equal(moving_mean(y, lag = 1), rev(moving_mean(rev(y), lag = 1)))
  expect_equal(moving_median(y, lag = 2), rev(moving_median(rev(y), lag = 2)))
})

test_that("local_regression returns one value per measurement", {
  x <- c(1, 3, 2, 5, 4, 6, 5, 8)
  expect_length(local_regression(x), length(x))
  expect_length(local_regression(x, mt = seq_along(x), f = 0.5), length(x))
  expect_false(any(is.na(local_regression(x))))
})
