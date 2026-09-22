# sample_names()

test_that("the documented types draw from their own list", {
  pool <- scan:::case_names
  for (ty in c("neutral", "female", "male", "mixed")) {
    x <- sample_names(5, type = ty)
    from <- switch(ty,
      neutral = pool$neutral, female = pool$female,
      male = pool$male, mixed = pool$all
    )
    expect_length(x, 5)
    expect_type(x, "character")
    expect_true(all(x %in% from))
    expect_false(anyDuplicated(x) > 0)
  }
  x <- sample_names()
  expect_length(x, 1)
  expect_true(x %in% pool$neutral)
  expect_length(sample_names(0), 0)
  expect_type(sample_names(0), "character")
})

test_that("the seed makes the draw reproducible", {
  expect_identical(sample_names(4, seed = 17), sample_names(4, seed = 17))
  expect_false(identical(sample_names(4, seed = 17), sample_names(4, seed = 18)))
  expect_identical(
    sample_names(3, type = "male", seed = 1),
    sample_names(3, type = "male", seed = 1)
  )
})

test_that("an unknown type is rejected", {
  for (ty in list("Male", "m", "gemischt", "", TRUE)) {
    expect_error(sample_names(3, type = ty))
  }
  expect_error(sample_names(length(scan:::case_names$neutral) + 1))
  expect_error(random_scdf(n = 3, random_names = "Male"))
})
