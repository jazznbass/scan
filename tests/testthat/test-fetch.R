# fetch()

test_that("fetch returns the model of a regression object", {
  m <- plm(exampleAB$Johanna)
  expect_identical(fetch(m), m$full.model)
  expect_identical(fetch(m, "model"), m$full.model)
  expect_s3_class(fetch(m), "lm")

  h <- hplm(exampleAB)
  expect_identical(fetch(h), h$hplm)
  expect_s3_class(fetch(h), "lme")

  mp <- mplm(exampleAB_add, dvar = c("wellbeing", "cigarrets"))
  expect_identical(fetch(mp), mp$full.model)
})

test_that("an unsupported what is reported instead of returning NULL", {
  m <- plm(exampleAB$Johanna)
  expect_error(fetch(m, "data"))
  expect_error(fetch(m, "Model"))
  expect_error(fetch(m, ""))
  expect_error(fetch(m, c("model", "model")))
  expect_error(fetch(m, 1))
  expect_error(fetch(m, NULL))
})
