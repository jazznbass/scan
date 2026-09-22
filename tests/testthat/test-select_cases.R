# select_cases(): names, numbers, negative selections

test_that("cases are selected by name, number or variable", {
  study <- exampleAB
  expect_identical(names(select_cases(study, Johanna, Karolina)),
                   c("Johanna", "Karolina"))
  expect_identical(names(select_cases(study, c(Johanna, Karolina))),
                   c("Johanna", "Karolina"))
  expect_identical(names(select_cases(study, 1, 2)), c("Johanna", "Karolina"))
  expect_identical(names(select_cases(study, 1:2)), c("Johanna", "Karolina"))
  expect_identical(names(select_cases(study, "Johanna")), "Johanna")
  expect_identical(names(select_cases(study, c("Johanna", "Anja"))),
                   c("Johanna", "Anja"))
  expect_identical(names(select_cases(study, "Johanna", 3)),
                   c("Johanna", "Anja"))
  expect_identical(names(select_cases(study, Johanna, 3)), c("Johanna", "Anja"))
  expect_identical(names(select_cases(study, 3, 1)), c("Anja", "Johanna"))
  v <- c("Moritz", "Jannis")
  expect_identical(names(select_cases(exampleA1B1A2B2, v)), v)
})

test_that("negative selections drop cases", {
  study <- exampleAB
  expect_identical(names(select_cases(study, -Johanna)), c("Karolina", "Anja"))
  expect_identical(names(select_cases(study, -c(Johanna, Karolina))), "Anja")
  expect_identical(names(select_cases(study, -Johanna, -Karolina)), "Anja")
  expect_identical(names(select_cases(study, -1, -2)), "Anja")
  expect_identical(names(select_cases(study, -Johanna, -Johanna)),
                   c("Karolina", "Anja"))
  expect_length(select_cases(study, -1, -2, -3), 0)
  expect_identical(as.data.frame(select_cases(study, -Johanna, -Karolina)),
                   as.data.frame(study[3]))
})

test_that("an impossible selection is rejected", {
  study <- exampleAB
  expect_error(select_cases(study))
  expect_error(select_cases(study, "Fritz"))
  expect_error(select_cases(study, Fritz))
  expect_error(select_cases(study, 7))
  expect_error(select_cases(study, -1, 2))
  expect_error(select_cases(study, -Johanna, Anja))
})

test_that("the selection is evaluated in the calling environment", {
  study <- exampleAB
  f <- function() {
    w <- c("Johanna", "Anja")
    select_cases(study, w)
  }
  expect_identical(names(f()), c("Johanna", "Anja"))
  g <- function(sel) select_cases(study, sel)
  expect_identical(names(g(c(1, 3))), c("Johanna", "Anja"))
  h <- function() {
    drop <- 1
    select_cases(study, -drop)
  }
  expect_identical(names(h()), c("Karolina", "Anja"))
})

test_that("the result is a valid scdf", {
  study <- exampleAB
  r <- select_cases(study, -Johanna, -Karolina)
  expect_s3_class(r, "scdf")
  expect_identical(c(dv(r), phase(r), mt(r)),
                   c(dv(study), phase(study), mt(study)))
  expect_no_error(describe(r))
})
