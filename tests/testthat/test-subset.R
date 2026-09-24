# subset.scdf()

study <- scdf(
  values = c(1, 2, 3, 4, 5, 11, 12, 13, 14, 15),
  phase_design = c(A = 5, B = 5), name = "a"
)

test_that("rows are filtered by a logical expression", {
  expect_equal(nrow(subset(study, values > 3)[[1]]), 7)
  expect_equal(subset(study, phase == "A")[[1]]$values, c(1, 2, 3, 4, 5))
  expect_s3_class(subset(study, values > 3), "scdf")
  # an expression that keeps nothing gives an empty case, not an error
  expect_equal(nrow(subset(study, values > 100)[[1]]), 0)
})

test_that("missing values in the filter count as FALSE", {
  na_study <- study
  na_study[[1]]$values[2] <- NA
  kept <- subset(na_study, values > 3)[[1]]
  expect_equal(nrow(kept), 7)
  expect_false(any(is.na(kept$values)))
})

test_that("variables are selected and dropped", {
  x <- subset(exampleAB_add, select = c(-cigarrets, -depression))
  expect_false(any(c("cigarrets", "depression") %in% names(x[[1]])))
  expect_identical(
    names(x[[1]]),
    setdiff(names(exampleAB_add[[1]]), c("cigarrets", "depression"))
  )
  # the variables the analysis needs are still there
  expect_true(all(
    c(dv(exampleAB_add), phase(exampleAB_add), mt(exampleAB_add)) %in%
      names(x[[1]])
  ))
  x <- subset(study, select = c(values, phase))
  expect_identical(names(x[[1]]), c("values", "phase"))
})

test_that("cases are selected by name and by range", {
  expect_identical(names(subset(exampleAB, cases = c(Karolina, Johanna))),
                   c("Karolina", "Johanna"))
  expect_identical(names(subset(exampleAB, cases = 1)), "Johanna")
  ids <- match(c("Pawel", "Moritz"), names(exampleA1B1A2B2))
  expect_identical(
    names(subset(exampleA1B1A2B2, cases = Pawel:Moritz)),
    names(exampleA1B1A2B2)[ids[1]:ids[2]]
  )
})

test_that("rows, variables and cases can be filtered in one call", {
  x <- subset(exampleA1B1A2B2, phase %in% c("A1", "B2"), cases = Pawel:Moritz)
  ids <- match(c("Pawel", "Moritz"), names(exampleA1B1A2B2))
  expect_length(x, length(ids[1]:ids[2]))
  for (case in x) {
    expect_true(all(as.character(case$phase) %in% c("A1", "B2")))
  }
  expect_s3_class(x, "scdf")
  expect_no_error(describe(x))
})
