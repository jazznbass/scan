# select_phases()

test_that("phases are combined into two phases", {
  x <- select_phases(exampleA1B1A2B2, A = c(1, 3), B = c(2, 4))
  expect_s3_class(x, "scdf")
  expect_length(x, length(exampleA1B1A2B2))
  for (i in seq_along(x)) {
    expect_length(unique(as.character(x[[i]]$phase)), 2)
    expect_equal(nrow(x[[i]]), nrow(exampleA1B1A2B2[[i]]))
  }
  expect_no_error(overlap(x))
})

test_that("a subset of the phases keeps only those measurements", {
  x <- select_phases(exampleA1B1A2B2, A = 1, B = 2)
  for (i in seq_along(x)) {
    kept <- as.character(exampleA1B1A2B2[[i]]$phase) %in% c("A1", "B1")
    expect_equal(nrow(x[[i]]), sum(kept))
    expect_equal(x[[i]]$values, exampleA1B1A2B2[[i]]$values[kept])
  }
})

test_that("the phase names follow the argument", {
  x <- select_phases(exampleA1B1A2B2, A = c(1, 3), B = c(2, 4),
                     phase_names = c("base", "int"))
  expect_setequal(unique(as.character(x[[1]]$phase)), c("base", "int"))

  # the default generates a name from the combined phases
  x <- select_phases(exampleA1B1A2B2, A = c(1, 3), B = c(2, 4))
  expect_length(unique(as.character(x[[1]]$phase)), 2)
})

test_that("select_phases works in a pipe with the overlap functions", {
  expect_no_error(
    exampleA1B1A2B2_zvt |> select_phases(A = c(1, 3), B = c(2, 4)) |> overlap()
  )
  expect_no_error(
    exampleA1B1A2B2 |> select_phases(A = c(1, 3), B = c(2, 4)) |> nap()
  )
})
