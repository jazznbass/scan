# read_scdf() and write_scdf()

values_of <- function(x) {
  unlist(lapply(x, function(case) case$values), use.names = FALSE)
}
phases_of <- function(x) {
  unlist(lapply(x, function(case) as.character(case$phase)), use.names = FALSE)
}

test_that("an scdf survives a csv round trip", {
  file <- tempfile(fileext = ".csv")
  write_scdf(exampleAB, file)
  expect_true(file.exists(file))

  expect_message(dat <- read_scdf(file), "Imported 3 cases")
  expect_s3_class(dat, "scdf")
  expect_length(dat, 3)
  expect_identical(names(dat), names(exampleAB))
  expect_equal(values_of(dat), values_of(exampleAB))
  expect_identical(phases_of(dat), phases_of(exampleAB))
  expect_identical(c(dv(dat), phase(dat), mt(dat)),
                   c(dv(exampleAB), phase(exampleAB), mt(exampleAB)))
})

test_that("the field and decimal separators are honoured in both directions", {
  file <- tempfile(fileext = ".csv")
  write_scdf(Grosche2011, file, sep = ";", dec = ",")
  dat <- suppressMessages(read_scdf(file, sep = ";", dec = ","))
  expect_length(dat, length(Grosche2011))
  expect_equal(values_of(dat), values_of(Grosche2011))
})

test_that("variables with other names are read back", {
  file <- tempfile(fileext = ".csv")
  write_scdf(exampleA1B1A2B2_zvt, file)
  dat <- suppressMessages(read_scdf(
    file, cvar = "case", pvar = "part", dvar = "zvt", mvar = "day"
  ))
  expect_equal(
    describe(dat)$descriptives,
    describe(exampleA1B1A2B2_zvt)$descriptives,
    ignore_attr = TRUE
  )
})

test_that("write_scdf writes to the console when filename is NULL", {
  expect_output(write_scdf(exampleAB))
  expect_output(write_scdf(exampleAB, filename = NULL))
})

test_that("a file type that can not be read is rejected", {
  # the unreadable type left the data object unassigned, so an object of that
  # name in the workspace was returned as if it were the file's content
  dat <- "not the data of a file"
  expect_error(read_scdf("somewhere.txt"))
  expect_error(read_scdf("somewhere.docx"))
  expect_error(read_scdf("somewhere.csv", type = "text"))
})

test_that("the file type is matched case-insensitively", {
  file <- tempfile(fileext = ".csv")
  write_scdf(exampleAB, file)
  expect_message(dat <- read_scdf(file, type = "CSV"), "Imported")
  expect_length(dat, 3)
})

test_that("a data frame is read directly", {
  expect_message(dat <- read_scdf(as.data.frame(exampleAB)), "Imported 3 cases")
  expect_s3_class(dat, "scdf")
  expect_equal(values_of(dat), values_of(exampleAB))
})

test_that("a yaml file is read", {
  skip_if_not_installed("yaml")
  file <- tempfile(fileext = ".yml")
  writeLines(c(
    "Anna:",
    "  values:",
    "    A: [1, 2, 3]",
    "    B: [7, 8, 9]",
    "Bert:",
    "  values:",
    "    A: [2, 3, 4]",
    "    B: [8, 9, 10]"
  ), file)
  dat <- read_scdf(file)
  expect_s3_class(dat, "scdf")
  expect_length(dat, 2)
  expect_identical(names(dat), c("Anna", "Bert"))
  expect_equal(values_of(dat), c(1, 2, 3, 7, 8, 9, 2, 3, 4, 8, 9, 10))
  expect_identical(phases_of(dat), rep(rep(c("A", "B"), each = 3), 2))
  expect_equal(dat[[1]]$mt, 1:6)
})
