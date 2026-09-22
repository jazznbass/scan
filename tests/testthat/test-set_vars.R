# set_dvar() / set_mvar() / set_pvar() / set_vars()

test_that("the setters change one variable and leave the others", {
  study <- exampleAB_add
  dvar <- dv(study); pvar <- phase(study); mvar <- mt(study)
  other_dv <- setdiff(
    names(study[[1]])[vapply(study[[1]], is.numeric, logical(1))],
    c(dvar, mvar)
  )[1]

  d <- set_dvar(study, other_dv)
  expect_identical(dv(d), other_dv)
  expect_identical(c(phase(d), mt(d)), c(pvar, mvar))
  expect_s3_class(d, "scdf")
  expect_identical(lapply(d, as.data.frame), lapply(study, as.data.frame))
  expect_false(isTRUE(all.equal(
    describe(d)$descriptives, describe(study)$descriptives
  )))

  s <- study
  for (i in seq_along(s)) {
    s[[i]]$time2 <- s[[i]][[mvar]] * 2
    s[[i]]$phase2 <- s[[i]][[pvar]]
  }
  expect_identical(mt(set_mvar(s, "time2")), "time2")
  expect_identical(phase(set_pvar(s, "phase2")), "phase2")

  v <- set_vars(s, dvar = other_dv, mvar = "time2", pvar = "phase2")
  expect_identical(c(dv(v), mt(v), phase(v)),
                   c(other_dv, "time2", "phase2"))
  expect_identical(set_vars(study), study)
  w <- set_vars(study, mvar = mvar)
  expect_identical(c(dv(w), phase(w)), c(dvar, pvar))
})

test_that("a variable that is not in the data is rejected", {
  study <- exampleAB_add
  dvar <- dv(study); pvar <- phase(study); mvar <- mt(study)
  expect_error(set_dvar(study, paste0(dvar, "x")))
  expect_error(set_mvar(study, paste0(mvar, "x")))
  expect_error(set_pvar(study, paste0(pvar, "x")))
  expect_error(set_vars(study, dvar = paste0(dvar, "x")))
  e <- tryCatch(set_dvar(study, "depresion"), error = conditionMessage)
  expect_true(grepl("depresion", e, fixed = TRUE))

  # a variable that only one case has
  part <- exampleAB
  part[[1]]$extra <- 1
  expect_error(set_dvar(part, "extra"))

  expect_error(set_dvar(study, 2))
  expect_error(set_dvar(study, c(dvar, "depression")))
  expect_error(set_dvar(study, NA))
  expect_error(set_dvar(as.data.frame(study), dvar))
})

test_that("the setters work in a pipe", {
  expect_no_error(exampleAB_add |> set_dvar("depression") |> describe())
  expect_no_error(
    exampleAB_add |>
      set_dvar("depression") |>
      set_mvar(mt(exampleAB_add)) |>
      set_pvar(phase(exampleAB_add)) |>
      plm()
  )
})
