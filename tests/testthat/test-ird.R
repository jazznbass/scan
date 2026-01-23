testthat::test_that("ird()", {
  data("byHeart2011")
  ird_res <- ird(byHeart2011, phases = c(1,2))
  testthat::expect_equal(round(ird_res$ird, 3), 0.845)
})

