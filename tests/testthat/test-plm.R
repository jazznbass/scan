test_that("plm", {
  
  result <- plm(exampleAB$Johanna)
  expect_identical(round(sum(coef(result)), 5), 94.64233) 
  expect_s3_class(result, "sc_plm")
  
  result <- plm(
    exampleAB_score$Christiano,
    family = "binomial",
    var_trials = "trials"
  )
  
  expect_identical(round(sum(coef(result)), 3), 3.565)
  
  result_2 <- plm(
    exampleAB_score$Christiano,
    family = "binomial",
    var_trials = 20
  )

  expect_identical(coef(result), coef(result_2))
  
})
