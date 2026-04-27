test_that("main", {
  
  res <- corrected_tau(exampleAB_score)
  expect_all_true(sum(res$tau) > 2.12)
})
