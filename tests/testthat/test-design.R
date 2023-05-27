test_that("design function returns expected output", {
  # Test case 1
  result <- design()
  expect_s3_class(result, "sc_design")
  expect_equal(length(result$cases), 1)
  expect_equal(result$distribution, "normal")
  
  # Test case 2
  result <- design(n = 3, level = list(0, 1))
  expect_s3_class(result, "sc_design")
  expect_equal(length(result$cases), 3)
  expect_equal(result$distribution, "normal")
  expect_equal(result$cases[[1]]$level, c(0, 1))
  expect_equal(result$cases[[2]]$level, c(0, 1))
  
  # Test case 3
  result <- design(n = 2, trend = c(0, 1), start_value = c(50, 60))
  expect_s3_class(result, "sc_design")
  expect_equal(length(result$cases), 2)
  expect_equal(result$distribution, "normal")
  expect_equal(result$cases[[1]]$trend, c(0))
  expect_equal(result$cases[[1]]$start_value, 50)
  expect_equal(result$cases[[2]]$trend, c(1))
  expect_equal(result$cases[[2]]$start_value, 60)
  
})
