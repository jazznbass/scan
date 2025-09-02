test_that("smd", {
  
  data <- Leidig2018
  result <- smd(data = data)
  expect_s3_class(result, "sc_smd")
  expect_equal(result$smd[,-1] |> sum() |> round(4), 558.2741)
  
})
