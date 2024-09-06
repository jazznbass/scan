test_that("trend", {
  
  scdf <- exampleA1B1A2B2$Moritz
  result <- trend(
    scdf,
    model = list("Cubic" = values ~ I(mt^3), "Log Time" = values ~ log(mt)),
    first_mt = 1
  )
  values <- result$trend |> sum() |> round(3)
  expect_equal(values, 386.582)

})
