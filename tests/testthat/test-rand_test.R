test_that("rand_test", {
  
  scdf <- exampleAB
  
  results <- rand_test(scdf, complete = TRUE)

  expect_identical(
    round(
      with(
        results, 
        p.value + observed.statistic + Z + p.Z.single + mean(distribution)
      ), 
    8),
    40.97299008
  )
  
  userstat <- list(
    statistic = function(a, b) sum(b)/length(b) - sum(a)/length(a), 
    aggregate = function(x) sum(x)/length(x),
    name = "mean B - A"
  )
  
  results_user <- rand_test(
    exampleAB, statistic_function = userstat , complete = TRUE
  )
  
  results_user$statistic <- results$statistic
  expect_identical(results_user, results)
  
})
