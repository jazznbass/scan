test_that("batch_apply applies a function to each case and preserves names", {
  scdf <- exampleAB
  
  res <- batch_apply(scdf, mean(as.data.frame(.)$values))
  
  expect_equal(res, list(Johanna = 69.25, Karolina = 68.05, Anja = 68.95))
  expect_equal(names(res), names(scdf))
})

test_that("batch_apply simplifies vector outputs to a data frame with case and rownames", {
  
  scdf <- exampleAB
  
  res <- batch_apply(scdf,
    {x <- as.data.frame(.)
     c(
       mean = mean(x$values),
       sd   = sd(x$values)
    )},
    simplify = TRUE
  ) 
  
  expect_equal(object_checksum(res), '242.2942')
                                                                                                                                                   
})
