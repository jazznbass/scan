test_that("main", {

  ex_scdf <- exampleA1B1A2B2
  
  l2 <- data.frame(
    case = c("Pawel", "Moritz", "Jannis"),
    gender = c(0,1,2),
    age = c(30, 40, 50)
  )
  
  object <- as.data.frame(ex_scdf)
  
  expect_s3_class(object, "data.frame")
  expect_equal(names(object), c("case", names(ex_scdf[[1]])))
  expect_equal(sum(unlist(lapply(ex_scdf, nrow))), nrow(object))
  
  object <- as.data.frame(ex_scdf, l2 = l2)
  expect_equal(names(object), c("case", names(ex_scdf[[1]]), names(l2)[-1]))

})


