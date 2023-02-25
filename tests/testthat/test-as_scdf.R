test_that("main", {
  
  df <- as.data.frame(exampleABC)

  scdf <- suppressMessages(as_scdf(df))
  expect_equal(scan:::check_scdf(scdf), TRUE)
  expect_equal(attributes(scdf)$scdf, attributes(exampleABC)$scdf)
  
  df <- as.data.frame(exampleABC$Rosalind)
  df$case <- NULL
  scdf <- suppressMessages(as_scdf(df))
  expect_equal(scan:::check_scdf(scdf), TRUE)
  
  
  df <- as.data.frame(exampleAB_score)
  scdf <- suppressMessages(as_scdf(df))
  expect_equal(scan:::check_scdf(scdf), TRUE)
  expect_equal(names(df)[-1], names(scdf[[1]]))
  
})

# test_that("warnings", {
#   
#   scdf <- exampleABC
#   l2 <- data.frame(
#     case = c("Marie", "Heinz", "Lise"),
#     gender = c(0,0,2),
#     age = c(23,35,12)
#   )
#   
#   expect_warning(scdf |> add_l2(l2))
#   
#   l2 <- data.frame(
#     name = c("Marie", "Rosalind", "Lise"),
#     gender = c(0,0,2),
#     age = c(23,35,12)
#   )
#   
#   expect_warning(scdf$Marie |> add_l2(l2[1,]))
#   
#})