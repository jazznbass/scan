test_that("combination", {
  
  scdf <- exampleABC
  
  l2 <- data.frame(
    case = c("Marie", "Rosalind", "Lise"),
    gender = c(0,0,2),
    age = c(23,35,12)
  )
  
  combined <- scdf |> add_l2(l2)
  
  expect_s3_class(combined, "scdf")
  expect_equal(scan:::check_scdf(combined), TRUE)
  expect_equal(all(combined[["Rosalind"]]$age == 35), TRUE)
  expect_equal(all(combined[["Lise"]]$gender ==2), TRUE)
  expect_equal(attributes(scdf), attributes(combined))
})

test_that("warnings", {
  
  scdf <- exampleABC
  l2 <- data.frame(
    case = c("Marie", "Heinz", "Lise"),
    gender = c(0,0,2),
    age = c(23,35,12)
  )
  
  expect_warning(scdf |> add_l2(l2))
  
  l2 <- data.frame(
    name = c("Marie", "Rosalind", "Lise"),
    gender = c(0,0,2),
    age = c(23,35,12)
  )
  
  expect_warning(scdf$Marie |> add_l2(l2[1,]))
  
})