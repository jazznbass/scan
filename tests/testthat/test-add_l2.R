test_that("combination", {
  
  scdf <- exampleABC
  
  l2 <- data.frame(
    case = c("Marie", "Rosalind", "Lise"),
    gender = c(0,0,2),
    age = c(23,35,12)
  )
  combined <- scdf |> add_l2(l2)
  
  expect_equal(object_checksum(combined), '9242.0000')
  
})

test_that("error", {
  
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
  
  expect_error(scdf$Marie |> add_l2(l2[1,]))
  
})
