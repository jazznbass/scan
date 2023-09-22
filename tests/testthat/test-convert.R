test_that("convert", {
  
  fx <- tempfile()
  fy <- tempfile()
  convert(exampleABC, file = fx, silent = TRUE)
  convert(exampleABC, inline = TRUE, file = fy, silent = TRUE)
  
  source(fx)
  x <- study
  
  source(fy)
  y <- study
  
  expect_equal(x,y)
  
  convert(Leidig2018, file = fx, silent = TRUE)
  source(fx)
  
  expect_equal(scan:::check_scdf(study), TRUE)
  
  ne <- new.env()

  eval(parse(text = convert(exampleAB_score, silent = TRUE)), ne)
  expect_equal(exampleAB_score, ne$study)
  
})
