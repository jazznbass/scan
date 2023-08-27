test_that("main", {
  
  fx <- tempfile()
  fy <- tempfile()
  convert(exampleABC, file = fx)
  convert(exampleABC, inline = TRUE, file = fy)
  
  source(fx)
  x <- study
  source(fy)
  y <- study
  
  expect_equal(x,y)
  
  convert(Leidig2018, file = fx)
  source(fx)
  expect_equal(scan:::check_scdf(study), TRUE)
  
  ne <- new.env()
  eval(parse(text = convert(exampleAB_score)), ne)
  expect_equal(exampleAB_score, ne$study)
  
})
