test_that("anova wrappers delegate to underlying models and match numeric results", {
  # sc_plm
  mod0 <- plm(exampleAB$Johanna, level = FALSE, slope = FALSE)
  mod1 <- plm(exampleAB$Johanna)
  
  res_wrap_plm <- anova(mod0, mod1)
  res_base_plm <- anova(mod0$full.model, mod1$full.model)
  
  expect_equal(res_wrap_plm, res_base_plm)
  
  expect_equal(object_checksum(res_wrap_plm), '936.6187')

  # sc_mplm
  data_mplm <- Leidig2018$`1a1`
  m0 <- mplm(
    data_mplm,
    update = . ~ 1,
    dvar = c("academic_engagement", "disruptive_behavior")
  )
  m1 <- mplm(
    data_mplm,
    trend = FALSE,
    dvar = c("academic_engagement", "disruptive_behavior")
  )
  m2 <- mplm(
    data_mplm,
    dvar = c("academic_engagement", "disruptive_behavior")
  )
  
  res_wrap_mplm <- anova(m0, m1, m2)
  res_base_mplm <- anova(m0$full.model, m1$full.model, m2$full.model)
  
  expect_equal(res_wrap_mplm, res_base_mplm)
  
  expect_equal(object_checksum(res_wrap_mplm), '491.2245')
  
  # sc_hplm
  h0 <- hplm(Leidig2018, trend = FALSE, slope = FALSE, level = FALSE)
  h1 <- hplm(Leidig2018, trend = FALSE)
  h2 <- hplm(Leidig2018)
  
  res_wrap_hplm <- anova(h0, h1, h2)
  res_base_hplm <- anova(h0$hplm, h1$hplm, h2$hplm)
  row.names(res_base_hplm) <- NULL  # match wrapper behavior
  
  expect_equal(res_wrap_hplm, res_base_hplm)
  
  expect_equal(object_checksum(res_wrap_hplm), '27419.7256')
  
})
