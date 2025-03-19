dat <- Leidig2018 |> 
    add_dummy_variables( 
    model = "W", 
    contrast_level = "first", 
    contrast_slope = "first"
  ) |> 
  as.data.frame()

dat <- exampleAB$Johanna |> 
  add_dummy_variables( 
    model = "W", 
    contrast_level = "first", 
    contrast_slope = "first"
  ) |> 
  as.data.frame()

library(MCMCglmm)
model <- nlme::lme(academic_engagement~1+mt+phaseB+interB, random = ~1|case,data = dat, na.action = na.omit)
summary(model)

model2 <- MCMCglmm(academic_engagement~1+mt+phaseB+interB, 
                   random = ~us(1+phaseB+interB):case,data = dat, verbose = FALSE)
model2 <- MCMCglmm(values~1+mt+phaseB+interB, data = dat, verbose = FALSE)

summary(model2, random = FALSE)

bayesian_hplm(Leidig2018, random_level = TRUE, random_slope = TRUE)
