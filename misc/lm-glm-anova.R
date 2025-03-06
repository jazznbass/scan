library(scan)
des <- design(
  phase_design = list(A = 20, B = 30), 
  level = list(0, 1), 
  distribution = "gaussian",
  n_trials = 1)


set.seed(123)
scdf <- random_scdf(des)
#scdf[[1]]$values <- c(rbinom(50, 1, 0.1), rbinom(50, 1, 0.6))

df <- scdf |> add_dummy_variables() |> as.data.frame()

mod1.1 <- glm(values ~ phaseB, data = df, family = "gaussian")
mod1.2 <- glm(values ~ mt + phaseB , data = df, family = "gaussian")
mod1.3 <- glm(values ~ mt + phaseB + interB , data = df, family = "gaussian")

mod2.1 <- lm(values ~ phaseB , data = df)
mod2.2 <- lm(values ~ mt + phaseB , data = df)
mod2.3 <- lm(values ~ mt + phaseB + interB , data = df)

anova(mod1.1, mod1.2)
pf(0.6121, 1, 47, lower = FALSE)

anova(mod1.1, mod1.2, test = "LRT")
dispersion <- 1027.2 / 47
pchisq(13.377 / dispersion, 1, lower.tail = FALSE)
pf(13.377 / dispersion, 1, 47, lower = FALSE)

mod0 <- plm(scdf, level = FALSE, trend = FALSE, slope = FALSE) |> fetch()
mod1 <- plm(scdf, trend = FALSE, slope = FALSE) |> fetch()
mod2 <- plm(scdf, slope = FALSE) |> fetch()
mod3 <- plm(scdf) |> fetch()

anova(mod0, mod1, mod2, mod3, test = "LRT")
