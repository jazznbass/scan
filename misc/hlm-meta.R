library(scan)

dat_shogren_2004 <- read.csv("http://jepusto.com/data/Shogren_data_merged.csv")

names(dat_shogren_2004)

dat_shogren_2004 <- as_scdf(dat_shogren_2004,cvar = "Case", dvar = "A", mvar = "time", pvar = "Phase")
      
dat_shogren_2004      
      
get_model <- function(x) x$full.model

dat <- exampleAB_50 

sd_res <- batch_apply(dat, {
  plm(.) |> 
    get_model() |> 
    residuals() |> 
    sd()
}, simplify = TRUE)
names(sd_res) <- c("case", "sd_res")

dat <- dat |> 
  add_l2(sd_res) |> 
  transform(values_std = values / sd_res)


add_l2(dat, sd_res)


dat %>%
  transform(
    values_std = 
  )

ses

fit <- plm(exampleAB$Johanna) |> get_model()

##
aov(fit)
fit$deviance
sqrt(403.4583 / 19)
sum(residuals(fit)^2)
sd(residuals(fit))


Shogren
