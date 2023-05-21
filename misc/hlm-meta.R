library(scan)

get_model <- function(x) x$full.model

dat <- exampleAB_50 

sd_res <- batch_apply(dat, {
  plm(.) |> 
    get_model() |> 
    residuals() |> 
    sd()
}, simplify = TRUE)
names(sd_res) <- c("case", "sd_res")

dat <- as.data.frame(dat) |> 
  merge(sd_res, by = "case") |> 
  as_scdf() |> 
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
