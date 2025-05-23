
# 2025-03-04 ---- own functions in power test ----

mc_function <-  function(x) {
  res <- summary(hplm(x, slope = FALSE, trend = FALSE  ,random.slopes=FALSE, ICC=FALSE)$hplm)$tTable
  res[2, 5]
}


design <- design(
  n = 16, phase_design = list(A = 2, B = 4), 
  trend = 0, level = list(1), slope = list(0), 
  start_value = 50, rtt = 0.7, distribution = "normal"
)

res <- power_test(design, method = list(hplm_only_level = mc_function, hplm_full = "hplm_level"), effect = "level", n_sim = 1000)


res <- power_test(design, method = list(hplm = mc_function), effect = "level", n_sim = 1000)

print(res, digits = 2)


summary(hplm(Leidig2018, slope = FALSE, trend = FALSE  ,random.slopes=FALSE, ICC=FALSE)$hplm)$tTable

random_scdf(design) |> select_cases(1:4) |> scplot()
random_scdf(design) %>% hplm(.,slope = TRUE, trend = TRUE  ,random.slopes=FALSE, ICC=FALSE)






# 2023-02-21

## interpolate

interpolate_missing <- function(values, 
                                mt, 
                                phase = NULL,
                                method = c("linear", "regression"),
                                model = NULL){
  
  method <- method[1]
  ids <- which(is.na(values))
  
  if (method == "regression") {
    values[ids] <- predict(model, data.frame(values, mt, phase))[ids]
  }

  if (method == "linear") {
    values[ids] <- approx(mt, values, xout = mt[ids])$y
  }

  values
}

data <- exampleAB$Anja[[1]]
plot(data$mt, data$values, type = "l")

data$values[sample(1:20, 5)] <- NA
plot(data$mt, data$values, type = "l")

plot(data$mt, interpolate_missing(data$values, data$mt, method = "linear"), type = "l")

model <- plm(exampleAB$Anja)$full

plot(mt, interpolate_missing(values, mt, phase, method = "regression", model = model), type = "l")




# 2023-02-10

library(scan)
trend(exampleABC$Marie)


# 2023-02-09

#How many overall downloads
library(ggplot2)
mls <- cranlogs::cran_downloads(packages="scan", from = "2016-06-1", to = Sys.Date()-1)

sum(mls[,2])

#Cumulative
cumulative <- cumsum(mls[,2])
mls2 <- cbind(mls,cumulative)


#Plot
gr1 <- ggplot(mls2, aes(mls2$date, mls2$cumulative)) + 
  geom_line(colour = "blue",size=1) 
gr1 + xlab("Time") + ylab("Nr. of downloads") + 
  labs(title = paste0("scan cumulative downloads until ", Sys.Date()-1)) 

# 2023-02-08

tran <- function(...) {
  expressions <- substitute(list(...))
  
  print(names(expressions[3]))
  print(typeof(expressions[3]))
  
  #df <- as.data.frame(`_scdf`)
  print(expressions[c(1,2)])
  
  env <- new.env()
  env$a <- 5
  new <- eval(expressions[c(1,2)], env)
  env[[names(new)]] <- new[[1]]
  env$f <- eval(expressions[c(1,3)], env)
  print(new)
  ls(env)
  env
}  



tran(b = a + 1, mean(1:10)) 

e$f



for(i_expression in 2:length(expressions)) {
    
    deparse_ch <- deparse(expressions[i_expression])



trans <- function(`_scdf`, ...) {
  expressions <- substitute(list(...))
  df <- as.data.frame(`_scdf`)
  
  for(i_expression in 2:length(expressions)) {
    
    if(startsWith(deparse(expressions[i_expression]), "across_cases(")) {
      new <- eval(eval(expressions[c(1,i_expression)], df, parent.frame(3)))
      df <- new[[1]]
      `_scdf` <- as_scdf(df)
    } else {
      for(i_case in seq_along(`_scdf`)) {
        .list_env <- as.list(`_scdf`[[i_case]])
        .list_env$all_cases <- .list_env$all <- function(x) {
          x <- substitute(x)
          eval(x, df)
        }
        new <- eval(expressions[c(1,i_expression)], .list_env, parent.frame())
        `_scdf`[[i_case]][[names(new)]] <- new[[1]]
      }
    }
  }
  `_scdf`
}

across_cases <- function(...) {
  print(environment())
  expressions <- substitute(list(...))
  for (i in 2:length(expressions)) {
    new <- eval(expressions[c(1,i)], df, parent.env(parent.env(environment())))
    df[[names(new)]] <- new[[1]]
  }
  df
}

#across_cases(df, values = 1, mt = 14)

#df <- as.data.frame(exampleAB)

 a <- exampleABC |>
   transform(
     across_cases(values = 1:90, mt = 90:1),
     values = values + mt
    )

b <-  exampleABC |>
   transform(values = (values - mean(all_cases(values))) / sd(all_cases(values)))
 
c <-  exampleABC |>
  transform(
    across_cases(values = (values - mean(values)) / sd(values))
  )

b$Rosalind[[1]]
c$Rosalind[[1]]
all.equal(b,c)
describe(c(a,c))
 
# 2023-02-01
library(scan)
case <- scdf(c(6,5,5,4,3,3, 7,7,7,6,7,6), phase_design = c(A = 6, B = 6))
tau_u(case, method = "parker", tau_method = "a")
tau_u(case, method = "complete", tau_method = "a")

# 2023-01-01



tau_z <- function(tau) {
  0.5 * log((1 + tau)/(1 - tau))
}

inv_tau_z <- function(tau) {
  (exp(2 * tau) - 1) / (exp(2 * tau) + 1)
}

tau_ci <- function(tau, n, ci = 0.95) {
  z <- qnorm((1 - ci) /2, lower.tail = FALSE)
  var_tau_z <- sqrt(0.437/(n-4))
  tau_z <- tau_z(tau)
  tau_z_ci <- c(
    lower = tau_z - z * var_tau_z, 
    upper = tau_z + z * var_tau_z
  )
  
  list(
    tau = inv_tau_z(tau_z),
    tau_ci = c(lower = inv_tau_z(tau_z_ci[1]), upper = inv_tau_z(tau_z_ci[2])),
    tau_z = tau_z,
    tau_z_ci = tau_z_ci,
    var_tau_z = var_tau_z
  )
  
}

tau_ci(0.3684211, 20)
tau_z(0.6)


library(scan)


# 2022-14-09 ------------------------------

overlap(Parker2011b)
cdc(Parker2011b) |> str()
nap(Parker2011b)

# 2022-10-09 09:24:09 ------------------------------


des <- design(
  start_value = 50,
  s = 10,
  level = 1,
  trend = 0.1,
  slope = 0,
  phase_design = list(A = 15, B = 45),
  distribution = "gaussian"
)

data <- random_scdf(des, round = 0)


tau_u(data, tau_method = "a", method = "parker") |> print(complete = TRUE)
tau_u(data, tau_method = "b") |> print(complete = TRUE)
corrected_tau(data)


res <- tau_u(data, method = "parker", tau_method = "a", meta_method = "none")$table[[1]]
list(
  p = res[which(row.names(res) == "A vs. B"), which(names(res) == "p")],
  es = res[which(row.names(res) == "A vs. B"), which(names(res) == "Tau")]
)


data2 <- data[[1]] 
rowsA <- which(data2[["phase"]] == "A")
rowsB <- which(data2[["phase"]] == "B")
A_data <- data2[rowsA, ]
B_data <- data2[rowsB, ]

y <- as.numeric(factor(data2[["phase"]]))
x <- data2[["values"]]
scan:::kendall_tau(x, y, tau_method = "a")$tau
scan:::kendall_tau(x, y, tau_method = "b")$tau

#Kendall::Kendall(x,y)
cor(x,y,method="kendall")
DescTools::KendallTauA(x,y)

wilcox.test(x~y) |> str()
z <- qnorm(wilcox.test(x~y)$p.value,lower.tail = FALSE)
r <- z/sqrt(length(x))
r

sum(data2[rowsA, "values"] > data2[rowsB, "values"])
data

sum(unlist(lapply(data2[rowsA, "values"], function(x) x > data2[rowsB, "values"])))

# --------------

x <- Tarlow2016a[[1]]$values
y <- as.numeric(Tarlow2016a[[1]]$phase)

x <- rep(2, 11)
kendall_tau(x,y)

cor.test(x,y,method="kendall",continuity = TRUE)
DescTools::KendallTauA(x,y)
corrected_tau(Tarlow2016a, continuity = TRUE)

# example page 7:
tau_u(Tarlow2016b, method = "parker", tau_method = "a")

## Hier macht Tarlow einen fehler: "Normally, this correlation would result in Tau = −0.829". Dies ist aber nur dann der Fall, wenn er nicht nur den Denominator korrigiert, sondern auch Tau-B mit Rangbindungen berechnet (was Parker nicht macht):
tau_u(Tarlow2016b, method = "complete", tau_method = "b")
## Tau-A ergibt -0.886:
tau_u(Tarlow2016b, method = "complete", tau_method = "a")

# Tarlow page 13:
corrected_tau(Tarlow2016c, continuity = TRUE)$parameters



scplot(Tarlow2016a)

# is continuity wrong???

res <- corrected_tau(data, continuity = FALSE, repeated = FALSE)
list(p = res$p, es = res$tau)
res
