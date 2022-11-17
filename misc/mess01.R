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
scan:::.kendall_full(x, y, tau_method = "a")$tau
scan:::.kendall_full(x, y, tau_method = "b")$tau

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
