# Studies on Tau-U  - 9.7.2024

extract_ab <- function(scdf) {
  scdf <- scdf[[1]]
  idA <- which(scdf[["phase"]] == "A")
  idB <- which(scdf[["phase"]] == "B")
  list(A = scdf[["values"]][idA], B = scdf[["values"]][idB])
}


case <- random_scdf(design(phase_design = list(A = 5, B = 50), level = list(1)))

dat <- extract_ab(case)

library(SingleCaseES)
source("misc/tarlow-tau-u.R")

tarwlow <- tauu(dat$A, dat$B)
wilbert <- tau_u(case, tau_method = "a",ci_method = "z",continuity_correction = TRUE)
pustejowski <- Tau(dat$A, dat$B)
pustejowski$p <- if (pustejowski$CI_lower >= 0 && pustejowski$CI_upper >= 0) 0.05 else 1

pustejowski
wilbert$table$Case1[1,-c(1:6)]
tarwlow$ab

Tau_U(dat$A, dat$B)
View(Tau_U)

# old kendal function

# .kendall <- function(x, y, 
#                      tau_method = "b", 
#                      continuity_correction = TRUE) {
#   
#   N <- length(x)
#   
#   if (N < 3) {
#     warning("could not calculate p-values for tau. Less than two data points.")
#   }
#   
#   if (all(x == x[1]) || all(x == x[2])) {
#     warning("could not calculate tau. Variance is zero.")
#   }
#   
#   .sort <- sort.list(x)
#   x <- x[.sort]
#   y <- y[.sort]
#   C <- 0
#   D <- 0
#   
#   for(i in 1:(N - 1)) {
#     C <- C + sum(y[(i + 1):N] > y[i] & x[(i + 1):N] > x[i])
#     D <- D + sum(y[(i + 1):N] < y[i] & x[(i + 1):N] > x[i])
#   }
#   
#   S  <- C - D
#   n0 <- N * (N - 1) / 2
#   
#   if (tau_method == "a") {
#     Den <- n0
#     sdS <- S / (3 * S / sqrt( N * (N - 1) * (2* N + 5) / 2 ))
#   }
#   
#   if (tau_method == "b") {
#     tie_x <- rle(x)$lengths
#     tie_y <- rle(sort(y))$lengths
#     
#     ti <- sum(
#       vapply(tie_x, FUN = function(x) x * (x - 1) / 2, FUN.VALUE = numeric(1))
#     )
#     ui <- sum(
#       vapply(tie_y, FUN = function(x) x * (x - 1) / 2, FUN.VALUE = numeric(1))
#     )
#     
#     Den <- sqrt( (n0 - ti) * (n0 - ui) )
#     
#     v0 <- N * (N - 1) * (2 * N + 5)
#     vt <- sum(
#       vapply(
#         tie_x, function(x) (x * (x - 1)) * (2 * x + 5), FUN.VALUE = numeric(1)
#       )
#     )
#     vu <- sum(
#       vapply(
#         tie_y, function(x) (x * (x - 1)) * (2 * x + 5), FUN.VALUE = numeric(1)
#       )
#     )
#     v1 <- sum(vapply(tie_x, function(x) (x * (x - 1)), FUN.VALUE = numeric(1))) * 
#       sum(vapply(tie_y, function(x) (x * (x - 1)), FUN.VALUE = numeric(1)))
#     v2 <- sum(vapply(tie_x, function(x) (x * (x - 1)) * (x - 2), FUN.VALUE = numeric(1))) * 
#       sum(vapply(tie_y, function(x) (x * (x - 1)) * (x - 2), FUN.VALUE = numeric(1)))
#     
#     varS <- (v0 - vt - vu) / 18 + 
#       (v1 / (2 * N * (N - 1))) +  
#       (v2 / (9 * N * (N - 1) * (N - 2)))
#     
#     sdS <- sqrt(varS)
#   }
#   
#   list(
#     D = Den,
#     sdS = sdS,
#     D = Den,
#     n0 = n0,
#     N = N,
#     S = S
#   )
#   
# }

