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
