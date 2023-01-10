library(scan)

# Standard error for Fisher z transformed values from a product moment correlation

se_r <- function(r, n) (1 - r*r) / sqrt(n - 2)

se_z_r <- function(n) 1 / sqrt(n - 3)

# Standard error for Fisher z transformed values from Kendall's tau
se_z_tau <- function(n) sqrt(0.437 / (n - 4))

weighted_es <- function(es, se) sum(es * (1/se^2)) / sum(1/se^2)

meta_tau <- function(tau, n, ci = 0.95) {
  ci_z <- qnorm((1 - ci) / 2, lower.tail = FALSE)
  tau_z <- scan:::.tau_z(tau)
  se_z <- 1 / sqrt(n - 3)#se_z_r(n)
  TE <- sum(tau_z * (1/se_z^2)) / sum(1/se_z^2)
  seTE <- se_z_r(sum(n - 2) - 1)
  ci_z <- TE + c(-1, 1) * ci_z * seTE
  list(
    tau = scan:::.inv_tau_z(TE),
    lower = scan:::.inv_tau_z(ci_z[1]),
    upper = scan:::.inv_tau_z(ci_z[2]),
    z = TE/ seTE,
    p = 2 * pnorm(abs(TE/ seTE), lower.tail = FALSE),
    se = seTE,
    ci = ci
  )
}


case1 <- scdf(c(A = 2,3,5,4,6,B = 4,5,3,5,6))
case2 <- scdf(c(A = 1,2,1,3,4,3,5,4,3,B = 5,4,6,4,5,6,5,4,3,4,5,6))
case3 <- scdf(c(A = 4,5,3,4,5,7,6,5,B = 6,5,6,5,4,5,3))
case4 <- scdf(c(A = 1,2,3,2,1,2,3,2,B = 6,5,4,1,2,4,3,2,3,4,5))
study1 <- c(case1, case2, case3, case4)

res_tau <- tau_u(study1)
n <- sapply(study1, function(x) nrow(x))
k <- length(n)
tau <- sapply(res_tau$table, function(x) x["A vs. B - Trend A", "Tau"])
tau_z <- scan:::.tau_z(tau)

weighted_es(tau_z, se_z_tau(n))|> scan:::.inv_tau_z()
weighted_es(tau_z, se_z_r(n))|> scan:::.inv_tau_z()
weighted_es(tau_z, 1 / sqrt(n - 9))|> scan:::.inv_tau_z()

weighted_es(tau, se_r(tau, n))

weighted.mean(tau, n)

se_z_r(sum(n)-k*2)

meta_tau(tau, n)

library(meta)
res1 <- meta::metacor(tau,n, random = FALSE)
res1
scan:::.inv_tau_z(res1$TE.common)
res1$w.fixed
res1$seTE.fixed

##
## Common effect estimate (Cooper & Hedges, 1994, p. 265-6)
##
w.common <- 1 / res1$seTE^2
TE <- weighted.mean(res1$TE, w.common)
seTE <- sqrt(1 / sum(w.common, na.rm = TRUE))
res1$w.common

ci(TE, seTE)

pnorm



# different package
res <- psychmeta::ma_r(tau, n)#, wt_type = "inv_var_mean")
res$meta_tables$`analysis_id: 1`$barebones$se_r

psychmeta::get_stuff(res)

res <- meta::ci(res$TE.common, res$seTE.common)
str(res)


NSM3::kendall.ci(case1[[1]]$values, as.numeric(case1[[1]]$phase))

n <- length(case1[[1]]$values)
r <- cor(case1[[1]]$values, as.numeric(case1[[1]]$phase), method = "k")

scan:::.tau_ci(r, 10)










x <- c(A = 1,2,1,3,4,3,5,4,3,B = 5,4,6,4,5,6,5,4,3,4,5,6)
y <- c(rep(0,9), rep(1,12))

bench::mark(
  meta:::kentau(x,y),
  scan:::.kendall_full(x,y, continuity_correction = FALSE),check = FALSE
)

