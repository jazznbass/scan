
.tau_z <- function(tau) {
  0.5 * log((1 + tau)/(1 - tau))
}

.inv_tau_z <- function(tau) {
  if (identical(tau, Inf)) {
    return(1)
  }
  if (identical(tau, -Inf)) {
    return(-1)
  }
  (exp(2 * tau) - 1) / (exp(2 * tau) + 1)
}

.tau_ci <- function(tau, n, ci, se_method) {
  z <- qnorm((1 - ci) /2, lower.tail = FALSE)
  
  if (se_method == "tau") se_z <- sqrt(0.437 / (n - 4))
  if (se_method == "z") se_z <- 1 / sqrt(n - 3)
  
  tau_z <- .tau_z(tau)
  tau_z_ci_lower <- tau_z - z * se_z
  tau_z_ci_upper <- tau_z + z * se_z
  
  list(
    tau = .inv_tau_z(tau_z),
    n = n,
    tau_ci_lower = .inv_tau_z(tau_z_ci_lower),
    tau_ci_upper = .inv_tau_z(tau_z_ci_upper),
    tau_z = tau_z,
    tau_z_ci_lower = tau_z_ci_lower,
    tau_z_ci_upper = tau_z_ci_upper,
    se_z = se_z,
    se_method = se_method
  )
  
}

.meta_tau <- function(tau, n, ci, se_method) {
  ci_z <- qnorm((1 - ci) / 2, lower.tail = FALSE)
  tau_z <- .tau_z(tau)
  
  if (se_method == "tau") se_z <- sqrt(0.437 / (n - 4))
  if (se_method == "z") se_z <- 1 / sqrt(n - 3)
  
  weight <- 1 / se_z^2
  TE <- sum(tau_z * weight) / sum(weight)
  
  # Common effect estimate (Cooper & Hedges, 1994, p. 265-6)
  seTE <- sqrt(1 / sum(weight))
  
  # return
  list(
    tau = .inv_tau_z(TE),
    lower = .inv_tau_z(TE - ci_z * seTE),
    upper = .inv_tau_z(TE + ci_z * seTE),
    z = TE / seTE,
    p = 2 * pnorm(abs(TE/ seTE), lower.tail = FALSE),
    se = seTE,
    ci = ci,
    se_method = se_method
  )
}

