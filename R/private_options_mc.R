
# MC Functions ------------------------------------------------------------

.opt$mc_fun <- list(
  #plm_level = function(x) {
  #  res <- plm(x)
  #  coef(res)["phaseB", "Pr(>|t|)"]
  #},
  plm_level = function(x) .plm.mt(x, type = "level p"),
  plm_slope = function(x) .plm.mt(x, type = "slope p"),
  plm_poisson_level = function(x) .plm.mt(x, count.data=TRUE, type = "level p"),
  plm_poisson_slope = function(x) .plm.mt(x, count.data=TRUE, type = "slope p"),
  hplm_level = function(x) {
    res <- summary(hplm(x, random.slopes=FALSE, ICC=FALSE)$hplm)$tTable
    res[3, 5]
  },
  hplm_slope = function(x) {
    res <- summary(hplm(x, random.slopes=FALSE, ICC=FALSE)$hplm)$tTable
    param <- (nrow(res) - 2) / 2
    res[2 + param + 1, 5]
  },
  tauU = function(x) {
    if (length(x) > 1) stop("Use 'tauU_meta' for multiple case designs.", call. = FALSE)
    res <- tau_u(x, method = "parker", meta_method = "none")$table[[1]]
    res[which(row.names(res) == "A vs. B - Trend A"), which(names(res) == "p")]
  },
  tauU_slope = function(x) {
    if (length(x) > 1) stop("Use 'tauU_slope_meta' for multiple case designs.", call. = FALSE)
    res <- tau_u(x, method = "parker", meta_method = "none")$table[[1]]
    res[which(row.names(res) == "A vs. B + Trend B - Trend A"), which(names(res) == "p")]
  },
  tauU_meta = function(x) {
    res <- tau_u(x, method = "parker", meta_method = "fixed")$Overall_tau_u
    res[which(res$Model == "A vs. B - Trend A"), which(names(res) == "p")]
  },
  tauU_slope_meta = function(x) {
    res <- tau_u(x, method = "parker", meta_method = "fixed")$Overall_tau_u
    res[which(res$Model == "A vs. B + Trend B - Trend A"), which(names(res) == "p")]
  },
  base_tau = function(x) corrected_tau(x)$p,
  rand = function(x) rand_test(x, number = 100, exclude.equal = "auto", limit = 3)$p.value,
  rand_decrease = function(x) rand_test(
    x, statistic = "Mean A-B", number = 100, exclude.equal = "auto", limit = 3)$p.value,
  rand_slope = function(x) rand_test(
    x, number = 100, statistic = "Slope B-A", exclude.equal = "auto", limit = 3)$p.value,
  rand_slope_decrease = function(x) rand_test(
    x, number = 100, statistic = "Slope B-A", exclude.equal = "auto", limit = 3)$p.value
  
)


.plm.mt <- function(data, 
                    type = "level p", 
                    model = "B&L-B", 
                    dvar = "values", pvar = "phase", mvar = "mt", 
                    count.data = FALSE) {
  n <- length(data)
  if (n > 1) {
    stop("Multiple single-cases are given. Calculations could only be applied ",
         "to a single data set.\n")
  }
  
  if (inherits(data, "list")) data <- data[[1]]
  if (ncol(data) < 3) data[, mvar] <- 1:nrow(data)
  
  y  <- data[, dvar]
  n1 <- sum(data[, pvar] == "A")
  n2 <- sum(data[, pvar] == "B")
  mt <- data[, mvar]
  D  <- c(rep(0, n1), rep(1, n2))
  
  if (model == "H-M") {
    inter <- (mt - mt[n1 + 1]) * D	
  } else if (model == "B&L-B") {
    inter <- (mt - mt[n1]) * D	
  }
  
  if (count.data) {
    full <- glm(I(round(y)) ~ 1 + mt + D + inter, family = "poisson")
  } else full <- lm(y ~ 1 + mt + D + inter)
  
  if (type == "1" || type == "level p")
    return(summary(full)$coef[3, 4])
  if (type == "2" || type == "slope p")
    return(summary(full)$coef[4, 4])
  if (type == "3" || type == "level t") 
    return(summary(full)$coef[3, 3])
  if (type == "4" || type == "slope t")
    return(summary(full)$coef[4, 3])
  if (type == "5" || type == "level B")
    return(summary(full)$coef[3, 1])
  if (type == "6" || type == "slope B")
    return(summary(full)$coef[4, 1])
  if (type == "model")
    return(full)
  
}

