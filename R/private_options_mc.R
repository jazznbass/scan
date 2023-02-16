
# MC Functions ------------------------------------------------------------

.opt$mc_fun <- list(
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
    if (length(x) > 1) stop("Use 'tauU_meta' for multiple case designs.")
    res <- tau_u(x, method = "parker", meta_method = "none")$table[[1]]
    res[which(row.names(res) == "A vs. B - Trend A"), which(names(res) == "p")]
  },
  tauU_slope = function(x) {
    if (length(x) > 1) stop("Use 'tauU_slope_meta' for multiple case designs.")
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
  
  rand = function(x) rand_test(x, number = 100, exclude.equal = "auto")$p.value
)
