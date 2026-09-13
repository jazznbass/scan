# ---------------------------------------------------------------------------
# Verification script, round 2 (tau_u, trend, autocorr).  Claude, 2026-09-11
#     devtools::load_all(".")
#     source("misc/claude-bugcheck-round2-2026-09-11.R")
# Read-only, changes nothing in the package.
# ---------------------------------------------------------------------------

hr <- function(n, txt) cat("\n\n===== [", n, "] ", txt, " =====\n", sep = "")

hr("2.1", "tau_u(ci = NULL): docs say it is allowed")
cat("-- @param ci: 'If NULL or NA, no confidence intervals are calculated.'\n")
cat("-- ci = NA (control):\n")
print(try(tau_u(exampleAB[1], ci = NA)$table[[1]][, c("Tau", "CI lower")], silent = TRUE))
cat("-- ci = NULL:\n")
print(try(tau_u(exampleAB[1], ci = NULL), silent = TRUE))


hr("2.2", "tau_u with only one observed value in phase A")
one_a <- scdf(c(A = 5, B = 7, 6, 9, 8, 10))
cat("-- phase A has n = 1:\n")
res <- try(tau_u(one_a, meta_analyses = FALSE), silent = TRUE)
if (inherits(res, "try-error")) print(res) else
  print(res$table[[1]][, c("pairs", "pos", "neg", "S", "Tau")])
cat("   (expected: clear warning + NA, not silent NA from a 1:0 loop)\n")

cat("\n-- same with NAs reducing phase A to n = 1:\n")
one_a2 <- scdf(c(A = 5, NA, NA, B = 7, 6, 9, 8, 10))
res2 <- try(tau_u(one_a2, meta_analyses = FALSE), silent = TRUE)
if (inherits(res2, "try-error")) print(res2) else
  print(res2$table[[1]][, c("pairs", "pos", "neg", "S", "Tau")])


hr("2.3", "trend(): custom model with more than one predictor")
d <- scdf(c(A = 3, 2, 4, 2, 5, B = 8, 10, 9, 12, 11))
cat("-- control, default models:\n")
print(try(trend(d)$trend, silent = TRUE))
cat("\n-- proper quadratic model (mt + mt^2):\n")
print(try(trend(d, model = list("Quad2" = values ~ mt + I(mt^2)))$trend, silent = TRUE))
cat("   (.beta_weights() returns 4 values, target is ma[.row, 1:3])\n")

cat("\n-- note: the built-in 'Quadratic' is values ~ I(mt^2), i.e. WITHOUT the linear term:\n")
print(trend(d)$formulas)


hr("2.4", "trend(): factor predictor, although @param names 'phase'")
cat("-- @param model: 'The parameters of the model are values, mt and phase.'\n")
print(try(trend(d, model = list("Level" = values ~ phase))$trend, silent = TRUE))


hr("2.5", "autocorr() with missing values")
cat("-- control, no NA:\n")
print(try(autocorr(scdf(c(A = 3, 2, 4, 2, 5, B = 8, 10, 9, 12, 11)), lag_max = 2)$autocorr[[1]],
          silent = TRUE))
cat("\n-- same data with two NAs (scdf() documents NA as supported):\n")
print(try(autocorr(scdf(c(A = 3, NA, 4, 2, 5, B = 8, 10, NA, 12, 11)), lag_max = 2)$autocorr[[1]],
          silent = TRUE))


hr("2.6", "autocorr() with a phase of length 1")
cat("-- phase A has n = 1 -> lag becomes 0 -> acf(lag.max = 0):\n")
print(try(autocorr(scdf(c(A = 5, B = 7, 6, 9, 8, 10)), lag_max = 2)$autocorr[[1]],
          silent = TRUE))


hr("2.7", "pand(method='sort'): 'decreasing' also reverses the phase tiebreak")
# ties between phases are broken by the phase label; with decreasing = TRUE
# that tiebreak flips from A-first to B-first.
tie <- scdf(c(A = 5, 5, 5, 5, B = 5, 5, 5, 5))
cat("-- all values tied, increasing:\n"); print(pand(tie, method = "sort")$pand)
cat("-- all values tied, decreasing:\n"); print(pand(tie, method = "sort", decreasing = TRUE)$pand)
cat("   (a fully tied data set should not look different by direction)\n")

cat("\n-- sample() in the sort branch: does removing it change anything?\n")
d2  <- scan:::recombine_phases(scan:::.prepare_scdf(Parker2007), phases = c(1, 2))$data
pv  <- scan:::phase(Parker2007); dvv <- scan:::dv(Parker2007)
with_sample <- replicate(200, {
  unlist(lapply(d2, function(x) {x <- x[sample(1:nrow(x)), ]
    as.character(x[[pv]][order(x[[dvv]], x[[pv]])])}))
}) |> apply(2, paste0, collapse = "") |> unique() |> length()
cat("   distinct sorted-phase-sequences over 200 shuffles:", with_sample, "\n")
cat("   (1 = sample() has no effect at all, i.e. it is dead code)\n")
