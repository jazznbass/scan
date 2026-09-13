# ---------------------------------------------------------------------------
# Verification script for the bug report of 2026-09-11 (Claude).
# Run in RStudio from the package root:
#     devtools::load_all(".")
#     source("misc/claude-bugcheck-2026-09-11.R")
# Then paste the console output back.
# This script only READS, it changes nothing in the package.
# ---------------------------------------------------------------------------

hr <- function(n, txt) cat("\n\n===== [", n, "] ", txt, " =====\n", sep = "")
show <- function(x) print(x)

hr(1, ".check_scdf(): early return type mismatch")
# expected per code intent: 'Attribute scdf missing.'
# suspected actual       : '$ operator is invalid for atomic vectors'
x <- exampleAB
attr(x, "scdf") <- NULL
cat("-- check_scdf() on scdf without attribute:\n")
print(try(scan:::check_scdf(x), silent = TRUE))


hr(2, "scdf(): documented priority of phase_design")
# docs: priority is phase_design > phase_starts > B_start > pvar > names(dv)
# so this should yield A = 7, B = 3
cat("-- phase_design = c(A=7,B=3) together with B_start = 4:\n")
d1 <- scdf(1:10, phase_design = c(A = 7, B = 3), B_start = 4)
print(table(d1[[1]]$phase))
cat("   (docs -> A=7 B=3 ; suspected actual -> A=3 B=7)\n")

cat("\n-- phase_design = c(A=7,B=3) together with phase_starts = c(A=1,B=4):\n")
d2 <- scdf(1:10, phase_design = c(A = 7, B = 3), phase_starts = c(A = 1, B = 4))
print(table(d2[[1]]$phase))

cat("\n-- phase_design alone (control, must be A=7 B=3):\n")
d3 <- scdf(1:10, phase_design = c(A = 7, B = 3))
print(table(d3[[1]]$phase))


hr(3, "kendall_tau(): missing zero-variance check for y")
# line 11 tests all(x == x[1]) || all(x == x[2]) -- both test x, never y
cat("-- constant y, no warning expected to be emitted (that is the bug):\n")
r <- withCallingHandlers(
  scan:::kendall_tau(x = c(1, 2, 3, 4, 5), y = c(2, 2, 2, 2, 2)),
  warning = function(w) {cat("   WARNING RAISED:", conditionMessage(w), "\n"); invokeRestart("muffleWarning")}
)
cat("   tau =", r$tau, " Den =", r$D, " z =", r$z, " p =", r$p, "\n")

cat("\n-- control: constant x does warn:\n")
r2 <- withCallingHandlers(
  scan:::kendall_tau(x = c(2, 2, 2, 2, 2), y = c(1, 2, 3, 4, 5)),
  warning = function(w) {cat("   WARNING RAISED:", conditionMessage(w), "\n"); invokeRestart("muffleWarning")}
)


hr(4, "recombine_phases(): phase names built with wrong index direction")
dat <- scan:::.prepare_scdf(exampleA1B1A2B2)
res <- scan:::recombine_phases(dat, phases = list(c("A1", "A2"), c("B1", "B2")))
cat("-- original phases of case 1:\n"); print(res$phases$original[[1]])
cat("-- new phase labels of case 1:\n");  print(res$phases$new[[1]])
cat("   (expected c('A1A2','B1B2'))\n")


hr(5, "pand(method='minimum'): field name + export()")
p_min  <- pand(exampleAB, method = "minimum")
p_sort <- pand(exampleAB, method = "sort")
cat("-- names() of the 'minimum' result:\n"); print(names(p_min))
cat("-- p_min$perc_overlap  :"); print(p_min$perc_overlap)
cat("-- p_min$perc_overlaps :"); print(p_min$perc_overlaps)
cat("-- p_sort$perc_overlap :"); print(p_sort$perc_overlap)
cat("\n-- print(p_min) -- watch the 'percentage =' line:\n")
print(p_min)
cat("\n-- export(p_min):\n")
print(try(export(p_min), silent = TRUE))


hr(6, "pand(method='sort'): randomisation despite docs saying otherwise")
cat("-- 10 repeated calls of pand(Parker2007)$pand:\n")
print(replicate(10, pand(Parker2007)$pand))
cat("-- 5 repeated calls of overlap(exampleAB)$overlap$PAND:\n")
print(t(replicate(5, overlap(exampleAB)$overlap$PAND)))
cat("   (docs say randomisation is excluded -> values should be constant)\n")


hr(7, ".moving_average(): reversed loop on short series")
cat("-- 5 values, lag = 3 (needs 2*lag+1 = 7):\n")
print(try(scan:::.moving_average(c(1, 2, 3, 4, 5), lag = 3), silent = TRUE))
cat("-- same via moving_mean() inside transform():\n")
tiny <- scdf(c(A = 1, 2, 3, B = 4, 5))
print(try(transform(tiny, sm = moving_mean(values, lag = 3))[[1]], silent = TRUE))
cat("   (expected: a clear warning or unchanged values, not silent NAs)\n")


hr(8, "sanity: sessionInfo")
cat("scan version:", as.character(utils::packageVersion("scan")), "\n")
print(R.version.string)
