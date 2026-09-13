# Check for the cdc() field renaming (Bug 17).  Read-only.
#   devtools::document(); devtools::load_all(".")
hr <- function(n, txt) cat("\n\n===== [", n, "] ", txt, " =====\n", sep = "")

hr(1, "documented field names exist, old ones are gone")
res <- cdc(exampleAB)
cat("-- names(res):\n"); print(names(res))
for (nm in c("cdc", "cdc_exc", "cdc_nb", "cdc_p", "cdc_all", "N",
             "decreasing", "trend_method", "conservative", "case_names")) {
  cat(sprintf("   %-13s %s\n", nm, if (nm %in% names(res)) "present" else "MISSING"))
}
cat("-- old names still there? ")
print(any(c("cdc_be", "cdc_b") %in% names(res)))
cat("-- 'phases' (was documented but never returned)? ")
print("phases" %in% names(res))

hr(2, "values are the ones the documentation describes")
cat("-- cdc_exc: number of phase B datapoints indicating expected change\n")
print(res$cdc_exc)
cat("-- cdc_nb: number of phase B datapoints\n")
print(res$cdc_nb)
cat("-- plausible? cdc_exc <= cdc_nb for every case: ")
print(all(res$cdc_exc <= res$cdc_nb))
cat("-- cdc_nb equals the observed phase B lengths: ")
print(identical(as.integer(res$cdc_nb),
                as.integer(sapply(exampleAB, function(x) sum(x$phase == "B")))))

hr(3, "print method")
print(res)

hr(4, "export method")
print(export(res))

hr(5, "single case, decreasing, and the helpfile examples")
print(cdc(random_scdf(design(n = 1, slope = 0.2), seed = 42)))
print(cdc(exampleAB_decreasing, decreasing = TRUE, trend_method = "bisplit"))
print(cdc(exampleABAB, trend_method = "trisplit", phases = c(1, 4)))
print(cdc(exampleAB_decreasing, decreasing = TRUE,
          trend_method = "bisplit", conservative = 0))

hr(6, "case that cannot be evaluated")
short <- c(scdf(c(A = 5, B = 7, 6, 9, 8, 10), name = "short"), exampleAB$Johanna)
r2 <- cdc(short)
print(data.frame(case = r2$case_names, cdc = r2$cdc,
                 cdc_exc = r2$cdc_exc, cdc_nb = r2$cdc_nb))
cat("-- overall evaluation: "); print(r2$cdc_all)

hr(7, "existing test file still passes")
print(testthat::test_file("tests/testthat/test-cdc.R"))
