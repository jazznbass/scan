# Runs the whole test suite and writes the result into a text file
library(scan)
library(testthat)

sink("misc/claude-check-testthat.txt", split = TRUE)

res <- test_dir(
  "tests/testthat", package = "scan", reporter = "summary",
  stop_on_failure = FALSE
)
df <- as.data.frame(res)

cat("\n===== per file =====\n")
print(df[, c("file", "test", "nb", "failed", "error", "warning", "skipped",
             "real")])

cat("\n===== totals =====\n")
cat("   tests   :", sum(df$nb), "\n")
cat("   failed  :", sum(df$failed), "\n")
cat("   errors  :", sum(df$error), "\n")
cat("   warnings:", sum(df$warning), "\n")
cat("   skipped :", sum(df$skipped), "\n")
cat("   time    :", round(sum(df$real), 1), "seconds\n")

cat("\n=====",
    if (sum(df$failed) + sum(df$error) == 0) "ALL PASSED" else "FAILED",
    "=====\n")
sink()
cat("written\n")
