# Check for as_scdf() (Bug 20).  Read-only.   devtools::load_all(".")
hr <- function(n, txt) cat("\n\n===== [", n, "] ", txt, " =====\n", sep = "")

hr(1, "normal round trip")
long <- as.data.frame(exampleAB)
cat("-- columns of the long format:\n"); print(names(long))
back <- as_scdf(long)
cat("-- class: "); print(class(back))
cat("-- all elements are data frames: ")
print(all(sapply(back, is.data.frame)))
cat("-- case names: "); print(names(back))
cat("-- identical to the original data? ")
print(all(mapply(function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE)),
                 back, exampleAB)))
cat("-- check_scdf: "); print(scan:::check_scdf(back))

hr(2, "minimal data: only case, values, mt, phase")
mini <- data.frame(
  case   = rep(c("a", "b"), each = 6),
  values = c(1, 2, 3, 7, 8, 9, 2, 3, 4, 8, 9, 10),
  mt     = rep(1:6, 2),
  phase  = rep(rep(c("A", "B"), each = 3), 2)
)
m <- as_scdf(mini)
cat("-- all elements are data frames: "); print(all(sapply(m, is.data.frame)))
cat("-- columns per case: "); print(names(m[[1]]))
print(m)

hr(3, "the drop = FALSE case: two variable roles point to one column")
one <- data.frame(case = rep("a", 6), mt = 1:6, phase = rep(c("A", "B"), each = 3))
d <- as_scdf(one, dvar = "mt", mvar = "mt")
cat("-- class of the single case: "); print(class(d[[1]]))
cat("-- is it still a data frame? "); print(is.data.frame(d[[1]]))
cat("-- columns: "); print(names(d[[1]]))
cat("   (before the fix this collapsed to a vector)\n")

hr(4, "missing case variable")
nocase <- data.frame(values = c(1, 2, 3, 7, 8, 9), mt = 1:6,
                     phase = rep(c("A", "B"), each = 3))
n <- as_scdf(nocase)
cat("-- number of cases: "); print(length(n))
cat("-- name: "); print(names(n))
cat("-- is a data frame: "); print(is.data.frame(n[[1]]))
cat("-- 'case' column removed again: "); print(!("case" %in% names(n[[1]])))

hr(5, "missing value in the case variable is now caught")
bad <- mini; bad$case[3] <- NA
print(try(as_scdf(bad), silent = TRUE))
cat("   (before the fix this check could not fire when cvar was absent)\n")

hr(6, "missing value in the phase variable")
bad2 <- mini; bad2$phase[3] <- NA
print(try(as_scdf(bad2), silent = TRUE))

hr(7, "missing required variables")
print(try(as_scdf(mini[, c("case", "mt", "phase")]), silent = TRUE))

hr(8, "sort_cases and phase_names")
unsorted <- mini; unsorted$case <- rep(c("b", "a"), each = 6)
cat("-- sort_cases = FALSE: "); print(names(as_scdf(unsorted)))
cat("-- sort_cases = TRUE : "); print(names(as_scdf(unsorted, sort_cases = TRUE)))
cat("-- phase_names:\n")
print(levels(as_scdf(mini, phase_names = c("Base", "Int"))[[1]]$phase))

hr(9, "input that already carries scdf attributes")
lo <- as.data.frame(exampleAB_add)
cat("-- scdf attributes present: "); print(!is.null(scan:::scdf_attr(lo)))
r <- suppressMessages(as_scdf(lo))
cat("-- all data frames: "); print(all(sapply(r, is.data.frame)))
cat("-- attributes: "); print(unlist(scan:::scdf_attr(r)))

hr(10, "existing test file")
print(testthat::test_file("tests/testthat/test-as_scdf.R"))
