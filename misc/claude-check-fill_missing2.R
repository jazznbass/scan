# Round 2 for fill_missing().  devtools::load_all(".") first.  Read-only.
hr <- function(n, txt) cat("\n\n===== [", n, "] ", txt, " =====\n", sep = "")

hr(1, "NA in the dependent variable, complete measurement times")
case1 <- scdf(c(3,6,2,4,3,5,2,6,3,2, 6,7,5,8,6,7,4,8,5,6),
              phase_design = c(A = 10, B = 10), name = "no NA")
case2 <- scdf(c(3,6,2,4,3,5,2,NA,3,2, 6,7,5,8,6,NA,4,8,5,6),
              phase_design = c(A = 10, B = 10), name = "NAs")
case3 <- fill_missing(case2)
cat("-- values, original vs filled (expect 2.5 at mt 8 and 5 at mt 16):\n")
print(data.frame(mt = case1[[1]]$mt,
                 no_NA = case1[[1]]$values,
                 with_NA = case2[[1]]$values,
                 filled = case3[[1]]$values))
cat("-- any NA left? "); print(anyNA(case3[[1]]$values))

hr(2, "NA at the very first and very last measurement")
d <- scdf(c(NA, 6, 2, 4, 3, 5, 2, 6, 3, NA),
          phase_design = c(A = 5, B = 5), name = "edges")
f <- fill_missing(d)
cat("-- values:\n"); print(f[[1]]$values)
cat("   (cannot be interpolated -> stay NA, the row is kept)\n")

hr(3, "NA in values AND a gap in mt at the same time")
d2 <- scdf(c(1, 2, NA, 4, 9, 10), mt = c(1, 2, 3, 4, 9, 10),
           phase_design = c(A = 4, B = 2), name = "both")
f2 <- fill_missing(d2)
cat("-- result:\n"); print(f2[[1]])

hr(4, "several variables, one with NA")
d3 <- exampleAB_add
d3[[1]][c(4, 9), "wellbeing"] <- NA
d3[[1]][6, "depression"] <- NA
f3 <- fill_missing(d3)
cat("-- rows 1:10 of case 1:\n"); print(f3[[1]][1:10, ])

hr(5, "regression check: the three helpfile examples still work")
g <- fill_missing(Grosche2011)
cat("-- Grosche2011 rows: "); print(sapply(g, nrow))
cat("-- any NA in values? "); print(any(sapply(g, function(x) anyNA(x$values))))
rolf_n <- exampleAB_add; rolf_n[[1]] <- rolf_n[[1]][-c(3, 7, 8), ]
cat("-- exampleAB_add rows after filling: "); print(nrow(fill_missing(rolf_n)[[1]]))
Maggie2_n <- random_scdf(design(level = list(0,1)), seed = 123)
Maggie2_n[[1]][c(5, 12:14, 20), "mt"] <- NA
cat("-- Maggie2 mt: "); print(fill_missing(Maggie2_n)[[1]]$mt)

hr(6, "phase boundaries -- decision needed")
d4 <- scdf(c(3, 3, 3, 3, NA, 9, 9, 9, 9, 9),
           phase_design = c(A = 5, B = 5), name = "boundary")
f4 <- fill_missing(d4)
cat("-- NA is the LAST measurement of phase A, next value is the first of B:\n")
print(data.frame(mt = f4[[1]]$mt, phase = f4[[1]]$phase,
                 values = f4[[1]]$values))
cat("   interpolated across the phase change -> 6. Is that what you want,\n")
cat("   or should interpolation stay within a phase?\n")
