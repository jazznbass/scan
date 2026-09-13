# Round 3 for fill_missing(): mark argument + regression.  Read-only.
#   devtools::document(); devtools::load_all(".")
hr <- function(n, txt) cat("\n\n===== [", n, "] ", txt, " =====\n", sep = "")

hr(1, "mark = TRUE with NAs in the dependent variable")
case2 <- scdf(c(3,6,2,4,3,5,2,NA,3,2, 6,7,5,8,6,NA,4,8,5,6),
              phase_design = c(A = 10, B = 10), name = "NAs")
f <- fill_missing(case2, mark = TRUE)
print(f[[1]])
cat("-- marked rows (expect 8 and 16): "); print(which(f[[1]]$interpolated))

hr(2, "mark = TRUE with a gap in mt")
d <- scdf(c(1, 2, 3, 8, 9, 10), mt = c(1, 2, 3, 8, 9, 10),
          phase_design = c(A = 3, B = 3), name = "gap")
f2 <- fill_missing(d, mark = TRUE)
print(f2[[1]])
cat("-- marked rows (expect mt 4,5,6,7): "); print(f2[[1]]$mt[f2[[1]]$interpolated])
cat("-- phase of the added rows: "); print(as.character(f2[[1]]$phase[f2[[1]]$interpolated]))

hr(3, "mark = FALSE (default) adds no column")
print(names(fill_missing(case2)[[1]]))

hr(4, "mark with a value that cannot be interpolated (edges)")
d3 <- scdf(c(NA, 6, 2, 4, 3, 5, 2, 6, 3, NA),
           phase_design = c(A = 5, B = 5), name = "edges")
f3 <- fill_missing(d3, mark = TRUE)
print(data.frame(mt = f3[[1]]$mt, values = f3[[1]]$values,
                 interpolated = f3[[1]]$interpolated))
cat("   (rows 1 and 10 stay NA and must NOT be marked)\n")

hr(5, "mark with several cases and unknown mt in one of them")
a <- scdf(c(1, 2, NA, 4, 5, 6), phase_design = c(A = 3, B = 3), name = "ok")
b <- scdf(c(1, 2, 3, 4, 5, 6), mt = c(1, 2, NA, 4, 5, 6),
          phase_design = c(A = 3, B = 3), name = "bad mt")
ab <- c(a, b)
f4 <- fill_missing(ab, interpolate_na = FALSE, mark = TRUE)
cat("-- case 'ok':\n");     print(f4[[1]])
cat("-- case 'bad mt':\n"); print(f4[[2]])
cat("-- both cases have the same columns? ")
print(identical(names(f4[[1]]), names(f4[[2]])))

hr(6, "name collision is refused")
coll <- case2
coll[[1]]$interpolated <- 1
print(try(fill_missing(coll, mark = TRUE), silent = TRUE))

hr(7, "argument checking")
print(try(fill_missing(case2, mark = "yes"), silent = TRUE))

hr(8, "regression: the four helpfile examples")
g <- fill_missing(Grosche2011)
cat("-- Grosche2011 rows: "); print(sapply(g, nrow))
cat("-- any NA in values? "); print(any(sapply(g, function(x) anyNA(x$values))))
rolf_n <- exampleAB_add; rolf_n[[1]] <- rolf_n[[1]][-c(3, 7, 8), ]
cat("-- exampleAB_add rows: "); print(nrow(fill_missing(rolf_n)[[1]]))
M <- random_scdf(design(level = list(0,1)), seed = 123)
M[[1]][c(5, 12:14, 20), "mt"] <- NA
cat("-- Maggie2 mt: "); print(fill_missing(M)[[1]]$mt)
cat("-- scdf still valid? "); print(scan:::check_scdf(fill_missing(Grosche2011)))

hr(9, "the scdf survives a downstream analysis")
print(describe(fill_missing(Grosche2011))$descriptives[, 1:6])
