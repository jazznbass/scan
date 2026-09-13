# Diagnostic for fill_missing().  devtools::load_all(".") first.  Read-only.
hr <- function(n, txt) cat("\n\n===== [", n, "] ", txt, " =====\n", sep = "")

hr(1, "helpfile example 3: NA in mt, including the LAST row")
Maggie2 <- random_scdf(design(level = list(0, 1)), seed = 123)
cat("rows:", nrow(Maggie2[[1]]), "-> NAs are set at rows 5, 12:14, 20\n")
Maggie2_n <- Maggie2
Maggie2_n[[1]][c(5, 12:14, 20), "mt"] <- NA
Maggie2_f <- fill_missing(Maggie2_n)
cat("\n-- mt after fill_missing():\n"); print(Maggie2_f[[1]]$mt)
cat("-- any NA left in mt? "); print(anyNA(Maggie2_f[[1]]$mt))
cat("   (doc: 'If mt values are missing (NA), they are also interpolated')\n")

hr(2, "leading vs. interior vs. trailing NA in mt, isolated")
cat("-- interior only:\n")
print(scan:::.interpolate(c(1, NA, NA, 4, 5)))
cat("-- leading:\n")
print(scan:::.interpolate(c(NA, 2, 3, 4, 5)))
cat("-- trailing:\n")
print(scan:::.interpolate(c(1, 2, 3, 4, NA)))

hr(3, "helpfile example 1: Grosche2011")
g <- fill_missing(Grosche2011)
cat("-- n rows per case, original vs filled:\n")
print(rbind(original = sapply(Grosche2011, nrow), filled = sapply(g, nrow)))
cat("-- mt of case 2 continuous?\n")
print(all(diff(g[[2]]$mt) == 1))
cat("-- any NA in values of case 2? "); print(anyNA(g[[2]]$values))

hr(4, "helpfile example 2: several variables")
rolf_n <- exampleAB_add
rolf_n[[1]] <- rolf_n[[1]][-c(3, 7, 8), ]
rolf_f <- try(fill_missing(rolf_n), silent = TRUE)
if (inherits(rolf_f, "try-error")) print(rolf_f) else print(rolf_f[[1]])

hr(5, "row names and ordering of the result")
d <- scdf(c(A = 1, 2, 3, B = 8, 9, 10), mt = c(1, 2, 3, 7, 8, 9))
f <- fill_missing(d)
cat("-- filled case:\n"); print(f[[1]])
cat("-- rownames:\n"); print(rownames(f[[1]]))
cat("-- mt sorted and complete? "); print(identical(f[[1]]$mt, 1:9 + 0))

hr(6, "interpolate_na = FALSE with an NA in mt")
d2 <- scdf(c(A = 1, 2, 3, B = 8, 9, 10), mt = c(1, 2, NA, 4, 5, 6))
f2 <- try(fill_missing(d2, interpolate_na = FALSE), silent = TRUE)
if (inherits(f2, "try-error")) print(f2) else print(f2[[1]])
cat("   (is the NA-mt row now duplicated by an interpolated row?)\n")

hr(7, "scdf attributes preserved")
cat("-- dv/phase/mt of the filled Grosche2011:\n")
print(unlist(scan:::scdf_attr(g)))
