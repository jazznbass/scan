#' @rdname print.sc
#' @param complete Print further parameters.
#' @export
#' 
print.sc_tauu <- function(x, complete = FALSE, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 3
  
  cat("Tau-U\n")
  cat("Method:", x$method, "\n")
  cat("Applied Kendall's Tau-", x$tau_method, "\n", sep = "")
  if (complete || (length(x$table) > 1 && x$meta_method != "none")) {
    cat(x$ci * 100, "% CIs for tau are reported.\n\n", sep = "")
  } else cat("\n")
  
  out <- x$table
  
  if (length(out) > 1 && x$meta_method != "none") {
    cat("Overall Tau-U\n")
    cat("Meta-anlysis model:", x$meta_method, "effect\n\n")
    print(x$Overall_tau_u, row.names = FALSE, digits = digits)
    cat("\n")
  }
  
  if (!complete) {
    select_vars <- c("Tau", "SE_Tau", "Z", "p")
    select_rows <- match(
      c(
        "A vs. B", 
        "A vs. B - Trend A",
        "A vs. B + Trend B", 
        "A vs. B + Trend B - Trend A"
      ), row.names(x$table[[1]])
    )
    
    out <- lapply(x$table, function(x) round(x[select_rows, select_vars], digits))
  }
  
  out <- lapply(x$table, function(x) {x$p <- round(x$p, digits); x})
  
  
  for(i in seq_along(out)) {
    cat("Case:", names(out)[i], "\n")
    print(out[[i]], digits = digits)
    cat("\n")
  }
  
}

