#' @describeIn tau_u Print results
#' @order 2
#' @inheritParams print.sc
#' @param x Object returned from [tau_u()].
#' @param complete Print all parameters.
#' @param select Character vector with name of variables to be included. When
#'   the vector is named, variables are renamed appropriately.
#' @param nice_p If TRUE, p-values are printed in publication friendly form.
#' @export
#' 
print.sc_tauu <- function(x, 
                          complete = FALSE, 
                          digits = "auto", 
                          select = c(
                            "Tau", "CI lower", "CI upper", "SD_S", "Z", "p"
                          ), 
                          nice_p = TRUE,
                          ...) {
  
  if (digits == "auto") digits <- 2
  
  cat("Tau-U\n")
  cat("Method:", x$method, "\n")
  cat("Applied Kendall's Tau-", x$tau_method, "\n", sep = "")
 
  if (!is.na(x$ci)) {
    cat(x$ci * 100, "% CIs for tau are reported.\n", sep = "")
    cat("CI method: ", x$ci_method, "\n\n", sep = "")
  } else cat("\n")
  
  out <- x$table
  
  if (length(out) > 1 && x$meta_analyses) {
    cat("Tau-U meta analyses:\n")
    
    cat("Weight method: ", x$meta_weight_method, "\n", sep = "")
    if (!is.na(x$ci)) cat(x$ci * 100, "% CIs are reported.\n", sep = "")
    cat("\n")
    #cat("Meta-anlysis model:", x$meta_method, "effect\n\n")
    print(x$Overall_tau_u, row.names = FALSE, digits = digits)
    cat("\n")
  }
  
  if (!complete) {
    select_vars <- select
    select_rows <- match(
      c(
        "A vs. B", 
        "A vs. B - Trend A",
        "A vs. B + Trend B", 
        "A vs. B + Trend B - Trend A"
      ), row.names(x$table[[1]])
    )
    
    out <- lapply(x$table, function(x) {
      x <- round(x[select_rows, select_vars], digits)
      if (nice_p) x$p <- .nice_p(x$p)
      if (!is.null(names(select))) names(x) <- names(select)
      x
    })
    
  } else {
    out <- lapply(out, function(x) {
      x <- round(x, digits)
      if (nice_p) x$p <- .nice_p(x$p) else x$p <- round(x$p, digits)
      x
    })
  }
  
  for(i in seq_along(out)) {
    cat("Case:", names(out)[i], "\n")
    print(out[[i]], ...)
    cat("\n")
  }
  
}

