#' @describeIn nap Print results
#' @order 2
#' @inheritParams print.sc
#' @inheritParams .inheritParams
#' @param complete If TRUE, all columns of the table are printed. By default
#'   the columns with the test statistics are left out.
#' @export
print.sc_nap <- function(x, 
                         digits = "auto", 
                         nice = TRUE, 
                         complete = FALSE, 
                         ...) {
  
  if (digits == "auto") digits <- 2
  cat("Nonoverlap of All Pairs\n\n")
  
  out <- as.data.frame(x$nap)
  if (!complete) out <- out[, -(4:7)]
  if (nice) out$p <- .nice_p(unlist(out$p))
  print(out, digits = digits, row.names = FALSE)
  
}

#' @describeIn nap Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @inheritParams .inheritParams
#' @export
export.sc_nap <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          select = c("Case", "NAP", "NAP Rescaled", 
                                     "w", "p", "d", "R\u00B2"),
                          round = 2,
                          ...) {
  
  if (is.na(caption)) caption <- c("Nonoverlap of all pairs")
  
  out <- as.data.frame(object$nap) 
  out$p <- .nice_p(out$p)
  out <- round_numeric(out, round)
  out <- .select(out, select)
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
