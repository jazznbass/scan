#' @describeIn power_test Print results
#' @order 2
#' @param duration If TRUE the duration for computation is printed.
#' @inheritParams print.sc
#' @export
print.sc_power <- function(x, duration = FALSE, digits = 1, ...) {
  
  cat("Test-Power in percent:\n\n")
  
  class(x) <- "data.frame"
  
  out <- x
  
  ci <- attr(x, "ci")
  binom_test_power <- attr(x, "binom_test_power")
  binom_test_alpha <- attr(x, "binom_test_alpha")
  binom_test_correct <- attr(x, "binom_test_correct")

  if (is.numeric(ci)) {
    ci_p <- attr(x, "ci") * 100
    ci_str <- c(paste0((100 - ci_p) / 2, "%"), paste0(ci_p + (100 - ci_p) / 2, "%"))
    
    out <- x[c("Method", "Power", "Power lower",  "Power upper", 
               "Alpha Error", "Alpha Error lower", "Alpha Error upper", 
               "Alpha:Beta", 
               "Correct", "Correct lower",  "Correct upper")]
    names(out)[c(3, 4, 6, 7, 10, 11)] <- rep(ci_str, 3)
  } else {
    out <- out[c("Method", "Power", "Alpha Error", "Alpha:Beta", "Correct")]
  }
  
  if (is.numeric(binom_test_power)) {
      out$p_power <- x$p_power
      names(out)[which(names(out) == "p_power")] <- paste0("p Power>=", binom_test_power*100)
  }

  if (is.numeric(binom_test_alpha)) {
    out$p_alpha <- x$p_alpha
    names(out)[which(names(out) == "p_alpha")] <- paste0("p Alpha Error<=", binom_test_alpha*100)
  }
    
  if (is.numeric(binom_test_correct)) {
    out$p_correct <- x$p_correct
    names(out)[which(names(out) == "p_correct")] <- paste0("p Correct>=", binom_test_correct*100)
  }

  print(out, row.names = FALSE, digits = digits)
  
  if (duration) 
    cat(
      "\nComputation duration is", 
      round(attr(x, "computation_duration")[3], 1), 
      "seconds.\n"
    )
  
}

#' @describeIn power_test Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @export
export.sc_power <- function(object, caption = NA, footnote = NA, filename = NA,
                            round = 3,
                            ...) {
  
  if (is.na(caption)) {
    caption <- c("Test power in percent")
  }
  
  footnote <- .footnote(footnote, "")

  out <- object
  class(out) <- "data.frame"
 
  out <- round_numeric(out, round)
  
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
