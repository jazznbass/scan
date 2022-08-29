#' @rdname print.sc
#' @param duration If TRUE the duration for computation is printed.
#' @export
print.sc_power <- function(x, duration = FALSE, digits = 1, ...) {
  
  cat("Test-Power in percent:\n\n")
  
  class(x) <- "data.frame"
  
  out <- x
  
  ci <- attr(x, "ci")
  binom_test <- attr(x, "binom_test")
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
    
  if (is.numeric(binom_test_power)) {
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
