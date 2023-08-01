#' @rdname print.sc
#' @export
print.sc_overlap <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") {
    digits_1 <- 0
    digits_2 <- 2
  } else {
    digits_1 <- digits
    digits_2 <- digits
  }
  
  cat("Overlap Indices\n\n")
  #cat("Design: ", x$design, "\n")
  cat(.phases_string(x$phases.A, x$phases.B),"\n\n")
  
  vars <- c("PND", "PEM", "PET", "NAP", "NAP rescaled", "PAND")
  x$overlap[, vars] <- round(x$overlap[, vars], digits_1)
  
  vars <- c(
    "IRD", "Tau_U(A)", "Tau_U(BA)", "Base_Tau", "Diff_mean", 
    "Diff_trend", "SMD", "Hedges_g"
  )
  x$overlap[, vars] <- round(x$overlap[, vars], digits_2)
  
  out <- as.data.frame(t(x$overlap[-1]))
  colnames(out) <- x$overlap$Case
  
  print(out, ...)
  .note_vars(x)
}
