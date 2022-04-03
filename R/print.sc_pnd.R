#' @rdname print.sc
#' @export
#' 
print.sc_pnd <- function(x, ...) {
  cat("Percent Non-Overlapping Data\n\n")
  out <- data.frame(
    Case = x$case.names, 
    PND = paste0(round(x$PND, 2),"%"), 
    "Total" = x$n.B, 
    "Exceeds" = round(x$PND / 100 * x$n.B)
  )
  print(out, row.names = FALSE)
  cat("\nMean  :", round(mean(x$PND, na.rm = TRUE), 2),"%\n")
}	
