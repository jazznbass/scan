#' @rdname print.sc
#' @export
#' 
print.sc_rci <- function(x, digits = 3, ...) {
  
  cat("Reliable Change Index\n\n")
  cat("Mean Difference = ", round(x$descriptives[2, 2] - x$descriptives[1, 2], digits), "\n")
  cat("Standardized Difference = ", round(x$stand_dif, digits), "\n")
  cat("Standard error of differences = ", round(x$se_dif, digits), "\n")
  cat("Reliability of measurements = ", round(x$reliability, digits), "\n")
  #cat("Reliability of difference scores = ", round(x$rdd, digits), "\n")
  cat("\n")
  cat("Descriptives:\n")
  print(round(x$descriptives, digits))
  cat("\n")
  cat(round(x$conf_percent * 100, digits), "% Confidence Intervals:\n")
  print(round(x$conf, digits))
  cat("\n")
  cat("Reliable Change Indices:\n")
  print(round(x$rci, digits))
  cat("\n")
}
