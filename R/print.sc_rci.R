#' @rdname print.sc
#' @export
#' 
print.sc_rci <- function(x, ...) {
  
  cat("Reliable Change Index\n\n")
  cat("Mean Difference = ", x$descriptives[2, 2] - x$descriptives[1, 2], "\n")
  cat("Standardized Difference = ", x$stand.dif, "\n")
  cat("\n")
  cat("Descriptives:\n")
  print(x$descriptives)
  cat("\n")
  cat("Reliability = ", x$reliability, "\n")
  cat("\n")
  cat(x$conf.percent * 100, "% Confidence Intervals:\n")
  print(x$conf)
  cat("\n")
  cat("Reliable Change Indices:\n")
  print(x$RCI)
  cat("\n")
}
