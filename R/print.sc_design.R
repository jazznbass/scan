#' @rdname print.sc
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
print.sc_design <- function(x, ...) {
  
  cat("A scdf design matrix\n\n")
  cat("Number of cases:", length(x$cases), "\n")
  cat("Distribution: ", x$distribution, "\n")
  cat("Start values: ", unique(
    sapply(x$cases, function(x) {x$start_value[1]})), "\n")
  if (x$distribution %in% c("normal", "gaussian")) {
    cat("SD = ", unique(sapply(x$cases, function(x) {x$s[1]})), "\n")
    cat("rtt = ", unique(sapply(x$cases, function(x) {x$rtt[1]})), "\n")
  }
  cat("Phase design: ", unique(sapply(x$cases, function(x) {
    paste0(x$phase, "=", x$length, collapse = " ")
  })), "\n")
  cat("Trend effect: ", unique(sapply(x$cases, function(x) {x$trend[1]})), "\n")
  cat("Level effect: ", unique(sapply(x$cases, function(x) {
    paste0(x$phase[-1], "=", x$level[-1], collapse = " ")
  })), "\n")
  cat("Slope effect: ", unique(sapply(x$cases, function(x) {
    paste0(x$phase[-1], "=", x$slope[-1], collapse = " ")
  })), "\n")
  cat("Missing proportion: ", unique(sapply(x$cases, function(x) {x$missing_prop})), "\n")
  
  ext_p <- unique(sapply(x$cases, function(x) {x$extreme_prop}))
  cat("Extreme proportion: ", ext_p, "\n")
  if (ext_p != 0) {
    cat("Extreme range: ", unique(sapply(x$cases, function(x) {
      paste0(x$extreme_low, "/", x$extreme_high, collapse = " ")
    })), "\n")
  }
  
}
