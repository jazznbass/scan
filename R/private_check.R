.start_check <- function() {
  c()
}

.check <- function(x, condition, ...) {
  message <- paste0(...)
  if (isFALSE(condition)) {
    x <- c(x, paste0(length(x) + 1, ": ", message, "\n"))
  }
  x
}

.check_not <- function(x, condition, ...) {
  .check(x, !condition, ...)
}

.check_in <- function(x, arg, match) {
  msg <- paste0("'", match, "'")
  if (length(match) == 2) msg <- paste0(msg, collapse = " or ")
  if (length(match) > 2) msg <- paste0("one of ", paste0(msg, collapse = ", "))
  .check(x, arg %in% match, as.character(match.call()[3]), " is not ", msg, ".")
}

.check_within <- function(x, arg, lower, upper) {
  if (!missing(lower))
    x <- .check_not(
      x, any(unlist(arg) < lower), as.character(match.call()[3]), " < ", lower
    ) 
  if (!missing(upper))
    x <- .check_not(
      x, any(unlist(arg) > upper), as.character(match.call()[3]), " > ", upper
    ) 
  x
}


.end_check <- function(x) {
  if (length(x) > 0) {
    stop("\n", x, call. = FALSE)
  }
}

