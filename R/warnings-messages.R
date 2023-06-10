return_messages <- function(msg, warning = FALSE) {
  if (length(msg) == 0) return(FALSE)
  msg <- table(msg)
  for(i in seq_along(msg)){
    if (msg[i] > 1) names(msg)[i] <- paste0(names(msg)[i], " (", msg[i], "x)")
  }
  msg <- paste0(1:length(msg), ": ", names(msg), collapse = "\n")
  msg <- paste0("\n", msg, "\n")
  if (warning) warning(msg, call. = FALSE) else message(msg)
}
