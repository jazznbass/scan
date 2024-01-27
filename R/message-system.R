messages <- new.env()
messages$messages <- c()
messages$last_messages <- c()

reset_messages <- function() {
  messages$last_messages <- messages$messages
  messages$messages <- c()
}

add_message <- function(..., collapse = "") {
  msg <- paste(c(...), collapse = collapse)
  messages$messages <- c(messages$messages, msg)
}

print_messages <- function(warning = FALSE) {
  msg <- messages$messages
  if (length(msg) == 0) return(FALSE)
  msg <- table(msg)
  for(i in seq_along(msg)){
    if (msg[i] > 1) names(msg)[i] <- paste0(names(msg)[i], " (", msg[i], "x)")
  }
  msg <- paste0(1:length(msg), ": ", names(msg), collapse = "\n")
  msg <- paste0("\n", msg, "\n")
  
  if (warning) warning(msg, call. = FALSE) else message(msg)
  
  reset_messages()
}


