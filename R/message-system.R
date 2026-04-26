## version 0.1.0-2

#' Throws a message
#'
#' Messages can be shown with different levels
#' of detail and as warnings. This is a convenience wrapper for internal use.
#'
#' @param ... The message. This can be a character string or a
#'   combination of character strings and variables.
#' @param detail The level of detail for the message. Messages with a detail
#'   level higher than the current detail level set in the options will not be
#'   shown. The default is 1.
#' @param warning If TRUE, the message will be shown as a warning. If FALSE
#'   (default), the message will be shown as an informational message.
#' @keywords internal

notify <- function(... , type = "!", detail = 1, warning = FALSE) {
  
  
  if (detail > getOption("wmisc.msg.detail", default = 1))
    return(invisible(NULL))
  
  msg <- paste0(...)
  msg <- gsub("\\n", "\n  ", msg)
  if (type == "h1") {
    msg <- paste0("\u256D\u2500 ", msg)
    msg <- cli::col_blue(msg)
    type <- ""
  }
  
  max_char <- getOption("wmisc.msg.max", default = 100)
  if (nchar(msg) > max_char) {
    msg <- paste0(substring(msg, 1, max_char), "... [truncated]")
  }
  msg <- setNames(msg, type)
  if (!warning) {
    rlang::inform(msg)
  } else {
    rlang::warn(msg)
  }
  invisible(NULL)
}

#' Throws a warning
#' This is a convenience wrapper for internal use.
warn <- function(...) {
  notify(..., warning = TRUE)
}

#' Throws an error
#'
#' This is a convenience wrapper for internal use.
#'
#' @param ... The message. This can be a character string or a
#'   combination of character strings and variables.
#' @param class An optional class or vector of classes to add to the error.
#' @param call If TRUE, the call will be included in the error message. Default
#'   is FALSE.
#' @keywords internal
abort <- function(..., class = NULL, call = FALSE) {
  
  call <- if (isTRUE(call)) {
    rlang::caller_env()
  } else if (identical(call, FALSE) || is.null(call)) {
    NULL
  } else {
    call
  }
  
  msg <- paste0(...)
  rlang::abort(message = msg, class = class, call = call)
  
  invisible(NULL)
}

