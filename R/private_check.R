
check_args <- function(...) {
  expressions <- substitute(list(...))
  
  env <- new.env(parent = parent.frame()) 
  
  env$is_true <- function(condition, ...) {
    
    if (isFALSE(condition)) {
      message <- paste0(...)
      if (length(message) == 0) {
        message <- paste0(as.character(match.call()[2]), " failed.")
      }
      return(message) 
    } else {
      return(TRUE)
    }
  }
  
  env$has_length <- function(arg, l, msg) {
    if (missing(msg)) 
      msg <- paste0(as.character(match.call()[2]), " not of length ", l, ".")
    env$is_true(length(arg) == l, msg)
  }
  
  env$not <- function(condition, ...) env$is_true(!condition, ...)
  
  env$one_of <- function(arg, ...) {
    match <- c(...)
    msg <- paste0("'", match, "'")
    if (length(match) == 2) msg <- paste0(msg, collapse = " or ")
    if (length(match) > 2) msg <- paste0("one of ", paste0(msg, collapse = ", "))
    env$is_true(
      arg %in% match, 
      as.character(match.call()[2]), " is not ", msg, "."
    )
  }
  
  env$by_call <- function(arg, fn) {
    args <- formals(fn)
    id <- which(names(args) == as.character(match.call()[2]))
    if (length(id) == 0) stop("by_call has no matching arg.")
    match <- eval(args[[id]])
    msg <- paste0("'", match, "'")
    if (length(match) == 2) msg <- paste0(msg, collapse = " or ")
    if (length(match) > 2) msg <- paste0("one of ", paste0(msg, collapse = ", "))
    env$is_true(
      arg %in% match, as.character(match.call()[2]), " is not ", msg, "."
    )
  }
  
  env$within <- function(arg, lower, upper) {
    env$is_true(
      arg >= lower && arg <= upper, 
      as.character(match.call()[2]), 
      " is not within ", lower, " and ", upper, " (is ", arg, ")"
    ) 
  }
  
  env$by_class <- function(param, class, ...) {
    env$is_true(
      inherits(param, class), 
      as.character(match.call()[2]), " is not of class ", class, "."
    )
  }
  
  out <- vector("list", length(expressions) - 1)
  for(i in 2:length(expressions)) {
    out[i - 1] <- eval(expressions[c(1, i)], envir = env)
    
  }
  out <- out[sapply(out, function(x) if (!isTRUE(x)) TRUE else FALSE)]
  
  if (length(out) > 0) {
    out <- paste0(1:length(out), ": ", unlist(out), "\n")
    stop("\n", out, call. = FALSE)
  }
}

# 

utils::globalVariables(c("by_class", "by_call", "not","within","one_of","has_length","is_true"))

