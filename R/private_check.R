
check_args <- function(...) {
  
  if (!opt("check_arguments")) {
    return()
  }
 
  expressions <- substitute(list(...))

  env <- new.env(parent = parent.frame()) 
 
  env$call <- as.call(
    as.list(parse(text = gsub("^[^:]+::", "", deparse(sys.call(-1)))))
  )[[1]]
  
  env$is_true <- function(condition, ..., .warning = FALSE) {
    
    if (isFALSE(condition)) {
      message <- paste0(...)
      if (length(message) == 0) {
        message <- paste0(
          "Argument ", as.character(match.call()[2]), " is ill defined."
        )
      }
      return(list(pass = FALSE, msg = message, warning = .warning) )
    } else {
      return(list(pass = TRUE))
    }
  }
  
  env$has_length <- function(arg, l, msg, .warning = FALSE) {
    if (missing(msg)) 
      msg <- paste0(
        "Argument ", as.character(match.call()[2]), " not of length ", l, "."
      )
    env$is_true(length(arg) == l, msg)
  }
  
  env$not <- function(condition, ..., .warning = FALSE) env$is_true(!condition, ...)
  
  env$one_of <- function(arg, ..., .warning = FALSE) {
    match <- c(...)
    msg <- paste0("'", match, "'")
    if (length(match) == 2) msg <- paste0(msg, collapse = " or ")
    if (length(match) > 2) {
      msg <- paste0("one of ", paste0(msg, collapse = ", "))
    }
    env$is_true(
      arg %in% match, 
      "Argument ", as.character(match.call()[2]), " is not ", msg, "."
    )
  }
  
  env$by_call <- function(arg, .warning = FALSE) {
    
    args <- formals(as.character(env$call[[1]]))
   
    id <- which(names(args) == as.character(match.call()[2]))
    if (length(id) == 0) stop("by_call has no matching arg.")
    match <- eval(args[[id]])
    msg <- paste0("'", match, "'")
    if (length(match) == 2) msg <- paste0(msg, collapse = " or ")
    if (length(match) > 2) msg <- paste0("one of ", paste0(msg, collapse = ", "))
    arg_name <- as.character(match.call()[2])
    if (identical(arg, match)) {
      caller_env <- parent.frame(n = 4)
      caller_env[[arg_name]] <- arg[1]
      return(list(pass = TRUE))
    }
  
    env$is_true(
      arg %in% match, 
      "Argument ", arg_name, " is not ", msg, "."
    )
  }
  
  env$within <- function(arg, lower, upper, .warning = FALSE) {
    env$is_true(
      arg >= lower && arg <= upper, 
      "Argument ",
      as.character(match.call()[2]), 
      " is not within ", lower, " and ", upper, " (is ", arg, ")"
    ) 
  }
  
  env$at_least <- function(arg, lower, msg, .warning = FALSE) {
    
    if (missing(msg)) {
      msg <- paste0(
        "Argument ",
        as.character(match.call()[2]), 
        " is not greater or equal to ", lower, " (is ", arg, ")"
      )
    }
    
    env$is_true(arg >= lower, msg) 
  }
  
  env$at_most <- function(arg, upper, msg, .warning = FALSE) {
    if(missing(msg)) {
      msg <- paste0(
        "Argument ",
        as.character(match.call()[2]), 
        " is not less or equal to ", upper, " (is ", arg, ")"
      )
    }
    env$is_true(arg <= upper, msg) 
  }
  
  env$by_class <- function(param, class, ..., .warning = FALSE) {
    env$is_true(
      inherits(param, class), 
      "Argument ", 
      as.character(match.call()[2]), " is not of class ", class, "."
    )
  }
  
  env$is_logical <- function(param, .warning = FALSE) {
    env$is_true(
      is.logical(param), 
      "Argument ", as.character(match.call()[2]), " is not logical."
    )
  }
  
  env$is_deprecated <- function() {
    .call <- env$call
    defaults <- formals(as.character(.call[[1]]))
    .call <- as.list(.call)
    id_deprecated <- names(defaults)[
      sapply(defaults, function(x) identical(x, "deprecated")) |> which()
    ]
    id <- which(names(.call) %in% id_deprecated)
  
    if (length(id) > 0) {
      message <- paste0(
        if (length(id) > 1) "Arguments " else "Argument ", 
        paste0("'", names(.call)[id], "'", collapse = ", "),
        if (length(id) > 1) " are " else " is ", 
        "deprecated"
      )
      return(list(pass = FALSE, msg = message, warning = TRUE) )
    }
    return(list(pass = TRUE))
  }
  
  env$as_deprecated <- function(...) {
    
    test_args <- list(...)
    .call <- env$call
    defaults <- formals(as.character(.call[[1]]))
    .call <- as.list(.call)
    deprecated_args <- test_args[which(names(test_args) %in% names(.call))]
    
    if (length(deprecated_args) > 0) {
      message <- paste0(
        "Deprecated ",
        if (length(deprecated_args) > 1) "arguments " else "argument ", 
        paste0("'", names(deprecated_args), "'", collapse = ", "),
        ". Use ",
        paste0("'", unlist(deprecated_args), "'", collapse = ", "),
        " instead"
      )
      return(list(pass = FALSE, msg = message, warning = TRUE) )
    }
    return(list(pass = TRUE))
  }
  
  
  out <- vector("list", length(expressions))
  
  if (length(out) > 0) {
    for(i in 2:length(expressions)) {
      out[i - 1] <- eval(expressions[c(1, i)], envir = env)
    }
  }  
  
  out[length(out)] <- list(eval(str2lang("is_deprecated()"), envir = env))

  out <- out[sapply(out, function(x) if (!isTRUE(x$pass)) TRUE else FALSE)]

  error_msg <- lapply(out, function(x) {
    if (!x$warning) x$msg else NULL
  }) |> unlist()
  
  warning_msg <- lapply(out, function(x) {
    if (x$warning) x$msg else NULL
  }) |> unlist()
  
  if (length(warning_msg) > 0) {
    warning_msg <- paste0(1:length(warning_msg), ": ", warning_msg, "\n")
    warning(warning_msg, call. = FALSE)
  }
  
  if (length(error_msg) > 0) {
    error_msg <- paste0(1:length(error_msg), ": ", error_msg, "\n")
    stop("\n", error_msg, call. = FALSE)
  }
}

# 

utils::globalVariables(
  c(
    "by_class", "by_call", "not","within", "one_of", "has_length", "is_true",
    "at_least", "at_most", "is_logical", "as_deprecated"
  )
)

