#' Summary function for an scdf object
#' 
#' Provides a summary of an `scdf` object, including the number of cases,
#' measurements per case, and design information.
#' 
#' @details The summary includes:
#' - Total number of cases in the `scdf`.
#' - A table listing each case with the number of measurements and design.
#' - Variable names with annotations for phase, measurement-time, and dependent variable.
#' - Additional information and author details if available.
#' 
#' `summary()` returns the summary object; the output is written by its print
#' method. So `summary(scdf)` shows the summary at the console as before, while
#' `export(summary(scdf))` and an assignment stay silent.
#' @author Juergen Wilbert
#' @param object An scdf object
#' @param all_cases If TRUE, more than 10 cases are summarized
#' @param ... not in use
#' @return An object of class `scdf_summary`.
#' @export
summary.scdf <- function(object, all_cases = FALSE, ...) {
  
  attr(object, "all_cases") <- all_cases
  class(object) <- "scdf_summary"
  object
  
}

#' @rdname summary.scdf
#' @param x An object of class `scdf_summary`
#' @export
print.scdf_summary <- function(x, all_cases = NULL, ...) {
  
  if (is.null(all_cases)) all_cases <- isTRUE(attr(x, "all_cases"))
  
  N <- length(x)
  
  if (!all_cases) max_cases <- 10 else max_cases <- N
  if (max_cases > N) max_cases <- N
  
  if(N > 1) {
    cat("#A single-case data frame with", number_word(N), "cases\n\n")
  } else {
    cat("#A single-case data frame with one case\n\n")
  }
  
  designs <- lapply(
    x, function(case) 
    paste0(
      rle(as.character(case[[phase(x)]]))$values, 
      collapse = "-"
    )
  )
  
  rows <- lapply(x, nrow)

  out <- data.frame(
    " " = format(revise_names(x), justify = "left")[1:max_cases],
    Measurements = unname(unlist(rows))[1:max_cases], 
    Design = unname(unlist(designs))[1:max_cases],
    check.names = FALSE
  )

  print(out, row.names = FALSE)
  
  if (N > max_cases) cat("... [skipped", N - max_cases, "cases]\n")
  
  cat("\n", sep = "")
  
  cat("Variable names:\n")
  names <- names(x[[1]])
  id_dv <- which(names == scdf_attr(x, opt("dv")))
  id_phase <- which(names == phase(x))
  id_mt <- which(names == scdf_attr(x, opt("mt")))
  names[id_phase] <- paste(names[id_phase], "<phase variable>")
  names[id_mt] <- paste(names[id_mt], "<measurement-time variable>")
  names[id_dv] <- paste(names[id_dv], "<dependent variable>")
  id_main <- c(id_dv, id_phase, id_mt)
  cat(names[c(id_main, setdiff(seq_along(names), id_main))], sep = "\n")
  cat("\n")
  
  
  if(!is.null(scdf_attr(x, "info"))) {
    cat("Note:", scdf_attr(x, "info"), "\n")
  }
  
  if(!is.null(scdf_attr(x,"author"))) {
    cat("\nAuthor of data:", scdf_attr(x, "author"), "\n")
  }
  
  invisible(x)
  
}
