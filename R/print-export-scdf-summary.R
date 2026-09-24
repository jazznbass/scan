#' @describeIn summary.scdf Print the summary
#' @order 2
#' @param x An object of class `scdf_summary`
#' @inheritParams print.sc
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

#' @describeIn summary.scdf Export the summary as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @export
export.scdf_summary <- function(object, 
                                caption = NA, 
                                footnote = NA, 
                                filename = NA,
                                round = 2, 
                                ...) {
  
  names <- names(object[[1]])
  id_dv <- which(names == dv(object))
  id_phase <- which(names == phase(object))
  id_mt <- which(names == mt(object))
  names[id_phase] <- paste(names[id_phase], "(phase variable)")
  names[id_mt] <- paste(names[id_mt], "(measurement-time variable)")
  names[id_dv] <- paste(names[id_dv], "(dependent variable)")
  str_vars <- paste(names[c(
    id_dv, id_phase, id_mt, (1:length(names))[-c(id_dv, id_phase, id_mt)]
  )], collapse = ", ")
  
  footnote <- .footnote(footnote, 
    if (!is.null(scdf_attr(object, "info"))) scdf_attr(object, "info"),
    if (!is.null(scdf_attr(object, "author"))) paste("Author:", scdf_attr(object, "author")),
    paste("Variable names:", str_vars)
  )
    
  N <- cases <- length(object)
  
  if (is.na(caption)) {
    caption <- if(N > 1) {
      paste("A single-case data frame with", number_word(N), "cases")
    } else {
      paste("A single-case data frame with one case")
    }
  }
  
  designs <- lapply(object, function(x) {
    paste0(
      rle(as.character(x[[phase(object)]]))$values, "(",
      rle(as.character(x[[phase(object)]]))$lengths, ")",
      collapse = "-")
  })
  
  rows <- lapply(object, nrow)
  
  out <- data.frame(
    Case = format(revise_names(object), justify = "left")[1:min(N, 10)],
    Measurements = unname(unlist(rows))[1:min(N, 10)], 
    Design = unname(unlist(designs))[1:min(N, 10)],
    check.names = FALSE
  )
  
  opts <- options(scan.export.kable = c(
    list(align = rep("c", ncol(out))), 
    getOption("scan.export.kable")
  ))
  on.exit(options(opts), add = TRUE)
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
