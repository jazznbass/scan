#' Summary function for an scdf
#'
#' @param object scdf
#' @param all_cases IF TRUE, more that 10 cases are summarized
#' @param ... not in use
#' @export
summary.scdf <- function(object, all_cases = FALSE, ...) {

  N <- length(object)
  
  if (!all_cases) max_cases <- 10 else max_cases <- N
  if (max_cases > N) max_cases <- N
  
  if(N > 1) {
    cat("#A single-case data frame with", number_word(N), "cases\n\n")
  } else {
    cat("#A single-case data frame with one case\n\n")
  }
  
  designs <- lapply(
    object, function(x) 
    paste0(
      rle(as.character(x[[scdf_attr(object, opt("phase"))]]))$values, 
      collapse = "-"
    )
  )
  
  rows <- lapply(object, nrow)

  out <- data.frame(
    " " = format(revise_names(object), justify = "left")[1:max_cases],
    Measurements = unname(unlist(rows))[1:max_cases], 
    Design = unname(unlist(designs))[1:max_cases],
    check.names = FALSE
  )

  print(out, row.names = FALSE)
  
  if (N > max_cases) cat("... [skipped", N - max_cases, "cases]\n")
  
  cat("\n", sep = "")
  
  cat("Variable names:\n")
  names <- names(object[[1]])
  id_dv <- which(names == scdf_attr(object, opt("dv")))
  id_phase <- which(names == scdf_attr(object, opt("phase")))
  id_mt <- which(names == scdf_attr(object, opt("mt")))
  names[id_phase] <- paste(names[id_phase], "<phase variable>")
  names[id_mt] <- paste(names[id_mt], "<measurement-time variable>")
  names[id_dv] <- paste(names[id_dv], "<dependent variable>")
  cat(names[c(
    id_dv, id_phase, id_mt, (1:length(names))[-c(id_dv, id_phase, id_mt)]
  )], sep = "\n")
  cat("\n")
  
  
  if(!is.null(scdf_attr(object, "info"))) {
    cat("Note:", scdf_attr(object, "info"), "\n")
  }
  
  if(!is.null(scdf_attr(object,"author"))) {
    cat("\nAuthor of data:", scdf_attr(object, "author"), "\n")
  }
  
}
