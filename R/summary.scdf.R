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
    cat("#A single-case data frame with", N, "cases\n\n")
  } else {
    cat("#A single-case data frame with one case\n\n")
  }
  
  designs <- lapply(
    object, function(x) 
    paste0(
      rle(as.character(x[[scdf_attr(object, .opt$phase)]]))$values, 
      collapse = "-"
    )
  )
  
  rows <- lapply(object, nrow)

  out <- data.frame(
    " " = format(.case_names(object), justify = "left")[1:max_cases],
    Measurements = unname(unlist(rows))[1:max_cases], 
    Design = unname(unlist(designs))[1:max_cases],
    check.names = FALSE
  )

  print(out, row.names = FALSE)
  
  if (N > max_cases) cat("... [skipped", N - max_cases, "cases]\n")
  
  cat("\n", sep = "")
  var_names <- TRUE
  if(var_names) {
    cat("Variable names:\n")
    name.tmp <- names(object[[1]])
    n.tmp <- which(name.tmp == scdf_attr(object, .opt$dv))
    name.tmp[n.tmp] <- paste0(name.tmp[n.tmp], " <dependent variable>")
    n.tmp <- which(name.tmp == scdf_attr(object, .opt$phase))
    name.tmp[n.tmp] <- paste0(name.tmp[n.tmp], " <phase variable>")
    n.tmp <- which(name.tmp == scdf_attr(object, .opt$mt))
    name.tmp[n.tmp] <- paste0(name.tmp[n.tmp], " <measurement-time variable>")
    cat(name.tmp, sep = "\n")
    cat("\n")
  }
  
  if(!is.null(scdf_attr(object, "info"))) {
    cat("Note:", scdf_attr(object, "info"), "\n")
  }
  
  if(!is.null(scdf_attr(object,"author"))) {
    cat("\nAuthor of data:", scdf_attr(object, "author"), "\n")
  }
  
}
