#' Summary functio for a scdf
#'
#' @param object scdf
#' @param ... not in use
#' @export
summary.scdf <- function(object, ...) {

  if(length(object) > 1) {
    cat("#A single-case data frame with", length(object), "cases\n\n")
  } else {
    cat("#A single-case data frame with one case\n\n")
  }
  
  designs <- lapply(
    object, function(x) 
    paste0(rle(as.character(x[[scdf_attr(object, .opt$phase)]]))$values, collapse = " ")
  )
  
  rows <- lapply(object, nrow)
  out <- data.frame(Measurements = unlist(rows), Design = unlist(designs))
  if(!is.null(names(object))) row.names(out) <- names(object)
  
  print(out)
  cat("\n", sep = "")
  var.names <- TRUE
  if(var.names) {
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
    cat("\nNote: ", scdf_attr(object, "info"))
  }
  
  if(!is.null(scdf_attr(object,"author"))) {
    cat("\nAuthor of data: ", scdf_attr(object, "author"), "\n")
  }
  
}
