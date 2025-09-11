#' @describeIn autocorr Print method
#' @order 2
#' @param x An object returned by [autocorr()]
#' @export
#' @inheritParams print.sc
print.sc_ac <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 2
  
  cat("Autocorrelations\n\n")
  
  x <- x$autocorr
  for (i in 1:length(x)) {
    x[[i]][, -1] <- round(x[[i]][, -1], digits)
    cat(names(x)[i], "\n")
    print(x[[i]], row.names = FALSE)
    cat("\n")
  }
}

#' @describeIn autocorr Export results to html
#' @order 3
#' @inheritParams export
#' @export
export.sc_ac <- function(object, 
                         caption = NA, 
                         footnote = NA, 
                         filename = NA,
                         round = 3,
                         ...) {
  
  if (is.na(caption)) {
    caption <- paste0(
      "Autocorrelations for variable '", 
      attr(object, opt("dv")),  "'"
    )
  }
 
  x <- object$autocorr
  out <- do.call(rbind, object$autocorr)
  rows <- sapply(object$autocorr, nrow)
  end <- cumsum(sapply(object$autocorr, nrow)) 
  start <- end - rows + 1
  row_group <- mapply(function(start, end) start:end, start, end, SIMPLIFY = FALSE)
  names(row_group) <- names(object$autocorr)
  out <- round_numeric(out, round)
  table <- .create_table(
    out,
    caption = caption,
    footnote = footnote,
    row_group = row_group
  )
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}


