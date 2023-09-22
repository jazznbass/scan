#' Apply a function to each element in an scdf.
#'
#' This function applies a given function to each case of a multiple case scdf,
#' returning a list of the output of each function call.
#'
#' @param scdf A list of inputs to apply the function to.
#' @param fn  The function to apply to each element. Use a `.` as a
#'   placeholder for the scdf (e.g. `describe(.)`).
#' @param simplify If simplify is TRUE and `fn` returns a vector of values,
#'   `batch_apply` will return a data frame case names.
#' @return A list of the output of each function call.
#'
#' @examples
#' batch_apply(exampleAB, coef(plm(.)))
#'
#' @export
batch_apply <- function(scdf, fn, simplify = FALSE) {
  fn <- substitute(fn)
  out <- vector("list", length(scdf))
  for (i in seq_along(scdf)) {
    data <- list(. = scdf[i])
    out[[i]] <- eval(fn, envir = data)
  }
  names(out) <- names(scdf)

  if (simplify) {
    out <- as.data.frame(do.call(rbind, out))
    out$case <- rep(names(scdf), each = nrow(out) / length(names(scdf)))
    out$rownames <- rownames(out)
    rownames(out) <- NULL
    out <- out[, c(ncol(out) - 1, ncol(out), 1:(ncol(out) - 2))]
  }

  out
}
