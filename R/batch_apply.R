#' Apply a function to each element in an scdf.
#'
#' This function applies a given function to each case of a multiple case scdf,
#' returning a list of the output of each function call.
#'
#' @param scdf    A list of inputs to apply the function to.
#' @param fn      The function to apply to each element. Use a `.` as a
#'   placeholder for the scdf (e.g. `describe(.)`).
#'
#' @return A list of the output of each function call.
#'
#' @examples
#' batch_apply(exampleAB, coef(plm(.)))
#'
#' @export
batch_apply <- function(scdf, fn, simplify = FALSE) {
  fn <- substitute(fn)
  out <- vector("list", length(scdf))
  for(i in seq_along(scdf)) {
    . <- scdf[i]
    out[[i]] <- eval(fn)
  }
  names(out) <- names(scdf)
  
  if (simplify) {
    out <- as.data.frame(do.call(rbind, out))
    out$case <- names(scdf)
    rownames(out) <- NULL
    out <- out[, c(ncol(out), 1:(ncol(out) - 1))]
  }
  
  out
}
