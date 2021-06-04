#' Descriptive statistics for single-case data
#'
#' The \code{describe} function provides common descriptive statistics for
#' single-case data.
#'
#'
#' @inheritParams .inheritParams
#' @return A data frame of descriptive statistics (for each single-case), i.e.:
#' number of observations, number of missing values, measures of central
#' tendency, variation, and trend.
#' @author Juergen Wilbert
#' @seealso \code{\link{overlap}}, \code{\link{plot.scdf}}
#' @examples
#'
#'
#' ## Descriptive statistics for a study of three single-cases
#' describe(Grosche2011)
#'
#' ## Descriptives of a three phase design
#' describe(exampleABC)
#'
#' \dontrun{
#' ## Write descriptive statistics to .csv-file
#' study <- describe(Waddell2011)
#' write.csv(study$descriptives, file = "descriptives.csv")
#' }
#' @export
describe <- function(data, dvar, pvar, mvar) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar

  data.list <- .SCprepareData(data)

  N <- length(data.list)
  case.names <- names(data.list)

  design <- rle(as.character(data.list[[1]][[pvar]]))$values

  while (any(duplicated(design))) {
    design[anyDuplicated(design)] <-
      paste0(design[anyDuplicated(design)], ".phase", anyDuplicated(design))
  }

  VAR <- c("n", "mis", "m", "md", "sd", "mad", "min", "max", "trend")
  VAR2 <- paste0(rep(VAR, each = length(design)), ".", design)

  desc <- as.data.frame(matrix(nrow = N, ncol = length(VAR2)))
  colnames(desc) <- VAR2
  rownames(desc) <- case.names

  for (case in 1:N) {
    data <- data.list[[case]]
    for (i in 1:length(design)) {
      phases <- .phasestructure(data, pvar = pvar)

      x <- data[[mvar]][phases$start[i]:phases$stop[i]]
      y <- data[[dvar]][phases$start[i]:phases$stop[i]]
      phase <- design[i]

      desc[case, paste0("n.", phase)] <- length(y)
      desc[case, paste0("mis.", phase)] <- sum(is.na(y), na.rm = TRUE)
      desc[case, paste0("m.", phase)] <- mean(y, na.rm = TRUE)
      desc[case, paste0("md.", phase)] <- median(y, na.rm = TRUE)
      desc[case, paste0("sd.", phase)] <- sd(y, na.rm = TRUE)
      desc[case, paste0("mad.", phase)] <- mad(y, na.rm = TRUE)
      desc[case, paste0("min.", phase)] <- min(y, na.rm = TRUE)
      desc[case, paste0("max.", phase)] <- max(y, na.rm = TRUE)
      desc[case, paste0("trend.", phase)] <- coef(lm(y ~ I(x - x[1] + 1)))[2]
    }
  }

  out <- list(
    descriptives = desc,
    design = design,
    N = N
  )
  class(out) <- c("sc_desc")
  attr(out, .opt$phase) <- pvar
  attr(out, .opt$mt) <- mvar
  attr(out, .opt$dv) <- dvar

  out
}

#' @rdname describe
#' @export
describeSC <- function(...) {
  #.deprecated_warning("describe", "describeSC")
  describe(...)
}
