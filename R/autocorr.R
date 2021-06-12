#' Autocorrelation for single-case data
#'
#' The autocorr function calculates autocorrelations within each phase and
#' across all phases.
#'
#' @inheritParams .inheritParams
#' @param lag.max The lag up to which autocorrelations will be computed.
#' Default is \code{lag.max = 3}.
#' @param ... Further arguments passed to the \code{\link{acf}} function
#' @return A data frame containing separate autocorrelations for each
#' phase and for all phases (for each single-case). If \code{lag.max} exceeds
#' the length of a phase minus one, NA is returned for this cell.
#' @author Juergen Wilbert
#' @seealso \code{\link{trend}}, \code{\link{plm}}, \code{\link{acf}}
#' @examples
#' ## Compute autocorrelations for a list of four single-cases up to lag 2.
#' autocorr(Huber2014, lag.max = 2)
#' @concept Autocorrelation
#' @concept Serial correlation
#' @export
autocorr <- function(data, dvar, pvar, mvar, lag.max = 3, ...) {
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar

  data <- .SCprepareData(data)

  N <- length(data)
  case_names <- .case.names(names(data), length(data))
  var_lag <- paste0("Lag ", 1:lag.max)
  
  ac <- list()
  for (case in 1:N) {
    phases <- .phasestructure(data[[case]], pvar = pvar)
    
    while (any(duplicated(phases$values))) {
      phases$values[anyDuplicated(phases$values)] <- paste0(
        phases$values[anyDuplicated(phases$values)], 
        "_phase", 
        anyDuplicated(phases$values)
      )
    }
    
    df <- data.frame(Phase = c(phases$values, "all"))
    for (phase in 1:length(phases$values)) {
      y <- data[[case]][phases$start[phase]:phases$stop[phase], dvar]
      if (length(y) - 1 < lag.max) lag <- length(y) - 1 else lag <- lag.max

      .tmp <- acf(y, lag.max = lag, plot = FALSE, ...)$acf[-1]
      df[phase, var_lag[1:lag]] <- .tmp
    }
    
    y <- data[[case]][[dvar]]
    if (length(y) - 1 < lag.max) lag <- length(y) - 1 else lag <- lag.max

    .tmp <- acf(y, lag.max = lag, plot = FALSE, ...)$acf[-1]
    df[length(phases$values) + 1, var_lag[1:lag]] <- .tmp
    
    ac[[case]] <- df
  }
  
  names(ac) <- case_names
  
  out <- list(
    autocorr = ac, 
    dvar = dvar
  )
  class(out) <- c("sc_ac")
  attr(out, .opt$phase) <- pvar
  attr(out, .opt$mt) <- mvar
  attr(out, .opt$dv) <- dvar
  out
}

#' @rdname autocorr
#' @export
autocorrSC <- function(...) {
  .deprecated_warning("autocorr", "autocorrSC")
  autocorr(...)
  }