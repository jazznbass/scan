#' Autocorrelation for single-case data
#'
#' The autocorr function calculates autocorrelations within each phase and
#' across all phases.
#'
#' @inheritParams .inheritParams
#' @param lag_max,lag.max The lag up to which autocorrelations will be computed.
#' @param ... Further arguments passed to the [acf()] function
#' @return A data frame containing separate autocorrelations for each phase and
#'   for all phases (for each single-case). If `lag_max` exceeds the length
#'   of a phase minus one, NA is returned for this cell.
#' @author Juergen Wilbert
#' @seealso [acf()]
#' @family regression functions
#' @keywords regression
#' @examples
#' ## Compute autocorrelations for a list of four single-cases up to lag 2.
#' autocorr(Huber2014, lag_max = 2)
#' @concept Autocorrelation
#' @concept Serial correlation
#' @export
autocorr <- function(data, dvar, pvar, mvar,
                     lag_max = 3,
                     lag.max,
                     ...) {
  if (!missing(lag.max)) lag_max <- lag.max

  # set defaults attributes
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar

  data <- .prepare_scdf(data)

  case_names <- revise_names(data)
  var_lag <- paste0("Lag ", 1:lag_max)

  ac <- list()
  for (case in seq_along(data)) {
    phases <- .phasestructure(data[[case]], pvar = pvar)

    while (any(duplicated(phases$values))) {
      phases$values[anyDuplicated(phases$values)] <- paste0(
        phases$values[anyDuplicated(phases$values)],
        "_phase",
        anyDuplicated(phases$values)
      )
    }

    df <- data.frame(Phase = c(phases$values, "all"))
    for (phase in seq_along(phases$values)) {
      y <- data[[case]][phases$start[phase]:phases$stop[phase], dvar]
      if (length(y) - 1 < lag_max) lag <- length(y) - 1 else lag <- lag_max

      .tmp <- acf(y, lag.max = lag, plot = FALSE, ...)$acf[-1]
      df[phase, var_lag[1:lag]] <- .tmp
    }

    y <- data[[case]][[dvar]]
    if (length(y) - 1 < lag_max) lag <- length(y) - 1 else lag <- lag_max

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
  attr(out, opt("phase")) <- pvar
  attr(out, opt("mt")) <- mvar
  attr(out, opt("dv")) <- dvar
  out
}

