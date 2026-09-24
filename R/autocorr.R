#' Autocorrelation within and across phases
#'
#' The autocorr function calculates autocorrelations within each phase and
#' across all phases.
#' 
#' Autocorrelations are computed using the [acf()] function from the stats
#' package. For each single-case in the scdf object, a data frame is returned
#' containing the autocorrelations for each phase and for all phases up to the
#' specified lag.
#'
#' @inheritParams .inheritParams
#' @param lag_max,lag.max The lag up to which autocorrelations will be computed.
#' @param na.action How to handle missing values, passed to [acf()]. The
#'   default [na.fail()] stops with an informative message; use [na.pass()] to
#'   compute autocorrelations from the incomplete series. Note that `acf()`
#'   returns `NA` for lags it cannot estimate from the available pairs.
#' @param ... Further arguments passed to the [acf()] function.
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
#' @order 1
#' @export
autocorr <- function(data, dvar, pvar, mvar,
                     lag_max = 3,
                     na.action = na.fail,
                     lag.max,
                     ...) {
  if (!missing(lag.max)) lag_max <- lag.max

  # set defaults attributes
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar

  data <- .prepare_scdf(data)

  if (identical(na.action, na.fail) &&
      any(vapply(data, function(x) anyNA(x[[dvar]]), logical(1)))) {
    abort(
      "Missing values in '", dvar, "'. Use fill_missing() first ",
      "or set na.action (see ?acf)."
    )
  }
  
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
      lag <- min(lag_max, length(y) - 1L)
      if (lag >= 1L) {
        .tmp <- acf(
          y, lag.max = lag, plot = FALSE, na.action = na.action, ...
        )$acf[-1]
        df[phase, var_lag[1:lag]] <- .tmp
      }
    }

    y <- data[[case]][[dvar]]
    
    lag <- min(lag_max, length(y) - 1L)
    if (lag >= 1L) {
      .tmp <- acf(
        y, lag.max = lag, plot = FALSE, na.action = na.action, ...
      )$acf[-1]
      df[length(phases$values) + 1, var_lag[1:lag]] <- .tmp
    }

    ac[[case]] <- df
  }

  names(ac) <- case_names

  out <- list(
    autocorr = ac,
    dvar = dvar
  )
  class(out) <- c("sc_ac")
  attributes(out)[opts("phase", "mt", "dv")] <- list(pvar, mvar, dvar)
  out
}

