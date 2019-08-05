#' Estimate single-case design
#'
#' This functions takes an scdf and extracts design parameters. The resulting
#' object can be unsed to randomly create new scdf files with the same
#' underlying parameters. This is usefull for monte-carlo studies and
#' bootstrapping procedures.
#'
#' @inheritParams .inheritParams
#' @param m The mean depcting the overall distribution of which all cases are a random sample of.
#' m is estimated when m = NULL.
#' @param s The standard deviation depcting the between case variance of the overall performance.
#' If more than two single-cases are included in the scdf, the variance is estimated if s is set to NULL.
#' @param rtt The reliability of the measurements. The reliability is estimated when rtt = NULL.
#' @param between If FALSE trend, level, and slope effect estimations will be identical for each case.
#' If TRUE effects are estimated for each case seperately.
#' @param ... Further arguments passed to the lm function used for parameter estimation.
#'
#' @return A list of parameters for each single-case. Parameters include name, length, and
#' starting measurementtime of each phase, trend level, and slope effects for each phase, mean,
#' standarddeviation, and reliability for each case.
#' @export

estimate_design <- function(data, dvar, pvar, mvar, m = NULL, s = NULL, rtt = NULL, between = TRUE, model = "JW", ...) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar

  data <- .SCprepareData(data)
  N <- length(data)
  case_names <- names(data)

  if (is.null(case_names)) case_names <- .case.names(names(data), length(data))

  cases <- lapply(data, function(x) {
    df <- as.data.frame(.phasestructure(x, pvar))
    names(df)[1:2] <- c("length", "phase")
    df
  })

  error <- c()
  fitted <- c()

  for (i in 1:N) {
    plm.model <- plm(data[i], model = model, ...)$full
    res <- coef(plm.model)
    n.phases <- nrow(cases[[i]])
    cases[[i]]$m <- res[1]
    cases[[i]]$trend <- res[2]
    cases[[i]]$level <- c(0, res[3:(1 + n.phases)])
    cases[[i]]$slope <- c(0, res[(2 + n.phases):(2 + 2 * (n.phases - 1))])
    cases[[i]]$error <- var(plm.model$residual)
    cases[[i]]$fitted <- var(plm.model$fitted.values)
    cases[[i]]$rtt <- var(plm.model$fitted.values) / (var(plm.model$fitted.values) + var(plm.model$residuals))


    missing.p <- c()
    for (y in 1:nrow(cases[[i]])) {
      tmp <- cases[[i]][y, ]
      missing.p <- c(missing.p, sum(is.na(data[[i]][tmp$start:tmp$stop, dvar])) / tmp$length)
    }
    cases[[i]]$missing.p <- missing.p
    error <- c(error, plm.model$residuals)
    fitted <- c(fitted, plm.model$fitted.values)
  }

  if (!between) {
    level <- rowMeans(sapply(cases, function(x) {
      x$level
    }))
    trend <- rowMeans(sapply(cases, function(x) {
      x$trend
    }))
    slope <- rowMeans(sapply(cases, function(x) {
      x$slope
    }))
    for (i in 1:N) {
      cases[[i]]$trend <- trend
      cases[[i]]$level <- level
      cases[[i]]$slope <- slope
    }
  }

  if (is.null(m)) m <- mean(sapply(cases, function(x) x$m[1]), na.rm = TRUE)
  if (N > 2 && is.null(s)) s <- sd(sapply(cases, function(x) x$m[1]), na.rm = TRUE)

  # if (is.null(rtt)) rtt <- 1 - (error / s^2)

  if (is.null(rtt)) rtt <- var(fitted) / (var(fitted) + var(error))

  error_sd <- sqrt(((1 - rtt) / rtt) * s^2)

  VAR <- c(
    "phase", "length", "mt", "rtt", "error", "missing.p", "extreme.p", "extreme.low",
    "extreme.high", "trend", "level", "slope", "m", "s", "start", "stop"
  )

  for (i in 1:N) {
    cases[[i]]$level <- cases[[i]]$level / s
    cases[[i]]$slope <- cases[[i]]$slope / s
    cases[[i]]$trend <- cases[[i]]$trend / s
    cases[[i]]$error <- error_sd
    cases[[i]]$s <- s
    cases[[i]]$rtt <- rtt
    cases[[i]]$extreme.p <- 0
    cases[[i]]$extreme.low <- -4
    cases[[i]]$extreme.high <- -3
    cases[[i]]$mt <- sum(cases[[i]]$length)
    cases[[i]] <- cases[[i]][[VAR]]
  }

  out <- list(
    cases = cases,
    distribution = "normal",
    prob = NA
  )

  class(out) <- c("sc", "design")
  out
}
