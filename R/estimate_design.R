#' Estimate single-case design
#'
#' This functions takes an scdf and extracts design parameters. The resulting
#' object can be used to randomly create new scdf files with the same
#' underlying parameters. This is useful for monte-carlo studies and
#' bootstrapping procedures.
#'
#' @inheritParams .inheritParams
#' @param m The mean depicting the overall distribution of which all cases are a random sample of.
#' m is estimated when m = NULL.
#' @param s The standard deviation depicting the between case variance of the overall performance.
#' If more than two single-cases are included in the scdf, the variance is estimated if s is set to NULL.
#' @param rtt The reliability of the measurements. The reliability is estimated when rtt = NULL.
#' @param between If FALSE trend, level, and slope effect estimations will be identical for each case.
#' If TRUE effects are estimated for each case separately.
#' @param ... Further arguments passed to the lm function used for parameter estimation.
#'
#' @return A list of parameters for each single-case. Parameters include name, length, and
#' starting measurement time of each phase, trend level, and slope effects for each phase, mean,
#' standard deviation, and reliability for each case.
#' @export

estimate_design <- function(data, dvar, pvar, mvar, 
                            m = NULL, 
                            s = NULL, 
                            rtt = NULL, 
                            between = TRUE, 
                            model = "JW", 
                            ...) {

  # set defaults attributes
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) 
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) 
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) 
  
  scdf_attr(data, .opt$dv) <- dvar
  scdf_attr(data, .opt$phase) <- pvar
  data <- .prepare_scdf(data)
  N <- length(data)
  case_names <- names(data)

  if (is.null(case_names)) case_names <- .case_names(names(data), length(data))

  cases <- lapply(data, function(x) {
    df <- as.data.frame(.phasestructure(x, pvar))
    names(df)[1:2] <- c("length", "phase")
    df
  })

  error <- c()
  fitted <- c()

  for (i in 1:N) {
    plm_model <- plm(data[i], model = model, ...)$full
    res <- coef(plm_model)
    var_fitted <- var(plm_model$fitted.values)
    var_residuals <- var(plm_model$residuals)
    n_phases <- nrow(cases[[i]])
    cases[[i]]$m <- res[1]
    cases[[i]]$trend <- res[2]
    cases[[i]]$level <- c(0, res[3:(1 + n_phases)])
    cases[[i]]$slope <- c(0, res[(2 + n_phases):(2 + 2 * (n_phases - 1))])
    cases[[i]]$error <- var_residuals
    cases[[i]]$fitted <- var_fitted
    cases[[i]]$rtt <- var_fitted / (var_fitted + var_residuals)


    missing.p <- c()
    for (y in 1:nrow(cases[[i]])) {
      tmp <- cases[[i]][y, ]
      missing.p <- c(
        missing.p, 
        sum(is.na(data[[i]][tmp$start:tmp$stop, dvar])) / tmp$length
      )
    }
    cases[[i]]$missing.p <- missing.p
    error <- c(error, plm_model$residuals)
    fitted <- c(fitted, plm_model$fitted.values)
  }

  if (!between) {
    level <- rowMeans(sapply(cases, function(x) x$level))
    trend <- rowMeans(sapply(cases, function(x) x$trend))
    slope <- rowMeans(sapply(cases, function(x) x$slope))
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

  vars <- c(
    "phase", "length", "mt", "rtt", "error", "missing.p", "extreme.p", 
    "extreme.low", "extreme.high", "trend", "level", "slope", "m", "s", 
    "start", "stop"
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
    cases[[i]] <- cases[[i]][vars]
  }

  out <- list(
    cases = cases,
    distribution = "normal",
    prob = NA
  )

  class(out) <- c("sc_design")
  attr(out, .opt$phase) <- pvar
  attr(out, .opt$mt) <- mvar
  attr(out, .opt$dv) <- dvar
  out
}
