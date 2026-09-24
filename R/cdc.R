#' Conservative Dual-Criterion Method
#'
#' Tests whether phase B differs systematically from phase A by comparing every
#' phase B measurement against both the mean and the trend line of phase A
#' (Fisher, Kelley, & Lomas, 2003).
#'
#' @details Both lines are shifted by `conservative` times the standard
#'   deviation of phase A, upwards for an expected increase and downwards for an
#'   expected decrease. A phase B measurement counts as exceeding when it lies
#'   beyond the shifted mean **and** beyond the shifted trend line. The number of
#'   exceeding measurements is tested against chance with a one sided binomial
#'   test at a probability of 0.5; a p value below .05 is labelled `"systematic
#'   change"`, otherwise `"no change"`.
#'
#'   Across the cases of a multiple baseline design, `cdc_all` is
#'   `"systematic change"` when at most a quarter of the cases are not
#'   significant. It stays `NA` as soon as one case could not be evaluated.
#'
#'   Measurements without a value for the dependent variable or the measurement
#'   time are dropped. A case is not evaluated and reported as
#'   `"insufficient data"` with a warning when it has fewer than two complete
#'   measurements in phase A, none in phase B, no two distinct measurement times
#'   in phase A, or — for the two split methods — fewer than five complete
#'   measurements in one of the phases.
#' @inheritParams .inheritParams
#' @param trend_method Method for the trend line of phase A: `"OLS"`,
#'   `"bisplit"` for Koenig's split middle line, or `"trisplit"` for Tukey's
#'   resistant line.
#' @param conservative Proportion of the phase A standard deviation by which the
#'   mean and the trend line are shifted. `conservative = 0` gives the
#'   Dual-Criterion (DC) method.
#' @return An object of class `sc_cdc` with the elements:
#'  |  |  |
#'  | --- | --- |
#'  | `cdc` | Evaluation per case: systematic change, no change, or insufficient data. |
#'  | `cdc_exc` | Number of exceeding measurements in phase B. |
#'  | `cdc_nb` | Number of complete measurements in phase B. |
#'  | `cdc_p` | P value of the binomial test. |
#'  | `cdc_all` | Evaluation across all cases of a multiple baseline design. |
#'  | `N` | Number of cases. |
#'  | `case_names` | Names of the cases. |
#' @author Timo Lueke, Juergen Wilbert
#' @references Fisher, W. W., Kelley, M. E., & Lomas, J. E. (2003). Visual Aids
#'   and Structured Criteria for Improving Visual Inspection and Interpretation
#'   of Single-Case Designs. *Journal of Applied Behavior Analysis, 36*,
#'   387-406. https://doi.org/10.1901/jaba.2003.36-387
#' @family overlap functions
#' @examples
#' cdc(exampleAB)
#'
#' # Koenig's split middle line for data expected to decrease
#' cdc(exampleAB_decreasing, decreasing = TRUE, trend_method = "bisplit")
#'
#' # Tukey's resistant line, comparing the first with the fourth phase
#' cdc(exampleABAB, trend_method = "trisplit", phases = c(1, 4))
#'
#' # the Dual-Criterion method: mean and trend without the shift
#' cdc(exampleAB, conservative = 0)
#' @order 1
#' @export
cdc <- function(data,
                dvar,
                pvar,
                mvar,
                decreasing = FALSE,
                trend_method = c("OLS", "bisplit", "trisplit"),
                conservative = .25,
                phases = c(1, 2)) {

  check_args(
    by_class(decreasing, "logical"),
    by_call(trend_method),
    within(conservative, 0, 1)
  )

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar

  data  <- .prepare_scdf(data)
  data  <- recombine_phases(data, phases = phases)$data

  n_cases <- length(data)
  cdc_na  <- rep(NA, n_cases)  # total data points in phase A
  cdc_nb  <- rep(NA, n_cases)  # total data points in phase B
  cdc_exc <- rep(NA, n_cases)  # exceeding data points in phase B
  cdc     <- rep(NA, n_cases)  # CDC rule evaluation of change
  cdc_p   <- rep(NA, n_cases)  # binomial p (50/50)
  cdc_all <- NA          # CDC rule evaluation of all "cases"

  for (i in 1:n_cases) {
    dat <- data[[i]]
    idx <- complete.cases(dat[, c(dvar, mvar), drop = FALSE])
    A <- dat[which(dat[[pvar]] == "A" & idx), , drop = FALSE]
    B <- dat[which(dat[[pvar]] == "B" & idx), , drop = FALSE]

    cdc_na[i] <- nrow(A)
    cdc_nb[i] <- nrow(B)

    if ((cdc_na[i] < 5 || cdc_nb[i] < 5) && trend_method != "OLS") {
      warn(
        "Case ", i, ": The selected method for trend estimation should not be ",
        "applied with less than five data points per phase."
      )
      cdc[i] <- "insufficient data"
      next
    }

    if (nrow(A) < 2L || nrow(B) < 1L) {
      warn(
        "Case ", i, ": need at least two complete observations ",
        "in phase A and one in phase B."
      )
      cdc[i] <- "insufficient data"
      next
    }

    if (length(unique(A[[mvar]])) < 2L) {
      warn("Case ", i, ": need distinct measurement times in phase A.")
      cdc[i] <- "insufficient data"
      next
    }

    if (trend_method == "bisplit") {
      x <- A[[mvar]]
      y <- A[[dvar]]
      # na.rm = FALSE for now to prevent misuse; will draw no line if NA present
      md1 <- c(
        (median(y[1:floor(length(y) / 2)], na.rm = FALSE)),
        median(x[1:floor(length(x) / 2)], na.rm = FALSE)
      )
      md2 <- c(
        (median(y[ceiling(length(y) / 2 + 1):length(y)], na.rm = FALSE)),
        median(x[ceiling(length(x) / 2 + 1):length(x)], na.rm = FALSE)
      )
      md <- as.data.frame(rbind(md1, md2))
      names(md) <- c(dvar, mvar)
      formula <- as.formula(paste0(dvar, "~", mvar))
      model <- lm(formula, data = md, na.action = na.omit)
    }

    if (trend_method == "trisplit") {
      x <- A[[mvar]]
      y <- A[[dvar]]
      # na.rm = FALSE for now to prevent misuse; will draw no line if NA present
      md1 <- c(
        (median(y[1:floor(length(y) / 3)], na.rm = FALSE)),
        median(x[1:floor(length(x) / 3)], na.rm = FALSE)
      )
      md2 <- c(
        (median(y[ceiling(length(y) / 3 * 2 + 1):length(y)], na.rm = FALSE)),
        median(x[ceiling(length(x) / 3 * 2 + 1):length(x)], na.rm = FALSE)
      )
      md <- as.data.frame(rbind(md1, md2))
      names(md) <- c(dvar, mvar)
      formula <- as.formula(paste0(dvar, "~", mvar))
      model <- lm(formula, data = md, na.action = na.omit)
    }

    if (trend_method == "OLS") {
      formula <- as.formula(paste0(dvar, "~", mvar))
      model <- lm(formula, data = A, na.action = na.omit)
    }

    trnd <- predict(model, B, se.fit = TRUE)

    if (!decreasing) {
      cdc_exc[i] <- sum(
        B[[dvar]] > trnd$fit + (conservative * sd(A[[dvar]])) &
        B[[dvar]] > (mean(A[[dvar]]) + (conservative * sd(A[[dvar]])))
      )
      cdc_p[i] <- binom.test(
        cdc_exc[i], cdc_nb[i], alternative = "greater"
      )$p.value
      cdc[i] <- if (cdc_p[i] < .05) "systematic change" else "no change"
    } else {
      cdc_exc[i] <- sum(
        B[[dvar]] < trnd$fit - (conservative * sd(A[[dvar]])) &
        B[[dvar]] < (mean(A[[dvar]]) - (conservative * sd(A[[dvar]])))
      )
      cdc_p[i] <- binom.test(
        cdc_exc[i], cdc_nb[i], alternative = "greater"
      )$p.value
      cdc[i] <- if (cdc_p[i] < .05) "systematic change" else "no change"
    }
  }

  if (length(cdc_p) > 0 && !anyNA(cdc_p)) {
    cdc_all <- if (mean(cdc_p > .05) <= .25) {
      "systematic change"
    } else {
      "no change"
    }
  }

  out <- list(
    cdc = cdc,
    cdc_exc = cdc_exc,
    cdc_nb = cdc_nb,
    cdc_p = cdc_p,
    cdc_all = cdc_all,
    N = n_cases,
    decreasing = decreasing,
    trend_method = trend_method,
    conservative = conservative,
    case_names = revise_names(data)
  )
  class(out) <- c("sc_cdc")
  attributes(out)[opts("phase", "mt", "dv")] <- list(pvar, mvar, dvar)
  out
}
