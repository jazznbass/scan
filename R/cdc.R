#' Conservative Dual-Criterion Method
#'
#' The `cdc()` function applies the Conservative Dual-Criterion Method (Fisher,
#' Kelley, & Lomas, 2003) to scdf objects. It compares phase B data points to
#' both phase A mean and trend (OLS, bi-split, tri-split) with an additional
#' increase/decrease of .25 SD. A binomial test against a 50/50 distribution is
#' computed and p-values below .05 are labeled "systematic change".
#'
#' @inheritParams .inheritParams
#' @param trend_method Method used to calculate the trend line. Default is
#'   `trend_method = "OLS"`. Possible values are: `"OLS"`, `"bisplit"`, and
#'   `"trisplit"`. `"bisplit"`, and `"trisplit"` should only be used for cases
#'   with at least five data-points in both relevant phases.
#' @param conservative The CDC method adjusts the original mean and trend lines
#'   by adding (expected increase) or subtracting (expected decrease) an
#'   additional .25 SD before evaluating phase B data. Default is the CDC method
#'   with `conservative = .25`. To apply the Dual-Criterion (DC) method, set
#'   `conservative = 0`.
#' @return 
#'  |  |  |
#'  | --- | --- |
#'  | `cdc` | CDC Evaluation based on a p-value below .05. |
#'  | `cdc_exc` | Number of phase B datapoints indicating expected change. |
#'  | `cdc_nb` | Number of phase B datapoints. |
#'  | `cdc_p` | P value of Binomial Test. |
#'  | `cdc_all` | Overall CDC Evaluation based on all instances/cases of a Multiple Baseline Design. |
#'  | `N` | Number of cases. |
#'  | `decreasing` | Logical argument from function call (see Arguments above). |
#'  | `conservative` | Numeric argument from function call (see Arguments above). |
#'  | `case_names` | Assigned name of single-case. |
#'  | `phases` | - |
#' @author Timo Lueke
#' @references Fisher, W. W., Kelley, M. E., & Lomas, J. E. (2003). Visual Aids
#'   and Structured Criteria for Improving Visual Inspection and Interpretation
#'   of Single-Case Designs. *Journal of Applied Behavior Analysis, 36*,
#'   387-406. https://doi.org/10.1901/jaba.2003.36-387
#' @family overlap functions
#' @keywords overlap
#' @examples
#'
#' ## Apply the CDC method (standard OLS line)
#' design <- design(n = 1, slope = 0.2)
#' dat <- random_scdf(design, seed = 42)
#' cdc(dat)
#'
#' ## Apply the CDC with Koenig's bi-split and an expected decrease in phase B.
#' cdc(exampleAB_decreasing, decreasing = TRUE, trend_method = "bisplit")
#'
#' ## Apply the CDC with Tukey's tri-split, comparing the first and fourth phase
#' cdc(exampleABAB, trend_method = "trisplit", phases = c(1,4))
#'
#' ## Apply the Dual-Criterion (DC) method (i.e., mean and trend without
#' ##shifting).
#' cdc(
#'  exampleAB_decreasing,
#'  decreasing = TRUE,
#'  trend_method = "bisplit",
#'  conservative = 0
#' )
#'
#'
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

  trend_method <- trend_method[1]

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar

  data  <- .prepare_scdf(data, na.rm = TRUE)
  data  <- recombine_phases(data, phases = phases)$data

  n_cases <- length(data)
  cdc_na  <- rep(NA, n_cases)  # total data points in phase A
  cdc_nb  <- rep(NA, n_cases)  # total data points in phase B
  cdc_exc <- rep(NA, n_cases)  # exceeding data points in phase B
  cdc     <- rep(NA, n_cases)  # CDC rule evaluation of change
  cdc_p   <- rep(NA, n_cases)  # binomial p (50/50)
  cdc_all <- NA          # CDC rule evaluation of all "cases"

  for (i in 1:n_cases) {
    A <- data[[i]][data[[i]][, pvar] == "A", ]
    B <- data[[i]][data[[i]][, pvar] == "B", ]
    cdc_na[i] <- nrow(A)
    cdc_nb[i] <- nrow(B)

    if ((cdc_na[i] < 5 || cdc_nb[i] < 5) && trend_method != "OLS") {
      stop(
        "The selected method for trend estimation should not be applied ",
        "with less than five data points per phase.\n"
      )
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
    
    cdc_all <- if (length(cdc_p[cdc_p > .05]) / length(cdc_p) <= .25) {
      "systematic change"
    } else {
      "no change"
    }
  }

  out <- list(
    cdc = cdc,
    cdc_be = cdc_exc,
    cdc_b = cdc_nb,
    cdc_p = cdc_p,
    cdc_all = cdc_all,
    N = n_cases,
    decreasing = decreasing,
    trend_method = trend_method,
    conservative = conservative,
    case_names = revise_names(data)
  )
  class(out) <- c("sc_cdc")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("mt")) <- mvar
  attr(out, opt("dv")) <- dvar
  out
}
