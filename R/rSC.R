#' Single-case data generator
#'
#' The \code{rSC} function generates random single-case data frames
#' for monte-carlo studies and demonstration purposes.
#' \code{design_rSC} is used to set up a design matrix with all parameters needed for the \code{rSC} function.
#'
#'
#' @param design A design matrix which is created by design_rSC and specifies
#'   all paramters.
#' @param round Rounds the scores to the defined decimal. To round to the second
#'   decimal, set \code{round = 2}.
#' @param random.names Is \code{FALSE} by default. If set \code{random.names =
#'   TRUE} cases are assigned random first names. If set \code{"male" or
#'   "female"} only male or female names are chosen. The names are drawn from
#'   the 2,000 most popular names for newborns in 2012 in the U.S. (1,000 male
#'   and 1,000 female names).
#' @param seed A seed number for the random generator.
#' @param ... Paramteres that are directly passed from the rSC function to the design_rSC function for a more concise coding.
#' @param n Number of cases to be created (Default is \code{n = 1}).
#' @param phase.design A vector defining the length and label of each phase.
#' E.g., \code{phase.length = c(A1 = 10, B1 = 10, A2 = 10, B2 = 10)}.
#' @param MT Number of measurements (in each study). Default is \code{MT = 20}.
#' @param B.start Phase B starting point. The default setting \code{B.start = 6}
#'   would assign the first five scores (of each case) to phase A, and all
#'   following scores to phase B. To assign different starting points for a set
#'   of multiple single-cases, use a vector of starting values (e.g.
#'   \code{B.start = c(6, 7, 8)}). If the number of cases exceeds the length of
#'   the vector, values will be repeated.
#' @param m Mean of the sample distribution the scores are drawn from. Default
#'   is \code{m = 50}. To assign different means to several single-cases, use a
#'   vector of values (e.g. \code{m = c(50, 42, 56)}). If the number of cases
#'   exceeds the length of the vector, values are repeated.
#' @param s Standard deviation of the sample distribution the scores are drawn
#'   from. Set to \code{s = 10} by default. To assign different variances to
#'   several single-cases, use a vector of values (e.g. \code{s = c(5, 10,
#'   15)}). If the number of cases exceeds the length of the vector, values are
#'   repeated.
#' @param prob If \code{distribution} (see below) is set \code{"binomial"},
#'   \code{prob} passes the probability of occurrence.
#' @param trend Defines the effect size \emph{d} of a trend per MT added
#'   across the whole data-set. To assign different trends to several
#'   single-cases, use a vector of values (e.g. \code{trend = c(.1, .3, .5)}).
#'   If the number of cases exceeds the length of the vector, values are
#'   repeated. While using a binomial or poisson distribution, \code{d.trend}
#'   indicates an increase in points / counts per MT.
#' @param level Defines the level increase (effect size \emph{d}) at the
#'   beginning of phase B. To assign different level effects to several
#'   single-cases, use a vector of values (e.g. \code{d.level = c(.2, .4, .6)}).
#'   If the number of cases exceeds the length of the vector, values are
#'   repeated. While using a binomial or poisson distribution, \code{d.level}
#'   indicates an increase in points / counts with the onset of the B-phase.
#' @param slope Defines the increase in scores - starting with phase B -
#'   expressed as effect size \emph{d} per MT. \code{d.slope = .1} generates an
#'   incremental increase of 0.1 standard deviations per MT for all phase B
#'   measurements. To assign different slope effects to several single-cases,
#'   use a vector of values (e.g. \code{d.slope = c(.1, .2, .3)}). If the number
#'   of cases exceeds the length of the vector, values are repeated. While using
#'   a binomial or poisson distribution, \code{d.slope} indicates an increase in
#'   points / counts per MT.
#' @param rtt Reliability of the underlying simulated measurements. Set
#'   \code{rtt = .8} by default. To assign different reliabilities to several
#'   single-cases, use a vector of values (e.g. \code{rtt = c(.6, .7, .8)}). If
#'   the number of cases exceeds the length of the vector, values are repeated.
#'   \code{rtt} has no effect when you're using binomial or poisson distributed
#'   scores.
#' @param extreme.p Probability of extreme values. \code{extreme.p = .05} gives
#'   a five percent probability of an extreme value. A vector of values assigns
#'   different probabilities to multiple cases. If the number of cases exceeds
#'   the length of the vector, values are repeated.
#' @param extreme.d Range for extreme values, expressed as effect size \emph{d}.
#'   \code{extreme.d = c(-7,-6)} uses extreme values within a range of -7 and -6
#'   standard deviations. In case of a binomial or poisson distribution,
#'   \code{extreme.d} indicates points / counts. Caution: the first value must
#'   be smaller than the second, otherwise the procedure will fail.
#' @param missing.p Portion of missing values. \code{missing.p = 0.1} creates
#'   10\% of all values as missing). A vector of values assigns different
#'   probabilities to multiple cases. If the number of cases exceeds the length
#'   of the vector, values are repeated.
#' @param distribution Distribution of the scores. Default is \code{distribution
#'   = "normal"}. Possible values are \code{"normal"}, \code{"binomial"}, and
#'   \code{"poisson"}. If set to \code{"normal"}, the sample of scores will be
#'   normally distributed with the parameters \code{m} and \code{s} as mean and
#'   standard deviation of the sample, including a measurement error defined by
#'   \code{rtt}. If set to \code{"binomial"}, data are drawn from a binomial
#'   distribution with the expectation value \code{m}. This setting is useful
#'   for generating criterial data like correct answers in a test. If set to
#'   \code{"poisson"}, data are drawn from a poisson distribution, which is very
#'   common for count-data like behavioral observations. There's no measurement
#'   error is included. \code{m} defines the expectation value of the poisson
#'   distribution, lambda.
#' @return A single-case data frame. See \code{\link{scdf}} to learn about this format.
#' @author Juergen Wibert
#' @keywords datagen
#' @examples
#'
#' ## Create random single-case data and inspect it
#' design <- design_rSC(
#'   n = 3, rtt = 0.75, slope = 0.1, extreme.p = 0.1,
#'   missing.p = 0.1
#' )
#' dat <- rSC(design, round = 1, random.names = TRUE, seed = 123)
#' describeSC(dat)
#' plotSC(dat)
#'
#' ## And now have a look at poisson-distributed data
#' design <- design_rSC(
#'   n = 3, B.start = c(6, 10, 14), MT = c(12, 20, 22), m = 10,
#'   distribution = "poisson", level = -5, missing.p = 0.1
#' )
#' dat <- rSC(design, seed = 1234)
#' pand(dat, decreasing = TRUE, correction = FALSE)
#' @name random
NULL


#' @rdname random
#' @export
rSC <- function(design = NULL, round = NA, random.names = FALSE, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)

  if (is.numeric(design)) {
    warning("The first argument is expected to be a design matrix created by design_rSC. If you want to set n, please name the first argument with n = ...")
    n <- design
    design <- NULL
  }
  if (is.null(design)) design <- design_rSC(...)

  n <- length(design$cases)

  cases <- design$cases
  distribution <- design$distribution
  prob <- design$prob

  dat <- list()
  for (i in 1:n) {
    if (distribution == "normal") {
      start_values <- c(cases[[i]]$m[1], rep(0, cases[[i]]$mt[1] - 1))
      trend_values <- c(0, rep(cases[[i]]$trend[1] * cases[[i]]$s[1], cases[[i]]$mt[1] - 1))
      slope_values <- c()
      level_values <- c()
      for (j in 1:nrow(cases[[i]])) {
        slope_values <- c(slope_values, rep(cases[[i]]$slope[j] * cases[[i]]$s[j], cases[[i]]$length[j]))
        level_values <- c(level_values, cases[[i]]$level[j] * cases[[i]]$s[j], rep(0, cases[[i]]$length[j] - 1))
      }

      true_values <- start_values + trend_values + slope_values + level_values
      true_values <- cumsum(true_values)
      error_values <- rnorm(cases[[i]]$mt[1], mean = 0, sd = cases[[i]]$error[1])
      measured_values <- true_values + error_values
    }

    if (distribution == "poisson" || distribution == "binomial") {
      start_values <- c(cases[[i]]$m[1], rep(0, cases[[i]]$mt[1] - 1))
      trend_values <- c(0, rep(cases[[i]]$trend[1], cases[[i]]$mt[1] - 1))
      slope_values <- c()
      level_values <- c()

      for (j in 1:nrow(cases[[i]])) {
        slope_values <- c(slope_values, rep(cases[[i]]$slope[j], cases[[i]]$length[j]))
        level_values <- c(level_values, cases[[i]]$level[j], rep(0, cases[[i]]$length[j] - 1))
      }

      true_values <- start_values + trend_values + slope_values + level_values
      true_values <- round(cumsum(true_values))
      true_values[true_values < 0] <- 0

      if (distribution == "poisson") {
        measured_values <- rpois(n = length(true_values), true_values)
      }
      if (distribution == "binomial") {
        measured_values <- rbinom(n = length(true_values), size = round(true_values * (1 / prob)), prob = prob)
      }
    }


    if (cases[[i]]$extreme.p[1] > 0) {
      ra <- runif(cases[[i]]$mt[1])
      if (distribution == "normal") {
        multiplier <- cases[[i]]$s[1]
      }
      if (distribution == "binomial" || distribution == "poisson") {
        multiplier <- 1
      }
      for (k in 1:cases[[i]]$mt[1]) {
        if (ra[k] <= cases[[i]]$extreme.p[1]) {
          measured_values[k] <- measured_values[k] + (runif(1, cases[[i]]$extreme.low[1], cases[[i]]$extreme.high[1]) * multiplier)
        }
      }
    }

    if (cases[[i]]$missing.p[1] > 0) {
      measured_values[sample(1:cases[[i]]$mt[1], cases[[i]]$missing.p[1] * cases[[i]]$mt[1])] <- NA
    }

    if (!is.na(round)) {
      measured_values <- round(measured_values, round)
    }

    if (distribution == "binomial" || distribution == "poisson") {
      measured_values[measured_values < 0] <- 0
    }

    condition <- rep(cases[[i]]$phase, cases[[i]]$length)

    dat[[i]] <- data.frame(phase = condition, values = measured_values, mt = 1:cases[[i]]$mt[1])
  }

  if (random.names == "male") names(dat) <- sample(.opt$male.names, n)
  if (random.names == "female") names(dat) <- sample(.opt$female.names, n)

  if (isTRUE(random.names)) names(dat) <- sample(.opt$names, n)

  attributes(dat) <- .defaultAttributesSCDF(attributes(dat))
  dat
}

#' @rdname random
#' @export
design_rSC <- function(n = 1, phase.design = list(A = 5, B = 15),
                       trend = list(0), level = list(0), slope = list(0),
                       rtt = list(0.80), m = list(50), s = list(10),
                       extreme.p = list(0), extreme.d = c(-4, -3),
                       missing.p = list(0), distribution = "normal",
                       prob = 0.5, MT = NULL, B.start = NULL) {
  if (!is.null(B.start)) {
    MT <- rep(MT, length.out = n)
    if (B.start[1] == "rand") {
      tmp.start <- round(as.numeric(B.start[2]) * MT)
      tmp.end <- round(as.numeric(B.start[3]) * MT)
      B.start <- round(runif(n, tmp.start, tmp.end))
    }

    if (any(B.start < 1) && any(B.start >= 1)) {
      stop("A B.start vector must not include values below and above 1 at the same time.")
    }
    if (B.start[1] < 1 && B.start[1] > 0) B.start <- round(B.start * MT) + 1
    B.start <- rep(B.start, length.out = n)

    phase.design <- rep(list(A = rep(NA, n), B = rep(NA, n)))
    for (i in 1:length(B.start)) {
      phase.design$A[i] <- B.start[i] - 1
      phase.design$B[i] <- 1 + MT[i] - B.start[i]
    }
  }

  if (length(m) != n) m <- rep(m, length = n)
  if (length(s) != n) s <- rep(s, length = n)
  if (length(rtt) != n) rtt <- rep(rtt, length = n)
  if (is.list(trend)) trend <- unlist(trend)

  trend <- .check.designSC(trend, n)
  level <- .check.designSC(level, n)
  slope <- .check.designSC(slope, n)
  phase.design <- .check.designSC(phase.design, n)

  if (length(extreme.p) != n) {
    extreme.p <- lapply(numeric(n), function(y) unlist(extreme.p))
  }
  # or:        extreme.p <- rep(n, list(unlist(extreme.p)))
  if (length(extreme.d) != n) {
    extreme.d <- lapply(numeric(n), function(y) unlist(extreme.d))
  }
  if (length(missing.p) != n) {
    missing.p <- lapply(numeric(n), function(y) unlist(missing.p))
  }

  out <- list()
  out$cases <- vector("list", n)
  out$distribution <- distribution
  out$prob <- prob

  for (case in 1:n) {
    error <- sqrt(((1 - rtt[[case]]) / rtt[[case]]) * s[[case]]^2)
    design <- data.frame(phase = names(phase.design))
    design$length <- unlist(lapply(phase.design, function(x) x[case]))
    design$mt <- sum(design$length)
    design$rtt <- rtt[[case]]
    design$error <- error
    design$missing.p <- missing.p[[case]]
    design$extreme.p <- extreme.p[[case]]
    design$extreme.low <- extreme.d[[case]][1]
    design$extreme.high <- extreme.d[[case]][2]
    design$trend <- trend[[1]][case]
    design$level <- unlist(lapply(level, function(x) x[case]))
    design$level[1] <- 0
    design$slope <- unlist(lapply(slope, function(x) x[case]))
    design$slope[1] <- 0
    design$m <- m[[case]]
    design$s <- s[[case]]

    design$start <- c(1, cumsum(design$length) + 1)[1:length(design$length)]
    design$stop <- cumsum(design$length)

    out$cases[[case]] <- design
  }
  attr(out, "call") <- mget(names(formals()), sys.frame(sys.nframe()))
  out
}

.check.designSC <- function(data, n) {
  if (is.numeric(data)) data <- list(data)
  for (phase in 1:length(data)) {
    if (length(data[[phase]]) != n) {
      data[[phase]] <- rep(data[[phase]], length = n)
    }
  }
  data
}

design.rSC <- function(...) {
  warning(.opt$function_deprecated_warning, "Please use describe_rSC instead")
  design_rSC(...)
}
