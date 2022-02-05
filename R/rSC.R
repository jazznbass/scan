#' Single-case data generator
#'
#' The \code{rSC} function generates random single-case data frames
#' for monte-carlo studies and demonstration purposes.
#' \code{design_rSC} is used to set up a design matrix with all parameters needed for the \code{rSC} function.
#'
#'
#' @param design A design matrix which is created by design_rSC and specifies
#'   all parameters.
#' @param round Rounds the scores to the defined decimal. To round to the second
#'   decimal, set \code{round = 2}.
#' @param random.names Is \code{FALSE} by default. If set \code{random.names =
#'   TRUE} cases are assigned random first names. If set \code{"neutral", "male" or
#'   "female"} only gender neutral, male, or female names are chosen. The names are drawn from
#'   the 2,000 most popular names for newborns in 2012 in the U.S. (1,000 male
#'   and 1,000 female names).
#' @param seed A seed number for the random generator.
#' @param ... arguments that are directly passed to the design_rSC function for a more concise coding.
#' @param n Number of cases to be designed (Default is \code{n = 1}).
#' @param phase.design A vector defining the length and label of each phase.
#' E.g., \code{phase.length = c(A1 = 10, B1 = 10, A2 = 10, B2 = 10)}.
#' @param MT Number of measurements (in each study). Default is \code{MT = 20}.
#' @param B.start Phase B starting point. The default setting \code{B.start = 6}
#'   would assign the first five scores (of each case) to phase A, and all
#'   following scores to phase B. To assign different starting points for a set
#'   of multiple single-cases, use a vector of starting values (e.g.
#'   \code{B.start = c(6, 7, 8)}). If the number of cases exceeds the length of
#'   the vector, values will be recycled.
#' @param start_value Starting value at the first measurement. Default
#'   is \code{50}. To assign different start values to several single-cases, use a
#'   vector of values (e.g. \code{c(50, 42, 56)}). If the number of cases
#'   exceeds the length of the vector, values are recycled.
#' @param m Deprecated. Use start_value instead.
#' @param s Standard deviation used to calculate absolute values from level,
#'  slope, trend effects and to calculate and error distribution from the 
#'  \code{rtt} values. Set to \code{10} by default. 
#'  To assign different variances to several single-cases, use a vector 
#'  of values (e.g. \code{s = c(5, 10, 15)}). If the number of cases 
#'  exceeds the length of the vector, values are recycled.
#' @param prob If \code{distribution} (see below) is set \code{"binomial"},
#'   \code{prob} passes the probability of occurrence.
#' @param trend Defines the effect size \emph{d} of a trend added incrementally 
#'   to each measurement across the whole data-set. To assign different trends 
#'   to several single-cases, use a vector of values 
#'   (e.g. \code{trend = c(.1, .3, .5)}).
#'   If the number of cases exceeds the length of the vector, values are
#'   recycled. While using a binomial or poisson distribution, \code{d.trend}
#'   indicates an increase in points / counts per MT.
#' @param level Defines the level increase (effect size \emph{d}) at the
#'   beginning of phase B. To assign different level effects to several
#'   single-cases, use a vector of values (e.g. \code{level = c(.2, .4, .6)}).
#'   If the number of cases exceeds the length of the vector, values are
#'   recycled. While using a binomial or poisson distribution, \code{level}
#'   indicates an increase in points / counts with the onset of the B-phase.
#' @param slope Defines the increase in scores - starting with phase B -
#'   expressed as effect size \emph{d} per MT. \code{slope = .1} generates an
#'   incremental increase of 0.1 standard deviations per MT for all phase B
#'   measurements. To assign different slope effects to several single-cases,
#'   use a vector of values (e.g. \code{slope = c(.1, .2, .3)}). If the number
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
#' @param distribution Erro distribution. Default is \code{"normal"}. 
#'   Possible values are \code{"normal"}, \code{"binomial"}, 
#'   and \code{"poisson"}.
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
#'   n = 3, B.start = c(6, 10, 14), MT = c(12, 20, 22), start_value = 10,
#'   distribution = "poisson", level = -5, missing.p = 0.1
#' )
#' dat <- rSC(design, seed = 1234)
#' pand(dat, decreasing = TRUE, correction = FALSE)
#' @name random
NULL


#' @rdname random
#' @export
rSC <- function(design = NULL, 
                round = NA, 
                random.names = FALSE, 
                seed = NULL, 
                ...) {
  
  if (!is.null(seed)) set.seed(seed)
  if (is.numeric(design)) {
    warning("The first argument is expected to be a design matrix created by ", 
            "design_rSC. If you want to set n, please name the first ",
            "argument with n = ...")
    n <- design
    design <- NULL
  }
  if (is.null(design)) design <- design_rSC(...)

  n <- length(design$cases)

  dat <- list()
  
  for (i in 1:n) {
    
    mt <- sum(design$cases[[i]]$length, na.rm = TRUE)
    start_value <- design$cases[[i]]$start_value[1]
    s <- design$cases[[i]]$s[1]
    
    .rtt <- design$cases[[i]]$rtt[1]
    error <- sqrt(((1 - .rtt) / .rtt) * s^2)
    
    trend <- design$cases[[i]]$trend[1]
    level <- design$cases[[i]]$level
    slope <- design$cases[[i]]$slope
    length <- design$cases[[i]]$length
    missing.p <- design$cases[[i]]$missing.p
    extreme.p <- design$cases[[i]]$extreme.p
    extreme.low <- design$cases[[i]]$extreme.low
    extreme.high <- design$cases[[i]]$extreme.high
    
    if (design$distribution == "normal") {
      start_values <- c(start_value, rep(0, mt - 1))
      trend_values <- c(0, rep(trend * s, mt - 1))
      slope_values <- c()
      level_values <- c()
      
      for (j in 1:length(length)) {
        slope_values <- c(slope_values, rep(slope[j] * s, length[j]))
        level_values <- c(level_values, level[j] * s, rep(0, length[j] - 1))
      }

      true_values <- start_values + trend_values + slope_values + level_values
      true_values <- cumsum(true_values)
      error_values <- rnorm(mt, mean = 0, sd = error)
      measured_values <- true_values + error_values
    }

    if (design$distribution %in% c("binomial", "poisson")) {
      trend_values <- c(0, rep(trend, mt - 1))
      slope_values <- c()
      level_values <- c()

      for (j in 1:length(length)) {
        slope_values <- c(slope_values, rep(slope[j], length[j]))
        level_values <- c(level_values, level[j], rep(0, length[j] - 1))
      }

      true_values <- start_values + trend_values + slope_values + level_values
      true_values <- round(cumsum(true_values))
      true_values[true_values < 0] <- 0

      if (design$distribution == "poisson") {
        measured_values <- rpois(n = length(true_values), true_values)
      }
      if (design$distribution == "binomial") {
        measured_values <- rbinom(
          n = length(true_values), 
          size = round(true_values * (1 / design$prob)), 
          prob = design$prob
        )
      }
      
    }

    if (extreme.p[1] > 0) {
      ra <- runif(mt)
      if (design$distribution == "normal") {
        multiplier <- s
      }
      if (design$distribution %in% c("binomial", "poisson")) {
        multiplier <- 1
      }
      for (k in 1:mt) {
        if (ra[k] <= extreme.p[1]) {
          tmp_error <- runif(n = 1, min = extreme.low[1], max = extreme.high[1])
          tmp_error <- tmp_error * multiplier 
          measured_values[k] <- measured_values[k] + tmp_error
        }
      }
    }

    if (missing.p[1] > 0) {
      .indices <- sample(1:mt, missing.p[1] * mt)
      measured_values[.indices] <- NA
    }

    if (!is.na(round)) {
      measured_values <- round(measured_values, round)
    }

    if (design$distribution %in% c("binomial", "poisson")) {
      measured_values[measured_values < 0] <- 0
    }

    condition <- rep(design$cases[[i]]$phase, length)

    dat[[i]] <- data.frame(
      phase = condition, 
      values = measured_values, 
      mt = 1:mt
    )
  }

  if (random.names == "male") names(dat) <- sample(.opt$male.names, n)
  if (random.names == "female") names(dat) <- sample(.opt$female.names, n)
  if (random.names == "neutral") names(dat) <- sample(.opt$neutrals.names, n)
  if (isTRUE(random.names)) names(dat) <- sample(.opt$names, n)

  attributes(dat) <- .default_attributes(attributes(dat))
  dat
}

#' @rdname random
#' @export
design_rSC <- function(n = 1, 
                       phase.design = list(A = 5, B = 15),
                       trend = list(0), 
                       level = list(0), 
                       slope = list(0),
                       rtt = list(0.80), 
                       m = NULL,
                       s = list(10),
                       start_value = list(50),
                       extreme.p = list(0), 
                       extreme.d = c(-4, -3),
                       missing.p = list(0), 
                       distribution = "normal",
                       prob = 0.5, 
                       MT = NULL, 
                       B.start = NULL) {
  
  if (!is.null(B.start)) {
    MT <- rep(MT, length.out = n)
    if (B.start[1] == "rand") {
      tmp.start <- round(as.numeric(B.start[2]) * MT)
      tmp.end <- round(as.numeric(B.start[3]) * MT)
      B.start <- round(runif(n, tmp.start, tmp.end))
    }

    if (any(B.start < 1) && any(B.start >= 1)) {
      stop("A B.start vector must not include values below and above 1 ", 
           "at the same time.")
    }
    if (B.start[1] < 1 && B.start[1] > 0) B.start <- round(B.start * MT) + 1
    B.start <- rep(B.start, length.out = n)

    phase.design <- rep(list(A = rep(NA, n), B = rep(NA, n)))
    for (i in 1:length(B.start)) {
      phase.design$A[i] <- B.start[i] - 1
      phase.design$B[i] <- 1 + MT[i] - B.start[i]
    }
  }

  if (!is.null(m)) {
    warning("The use of the 'm' argument is deprcated. ",
            "Please use 'start_value' instead. ",
            "'m' is used as 'start_value' in this operation.")
    start_value <- m
  }
  
  if (length(start_value) != n) start_value <- rep(start_value, length = n)
  if (length(s) != n) s <- rep(s, length = n)
  if (length(rtt) != n) rtt <- rep(rtt, length = n)
  if (is.list(trend)) trend <- unlist(trend)

  trend <- .check_design(trend, n)
  level <- .check_design(level, n)
  slope <- .check_design(slope, n)
  phase.design <- .check_design(phase.design, n)

  if (length(extreme.p) != n) {
    extreme.p <- lapply(numeric(n), function(y) unlist(extreme.p))
  }
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
    design <- data.frame(phase = names(phase.design))
    design$length <- unlist(lapply(phase.design, function(x) x[case]))
    design$rtt <- rtt[[case]]
    design$missing.p <- missing.p[[case]]
    design$extreme.p <- extreme.p[[case]]
    design$extreme.low <- extreme.d[[case]][1]
    design$extreme.high <- extreme.d[[case]][2]
    design$trend <- trend[[1]][case]
    design$level <- unlist(lapply(level, function(x) x[case]))
    design$level[1] <- 0
    design$slope <- unlist(lapply(slope, function(x) x[case]))
    design$slope[1] <- 0
    design$start_value <- start_value[[case]]
    design$s <- s[[case]]

    design$start <- c(1, cumsum(design$length) + 1)[1:length(design$length)]
    design$stop <- cumsum(design$length)

    out$cases[[case]] <- design
  }
  attr(out, "call") <- mget(names(formals()), sys.frame(sys.nframe()))
  
  class(out) <- c("sc_design")
  
  out
}

.check_design <- function(data, n) {
  if (is.numeric(data)) data <- list(data)
  for (phase in 1:length(data)) {
    if (length(data[[phase]]) != n) {
      data[[phase]] <- rep(data[[phase]], length = n)
    }
  }
  data
}

design.rSC <- function(...) {
  .deprecated_warning("design_rSC", "design.rSC")
  design_rSC(...)
}
