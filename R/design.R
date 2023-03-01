#' Generate a single-case design matrix
#'
#' Generates a parameter list used for generating multiple random single-cases.
#' This is used within the `random_scdf` function and the `power_test` function
#' and for other Monte-Carlo tasks.
#'
#' @param n Number of cases to be designed (Default is `n = 1`).
#' @param phase_design,phase.design A list defining the length and label of each
#'   phase. E.g., `phase.length = list(A1 = 10, B1 = 10, A2 = 10, B2 = 10)`. Use
#'   vectors if you want to define different values for each case `phase.length
#'   = list(A = c(10, 15), B = c(10, 15)`.
#' @param mt,MT Number of measurements (in each study). Default is `mt = 20`.
#' @param B_start,B.start Phase B starting point. The default setting `B_start =
#'   6` would assign the first five scores (of each case) to phase A, and all
#'   following scores to phase B. To assign different starting points for a set
#'   of multiple single-cases, use a vector of starting values (e.g., `B_start =
#'   c(6, 7, 8)`). If the number of cases exceeds the length of the vector,
#'   values will be recycled.
#' @param start_value,m Starting value at the first measurement. Default is
#'   `50`. When `distribution = "poission"` the start_value represents
#'   frequency. When `distribution = "binomial"` start_value must range between
#'   0 and 1 and they represent the probability of on event. To assign different
#'   start values to several single-cases, use a vector of values (e.g. `c(50,
#'   42, 56)`). If the number of cases exceeds the length of the vector, values
#'   are recycled. The `m` argument is deprecated.
#' @param s Standard deviation used to calculate absolute values from level,
#'   slope, trend effects and to calculate and error distribution from the `rtt`
#'   values. Set to `10` by default. To assign different variances to several
#'   single-cases, use a vector of values (e.g. `s = c(5, 10, 15)`). If the
#'   number of cases exceeds the length of the vector, values are recycled. if
#'   the distribution is 'poisson' or 'binomial' s is not applied.
#' @param n_trials If \code{distribution} (see below) is `"binomial"`,
#'   `n_trials` is the number of trials/observations/items.
#' @param trend Defines the effect size of a trend added incrementally to each
#'   measurement across the whole data-set. To assign different trends to
#'   several single-cases, use a vector of values (e.g. `trend = c(.1, .3,
#'   .5)`). If the number of cases exceeds the length of the vector, values are
#'   recycled. When using a 'gaussian' distribution, the `trend` parameters
#'   indicate effect size
#'   *d* changes.  When using a binomial or poisson distribution, `trend`
#'   indicates an increase in points / counts per measurement.
#' @param level A list that defines the level increase (effect size *d*) at the
#'   beginning of each phase relative to the previous phase (e.g. `list(A = 0, B
#'   = 1)`). The first element must be zero as the first phase of a single-case
#'   has no level effect (if you have one less list element than the number of
#'   phases, scan will add a leading element with 0 values). Use vectors to
#'   define variable level effects for each case (e.g. `list(A = c(0, 0), B =
#'   c(1, 2))`). When using a 'gaussian' distribution, the `level` parameters
#'   indicate effect size *d* changes. When using a binomial or poisson
#'   distribution, `level` indicates an increase in points / counts with the
#'   onset of each phase.
#' @param slope A list that defines the increase per measurement for each phase
#'   compared to the previous phase. `slope = list(A = 0, B = .1)` generates an
#'   incremental increase of 0.1 per measurement starting at the B phase. The
#'   first list element must be zero as the first phase of a single-case has no
#'   slope effect (if you have one less list element than the number of phases,
#'   scan will add a leading element with 0 values). Use vectors to define
#'   variable slope effects for each case (e.g. `list(A = c(0, 0), B = c(0.1,
#'   0.2))`). If the number of cases exceeds the length of the vector, values
#'   are recycled. When using a 'gaussian' distribution, the `slope` parameters
#'   indicate effect size *d* changes per measurement. When using a binomial or
#'   poisson distribution, `slope` indicates an increase in points / counts per
#'   measurement.
#' @param rtt Reliability of the underlying simulated measurements. Set `rtt =
#'   .8` by default. To assign different reliabilities to several single-cases,
#'   use a vector of values (e.g. `rtt = c(.6, .7, .8)`). If the number of cases
#'   exceeds the length of the vector, values are repeated. `rtt` has no effect
#'   when you're using binomial or poisson distributions.
#' @param extreme_prop,extreme.p Probability of extreme values. `extreme.p =
#'   .05` gives a five percent probability of an extreme value. A vector of
#'   values assigns different probabilities to multiple cases. If the number of
#'   cases exceeds the length of the vector, values are recycled.
#' @param extreme_range,extreme.d Range for extreme values. `extreme_range =
#'   c(-7,-6)` uses extreme values within a range of -7 and -6 . In case of a
#'   binomial or poisson distribution, `extreme_range` indicates frequencies. In
#'   case of a gaussian (or normal) distribution it indicates effect size d.
#'   Caution: the first value must be smaller than the second, otherwise the
#'   procedure will fail.
#' @param missing_prop,missing.p Portion of missing values. `missing_prop = 0.1`
#'   creates 10\% of all values as missing). A vector of values assigns
#'   different probabilities to multiple cases. If the number of cases exceeds
#'   the length of the vector, values are repeated.
#' @param distribution Distribution of the criteria varible. Default is
#'   `"normal"`. Possible values are `"normal"`, `"binomial"`, and `"poisson"`.
#' @return An object of class sc_design.
#' @family mc functions
#' @author Juergen Wibert
#' @keywords datagen
#' @examples
#'
#'  ## Create random single-case data and inspect it
#'  design <- design(
#'    n = 3, rtt = 0.75, slope = 0.1, extreme_prop = 0.1,
#'    missing_prop = 0.1
#'  )
#'  dat <- random_scdf(design, round = 1, random.names = TRUE, seed = 123)
#'  describe(dat)
#'
#'  ## And now have a look at poisson-distributed data
#'  design <- design(
#'    n = 3, B_start = c(6, 10, 14), mt = c(12, 20, 22), start_value = 10,
#'    distribution = "poisson", level = -5, missing_prop = 0.1
#'  )
#'  dat <- random_scdf(design, seed = 1234)
#'  pand(dat, decreasing = TRUE, correction = FALSE)
#' @export
design <- function(n = 1, 
                   phase_design = list(A = 5, B = 15),
                   trend = 0, 
                   level = list(0), 
                   slope = list(0),
                   start_value = 50,
                   s = 10,
                   rtt = 0.80, 
                   extreme_prop = list(0), 
                   extreme_range = c(-4, -3),
                   missing_prop = 0, 
                   distribution = c("normal", "gaussian", "poisson", 
                                    "binomial"),
                   n_trials = NULL,
                   mt = NULL, 
                   B_start = NULL,
                   m,
                   phase.design,
                   MT,
                   B.start,
                   extreme.p,
                   extreme.d,
                   missing.p) {
  
  distribution <- match.arg(distribution)
  
  if (!missing(m)) start_value <- m
  if (!missing(phase.design)) phase_design <- phase.design
  if (!missing(MT)) mt <- MT
  if (!missing(extreme.p)) extreme_prop <- extreme.p
  if (!missing(extreme.d)) extreme_range <- extreme.d
  if (!missing(missing.p)) missing_prop <- missing.p
  if (!missing(B.start)) B_start <- B.start
  out <- list()
  attr(out, "call") <- mget(names(formals()), sys.frame(sys.nframe()))
  
  if (is.list(start_value)) start_value <- unlist(start_value)
  if (is.list(trend)) trend <- unlist(trend)
  if (is.list(s)) s <- unlist(s)
  if (is.list(rtt)) rtt <- unlist(rtt)
  
  
  check_args(
    one_of(distribution, c("normal", "gaussian", "poisson", "binomial")),
    not(distribution == "binomial" && is.null(n_trials),
        "Binomial distributions but n_trials not defined."),
    not(distribution=="binomial" && (any(start_value>1) || any(start_value<0)), 
        "Binomial distributions but start_values outside [0,1]."),
    not(any(B_start < 1) && any(B_start >= 1), 
        "B_start with values below and above 1."),
    within(extreme_prop, 0, 1),
    within(missing_prop, 0, 1)
  )
  
  # start_check() %>%
  #   check_in(distribution, c("normal", "gaussian", "poisson", "binomial")) %>%
  #   check_not(distribution == "binomial" && is.null(n_trials),
  #              "Binomial distributions but n_trials not defined.") %>%
  #   check_not(
  #     distribution=="binomial" && (any(start_value>1) || any(start_value<0)), 
  #     "Binomial distributions but start_values outside [0,1].") %>%
  #   check_not(any(B_start < 1) && any(B_start >= 1),
  #              "B_start with values below and above 1.") %>%
  #   check_within(extreme_prop, 0, 1) %>%
  #   check_within(missing_prop, 0, 1) %>%
  #   end_check()
  
  
  if (!is.null(B_start)) {
    mt <- rep(mt, length.out = n)
    if (B_start[1] == "rand") {
      tmp_start <- round(as.numeric(B_start[2]) * mt)
      tmp_end <- round(as.numeric(B_start[3]) * mt)
      B_start <- round(runif(n, tmp_start, tmp_end))
    }

    if (B_start[1] < 1 && B_start[1] > 0) B_start <- round(B_start * mt) + 1
    B_start <- rep(B_start, length.out = n)

    phase_design <- rep(list(A = rep(NA, n), B = rep(NA, n)))
    for (i in 1:length(B_start)) {
      phase_design$A[i] <- B_start[i] - 1
      phase_design$B[i] <- 1 + mt[i] - B_start[i]
    }
  }

  if (length(start_value) != n) start_value <- rep(start_value, length = n)
  if (length(s) != n) s <- rep(s, length = n)
  if (length(rtt) != n) rtt <- rep(rtt, length = n)
  if (is.list(trend)) trend <- unlist(trend)

  trend <- .check_design(trend, n)
  level <- .check_design(level, n)
  slope <- .check_design(slope, n)
  phase_design <- .check_design(phase_design, n)

  if (length(extreme_prop) != n) {
    extreme_prop <- lapply(numeric(n), function(y) unlist(extreme_prop))
  }
  if (length(extreme_range) != n) {
    extreme_range <- lapply(numeric(n), function(y) unlist(extreme_range))
  }
  if (length(missing_prop) != n) {
    missing_prop <- lapply(numeric(n), function(y) unlist(missing_prop))
  }

  out$cases <- vector("list", n)
  out$distribution <- distribution
  out$n_trials <- n_trials
  
  for (case in 1:n) {
    design <- list()
    design$phase <- names(phase_design)
    design$length <- unlist(lapply(phase_design, function(x) x[case]))
    design$rtt <- rtt[[case]]
    design$missing_prop <- missing_prop[[case]]
    design$extreme_prop <- extreme_prop[[case]]
    design$extreme_low <- extreme_range[[case]][1]
    design$extreme_high <- extreme_range[[case]][2]
    design$trend <- trend[[1]][case]
    design$level <- .design_effect(level, case, length(phase_design))
    design$slope <- .design_effect(slope, case, length(phase_design))
    design$start_value <- start_value[[case]]
    design$s <- s[[case]]

    design$start <- c(1, cumsum(design$length) + 1)[1:length(design$length)]
    design$stop <- cumsum(design$length)

    out$cases[[case]] <- design
  }
  
  class(out) <- c("sc_design")
  
  out
}

.design_effect <- function(effects, case, phase_length) {
  
  case_effects <- unlist(lapply(effects, function(x) x[case]))
  
  if (identical(case_effects, 0)) case_effects <- rep(0, phase_length)
  
  if (length(case_effects) == phase_length && case_effects[1] != 0) {
    warning("Effect for first phase is not 0. Looks like a missspecification")
  }  
    
  if (length(case_effects) == phase_length - 1) 
    case_effects <- c(0, case_effects)
  
  if (length(case_effects) != phase_length) {
    warning("The wrong number of phase effects defined. Looks like a ",
            "missspecification")
  }  
  
  case_effects
  
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
