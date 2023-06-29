#' Single-case data generator
#'
#' The \code{random_scdf} function generates random single-case data frames for
#' monte-carlo studies and demonstration purposes. \code{design} is used to set
#' up a design matrix with all parameters needed for the \code{random_scdf}
#' function.
#'
#'
#' @param design A design matrix which is created by \code{design} and specifies
#'   all parameters.
#' @param round Rounds the scores to the defined decimal. To round to the second
#'   decimal, set \code{round = 2}.
#' @param random_names Is \code{FALSE} by default. If set \code{random_names =
#'   TRUE} cases are assigned random first names. If set \code{"neutral", "male"
#'   or "female"} only gender neutral, male, or female names are chosen. The
#'   names are drawn from the 2,000 most popular names for newborns in 2012 in
#'   the U.S. (1,000 male and 1,000 female names).
#' @param seed A seed number for the random generator.
#' @param ... arguments that are directly passed to the \code{design} function
#'   for a more concise coding.
#' @return A single-case data frame. See \code{\link{scdf}} to learn about this
#'   format.
#' @family mc fucntions
#' @author Juergen Wibert
#' @keywords datagen
#' @export
#' @examples
#'
#' ## Create random single-case data and inspect it
#' design <- design(
#'   n = 3, rtt = 0.75, slope = 0.1, extreme_prop = 0.1,
#'   missing_prop = 0.1
#' )
#' dat <- random_scdf(design, round = 1, random_names = TRUE, seed = 123)
#' describe(dat)
#'
#' ## And now have a look at poisson-distributed data
#' design <- design(
#'   n = 3, B_start = c(6, 10, 14), mt = c(12, 20, 22), start_value = 10,
#'   distribution = "poisson", level = -5, missing_prop = 0.1
#' )
#' dat <- random_scdf(design, seed = 1234)
#' pand(dat, decreasing = TRUE)

random_scdf <- function(design = NULL, 
                        round = NA, 
                        random_names = FALSE, 
                        seed = NULL, 
                        ...) {
  
  if (!is.null(seed)) set.seed(seed)
  if (is.numeric(design)) {
    warning("The first argument is expected to be a design matrix created by ", 
            "design(). If you want to set n, please name the first ",
            "argument with n = ...")
    n <- design
    design <- NULL
  }
  if (is.null(design)) design <- design(...)

  n <- length(design$cases)

  out <- vector("list", n)
  
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
    missing_prop <- design$cases[[i]]$missing_prop
    extreme_prop <- design$cases[[i]]$extreme_prop
    extreme_low <- design$cases[[i]]$extreme_low
    extreme_high <- design$cases[[i]]$extreme_high
    
    if (design$distribution %in% c("normal", "gaussian")) {
      trend <- trend * s
      slope <- slope * s
      level <- level * s
      extreme_low <- extreme_low * s
      extreme_high <- extreme_high * s
    }
    
    start_values <- c(start_value, rep(0, mt - 1))
    trend_values <- c(0, rep(trend, mt - 1))
    slope_values <- c()
    level_values <- c()
    
    for (j in 1:length(length)) {
      slope_values <- c(slope_values, rep(slope[j], length[j]))
      level_values <- c(level_values, level[j], rep(0, length[j] - 1))
    }
    
    true_values <- start_values + trend_values + slope_values + level_values
    true_values <- cumsum(true_values)
    
    if (design$distribution %in% c("normal", "gaussian")) {
      error_values <- rnorm(mt, mean = 0, sd = error)
      measured_values <- true_values + error_values
    }

    if (design$distribution %in% c("poisson")) {
      true_values[true_values < 0] <- 0
      measured_values <- rpois(length(true_values), lambda = true_values)
    }

    if (design$distribution == "binomial") {
      true_values[true_values < 0] <- 0
      true_values[true_values > 1] <- 1
      measured_values <- rbinom(
          n = length(true_values), 
          size = design$n_trials,
          prob = true_values
      )
    }
    
    if (extreme_prop > 0) {
      .ids <- which(runif(mt) <= extreme_prop)
      .error <- runif(length(.ids), min = extreme_low, max = extreme_high)
      measured_values[.ids] <- measured_values[.ids] + .error
    }

    if (missing_prop > 0) {
      .ids <- sample(1:mt, missing_prop * mt)
      measured_values[.ids] <- NA
    }

    if (!is.na(round)) {
      measured_values <- round(measured_values, round)
    }

    if (design$distribution %in% c("binomial", "poisson")) {
      measured_values[measured_values < 0] <- 0
    }
    
    # fast df assignment
    df <- list(
      phase = rep(design$cases[[i]]$phase, length), 
      values = measured_values, 
      mt = 1:mt
    )
    
    if (design$distribution == "binomial") 
      df$trials <- rep(design$n_trials, mt)
    
    class(df) <- "data.frame"
    attr(df, "row.names") <- .set_row_names(length(df[[1]]))
  
    out[[i]] <- df
    #out[[i]] <- data.frame(
    ##  phase = rep(design$cases[[i]]$phase, length), 
    #  values = measured_values, 
    #  mt = 1:mt
    #)
  }

  if (random_names == "male") names(out) <- sample(case_names$male, n)
  if (random_names == "female") names(out) <- sample(case_names$female, n)
  if (random_names == "neutral") names(out) <- sample(case_names$neutral, n)
  if (isTRUE(random_names)) names(out) <- sample(case_names$all, n)

  attributes(out) <- .default_attributes(attributes(out))
  out
}

#' @rdname deprecated-functions
#' @export
rSC <- function(...) {
  .deprecated_warning("random_scdf", "rSC")
  random_scdf(...)
}
