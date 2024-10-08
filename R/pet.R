#' Percent exceeding the trend
#'
#' The `pet` function returns the percentage of Phase B data points that exceed
#' the prediction based on the Phase A trend. A binomial test against a 50/50
#' distribution is calculated. It also calculates the percentage of Phase B data
#' points that exceed the upper (or lower) 95 percent confidence interval of the
#' predicted progression.
#'
#' @inheritParams .inheritParams
#' @param ci Width of the confidence interval. Default is `ci = 0.95`.
#' @return 
#'  |  |  |
#'  | --- | --- |
#'  | `PET` | Percent exceeding the trend. |
#'  | `ci` | Width of confidence interval. |
#'  | `decreasing` | Logical argument from function call (see Arguments above). | 
#' @author Juergen Wilbert
#' @family overlap functions
#' @examples
#'
#' ## Calculate the PET and use a 99%-CI for the additional calculation
#' # create random example data
#' design <- design(n = 5, slope = 0.2)
#' dat <- random_scdf(design, seed = 23)
#' pet(dat, ci = .99)
#'
#' @export
pet <- function(data, 
                dvar, pvar, mvar, 
                ci = 0.95, 
                decreasing = FALSE, 
                phases = c(1, 2)) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar
  
  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data
  
  N <- length(data)
  
  if (ci != 0) se_factor <- qnorm(ci) else se_factor <- 0 
  
  pet    <- rep(NA, N)
  pet_ci <- rep(NA, N)
  p      <- rep(NA, N)
  
  for(i in 1:N) {
    formula <- as.formula(paste0(dvar, "~", mvar))
    model <- lm(
      formula, 
      data = data[[i]][data[[i]][, pvar] == "A",], 
      na.action = na.omit
    )
    B <- data[[i]][data[[i]][, pvar] == "B",]
    res <- predict(model, B, se.fit = TRUE)
    nB <- nrow(B)
    if(!decreasing) {
      pet_ci[i] <- mean(B[, dvar] > (res$fit + res$se.fit * se_factor)) * 100
      pet[i]    <- mean(B[, dvar] > res$fit)*100
      p[i]      <- binom.test(
        sum(B[, dvar] > res$fit), nB, alternative = "greater"
      )$p.value
    } else {
      pet_ci[i] <- mean(B[, dvar] < (res$fit - res$se.fit * se_factor)) * 100
      pet[i] <- mean(B[, dvar] < res$fit) * 100
      p[i] <- binom.test(
        sum(B[, dvar] < res$fit), nB, alternative = "greater"
      )$p.value
    }
  }

  pet <- data.frame(
    Case = revise_names(data),
    PET = pet, 
    "PET CI" = pet_ci, 
    binom.p = p, 
    check.names = FALSE
  )
  
  out <- list(
    PET = pet,
    ci = ci,
    decreasing = decreasing
  )
  class(out) <- c("sc_pet")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  out
}
