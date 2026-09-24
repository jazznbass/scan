#' Percent exceeding the trend (PET)
#'
#' Percentage of the phase B measurements that lie beyond the trend of phase A,
#' extrapolated into phase B.
#'
#' @details A linear regression of the dependent variable on the measurement
#'   time is fitted to phase A and predicted into phase B. PET is the percentage
#'   of phase B measurements above that prediction, or below it for
#'   `decreasing = TRUE`. `binom.p` tests that percentage against chance with a
#'   one sided binomial test at a probability of 0.5.
#'
#'   `PET CI` applies the same comparison to a stricter line: the prediction
#'   plus `qnorm(ci)` standard errors of the predicted mean, minus them for an
#'   expected decrease. The bound is one sided, so `ci = 0.95` shifts the line
#'   by 1.64 standard errors. With `ci = 0` the line is the prediction itself
#'   and `PET CI` equals `PET`.
#'
#'   Measurements without a value for the dependent variable or the measurement
#'   time are dropped. A case with fewer than two complete measurements in phase
#'   A, none in phase B, or without two distinct measurement times in phase A is
#'   not evaluated and gives `NA` with a warning. `PET CI` additionally needs
#'   three complete measurements in phase A; with fewer, PET is still computed
#'   and only `PET CI` stays `NA`.
#' @inheritParams .inheritParams
#' @param ci Width of the one sided confidence bound for the `PET CI` column.
#' @return An object of class `sc_pet` with the element `PET`, a data frame
#'   holding one row per case:
#'  |  |  |
#'  | --- | --- |
#'  | `Case` | Name of the case. |
#'  | `PET` | Percentage of phase B measurements beyond the phase A trend. |
#'  | `PET CI` | The same percentage against the confidence bound. |
#'  | `binom.p` | P value of the binomial test for `PET`. |
#' @author Juergen Wilbert
#' @family overlap functions
#' @examples
#' pet(exampleAB)
#'
#' # a stricter bound for the PET CI column
#' pet(exampleAB, ci = 0.99)
#'
#' # data that are expected to decrease in phase B
#' pet(exampleAB_decreasing, decreasing = TRUE)
#' @order 1
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
  
  data <- .prepare_scdf(data)
  data <- recombine_phases(data, phases = phases)$data
  
  N <- length(data)
  
  if (ci != 0) se_factor <- qnorm(ci) else se_factor <- 0 
  
  pet    <- rep(NA, N)
  pet_ci <- rep(NA, N)
  p      <- rep(NA, N)
  
  for(i in 1:N) {
    dat <- data[[i]]
    usable <- complete.cases(dat[, c(dvar, mvar), drop = FALSE])
    A <- dat[which(dat[[pvar]] == "A" & usable), , drop = FALSE]
    B <- dat[which(dat[[pvar]] == "B" & usable), , drop = FALSE]

    if (nrow(A) < 2L || nrow(B) < 1L) {
      warn(
        "Case ", i, ": need at least two complete observations ",
        "in phase A and one in phase B."
      )
      next
    }
    if (length(unique(A[[mvar]])) < 2L) {
      warn("Case ", i, ": need distinct measurement times in phase A.")
      next
    }

    calculate_ci <- nrow(A) >= 3L
    if (!calculate_ci) {
      warn(
        "Case ", i, ": need at least three complete observations ",
        "in phase A for PET CI. Ordinary PET is still calculated."
      )
    }

    formula <- as.formula(paste0(dvar, "~", mvar))
    model <- lm(
      formula, 
      data = A,
      na.action = na.omit
    )
    res <- predict(model, B, se.fit = calculate_ci)
    if (!calculate_ci) res <- list(fit = res)
    nB <- nrow(B)
    if(!decreasing) {
      if (calculate_ci) {
        pet_ci[i] <- mean(B[, dvar] > (res$fit + res$se.fit * se_factor)) * 100
      }
      pet[i]    <- mean(B[, dvar] > res$fit)*100
      p[i]      <- binom.test(
        sum(B[, dvar] > res$fit), nB, alternative = "greater"
      )$p.value
    } else {
      if (calculate_ci) {
        pet_ci[i] <- mean(B[, dvar] < (res$fit - res$se.fit * se_factor)) * 100
      }
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
  attributes(out)[opts("phase", "dv")] <- list(pvar, dvar)
  out
}
