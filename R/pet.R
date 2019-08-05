#' Percent exceeding the trend
#' 
#' The \code{pet} function provides the percentage of phase B data points
#' exceeding the prediction based on the phase A trend. A binomial test against
#' a 50/50 distribution is computed. Furthermore, the percentage of phase B
#' data points exceeding the upper (or lower) 95 percent confidence interval of
#' the predicted progress is computed.
#' 
#' 
#' @inheritParams .inheritParams
#' @param ci Width of the confidence interval. Default is \code{ci = 0.95}.
#' @return \item{PET}{Percent exceeding the trend.} \item{PET.ci}{Percent
#' exceeding the upper / lower 95\%-CI boundary.} \item{p}{P value of Binomial
#' Test.} \item{ci.percent}{Width of confidence interval in percent.}
#' \item{se.factors}{Standard error.} \item{N}{Number of cases.}
#' \item{decreasing}{Logical argument from function call (see \code{Arguments}
#' above).} \item{case.names}{Assigned name of single-case.} \item{phases}{-}
#' @author Juergen Wilbert
#' @family overlap functions
#' @examples
#' 
#' ## Calculate the PET and use a 99%-CI for the additional calculation
#' # create random example data
#' design <- design_rSC(n = 5, slope = 0.2)
#' dat <- rSC(design, seed = 23)
#' pet(dat, ci = .99)
#' 
#' @export
pet <- function(data, dvar, pvar, mvar, ci = 0.95, decreasing = FALSE, phases = c("A","B")) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
  
  data <- .SCprepareData(data, na.rm = TRUE)
  data <- .keepphasesSC(data, phases = phases, pvar = pvar)$data
  
  N <- length(data)
  
  if (ci != 0) se.factor <- qnorm(ci) else se.factor <- 0 
  
  pet    <- rep(NA, N)
  pet.ci <- rep(NA, N)
  p      <- rep(NA, N)
  
  for(i in 1:N) {
    formula <- as.formula(paste0(dvar, "~", mvar))
    model <- lm(formula, data = data[[i]][data[[i]][, pvar] == "A",], na.action = na.omit)
    B <- data[[i]][data[[i]][, pvar] == "B",]
    res <- predict(model, B, se.fit = TRUE)
    nB <- nrow(B)
    if(!decreasing) {
      pet.ci[i] <- mean(B[, dvar] > (res$fit + res$se.fit * se.factor)) * 100
      pet[i]    <- mean(B[, dvar] > res$fit)*100
      p[i]      <- binom.test(sum(B[, dvar] > res$fit), nB, alternative = "greater")$p.value
    } else {
      pet.ci[i] <- mean(B[, dvar] < (res$fit - res$se.fit * se.factor)) * 100
      pet[i]    <- mean(B[, dvar] < res$fit) * 100
      p[i]      <- binom.test(sum(B[, dvar] < res$fit), nB, alternative = "greater")$p.value
    }
  }

  out <- list(
    PET = pet, PET.ci = pet.ci, p = p, ci.percent = ci * 100, 
    se.factors = se.factor, N = N, decreasing = decreasing, 
    case.names = .case.names(names(data), length(data))
  )
  class(out) <- c("sc","PET")
  out
}
