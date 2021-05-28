#' Conservative Dual-Criterion Method
#' 
#' The \code{cdc} function applies the Conservative Dual-Criterion Method
#' (Fisher, Kelley, & Lomas, 2003) to scdf objects. It compares phase B
#' data points to both phase A mean and trend (OLS, bi-split, tri-split) with
#' an additional increase/decrease of .25 SD. A binomial test against a 50/50
#' distribution is computed and p-values below .05 are labeled "systematic
#' change".
#' 
#' 
#' @inheritParams .inheritParams
#' @param trend.method Method used to calculate the trend line. Default is \code{trend.method = "OLS"}.
#' Possible values are: \code{"OLS"}, \code{"bisplit"}, and \code{"trisplit"}.
#' \code{"bisplit"}, and \code{"trisplit"} should only be used for cases with at
#' least five data-points in both relevant phases.
#' @param conservative The CDC method adjusts the original mean and trend lines
#' by adding (expected increase) or subtracting (expected decrease) an
#' additional .25 SD before evaluating phase B data. Default is the CDC method
#' with \code{conservative = .25}. To apply the Dual-Criterion (DC) method, set
#' \code{conservative = 0}.
#' @return \item{cdc}{CDC Evaluation based on a p-value below .05.}
#' \item{cdc.exc}{Number of phase B datapoints indicating expected change.}
#' \item{cdc.nb}{Number of phase B datapoints.} \item{cdc.p}{P value of Binomial
#' Test.} \item{cdc.all}{Overall CDC Evaluation based on all instances/cases of
#' a Multiple Baseline Design.} \item{N}{Number of cases.} \item{decreasing}
#' {Logical argument from function call (see \code{Arguments} above).}
#' \item{conservative}{Numeric argument from function call (see \code{Arguments}
#' above).} \item{case.names}{Assigned name of single-case.} \item{phases}{-}
#' @author Timo Lueke
#' @references Fisher, W. W., Kelley, M. E., & Lomas, J. E. (2003). Visual Aids
#' and Structured Criteria for Improving Visual Inspection and Interpretation of
#' Single-Case Designs. \emph{Journal of Applied Behavior Analysis, 36}, 387-406.
#' https://doi.org/10.1901/jaba.2003.36-387
#' @examples
#' 
#' ## Apply the CDC method (standard OLS line)
#' design <- design_rSC(n = 1, slope = 0.2)
#' dat <- rSC(design, seed = 42)
#' cdc(dat)
#' 
#' ## Apply the CDC with Koenig's bi-split and an expected decrease in phase B.
#' cdc(exampleAB_decreasing, decreasing = TRUE, trend.method = "bisplit")
#' 
#' ## Apply the CDC with Tukey's tri-split, comparing the first and fourth phase.
#' cdc(exampleABAB, trend.method = "trisplit", phases = c(1,4))
#' 
#' ## Apply the Dual-Criterion (DC) method (i.e., mean and trend without shifting).
#' cdc(exampleAB_decreasing, decreasing = TRUE, trend.method = "bisplit", conservative = 0)
#'
#' 
#' @export
cdc <- function(data, dvar, pvar, mvar, decreasing = FALSE, trend.method = "OLS", conservative = .25, phases = c(1, 2)) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
  
  data  <- .SCprepareData(data, na.rm = TRUE)
  data  <- .keepphasesSC(data, phases = phases, pvar = pvar)$data
  
  N       <- length(data)
  cdc.na  <- rep(NA, N)  # total data points in phase A
  cdc.nb  <- rep(NA, N)  # total data points in phase B
  cdc.exc <- rep(NA, N)  # exceeding data points in phase B
  cdc     <- rep(NA, N)  # CDC rule evaluation of change
  cdc.p   <- rep(NA, N)  # binomial p (50/50)
  cdc.all <- NA          # CDC rule evaluation of all "cases"
  
    for(i in 1:N) {
    A         <- data[[i]][data[[i]][, pvar] == "A",]
    B         <- data[[i]][data[[i]][, pvar] == "B",]
    cdc.na[i] <- nrow(A)
    cdc.nb[i] <- nrow(B)

    if ((cdc.na[i] < 5 | cdc.nb[i] < 5) && trend.method != "OLS") {
      stop("The selected method for trend estimation should not be applied with less than five data points per phase.\n")
    }

    if(trend.method == "bisplit"){
      x     <- A[,mvar]
      y     <- A[,dvar]
      # na.rm = FALSE for now to prevent misuse; will draw no line if NA present
      md1   <- c((median(y[1:floor(length(y)/2)], na.rm = FALSE)),
                 median(x[1:floor(length(x)/2)], na.rm = FALSE))
      md2   <- c((median(y[ceiling(length(y)/2+1):length(y)], na.rm = FALSE)),
                 median(x[ceiling(length(x)/2+1):length(x)], na.rm = FALSE))
      md    <- as.data.frame(rbind(md1, md2))
      names(md) <- c(dvar,mvar)
      formula <- as.formula(paste0(dvar,"~",mvar))
      model <- lm(formula, data = md, na.action = na.omit)
    }
    
    if(trend.method == "trisplit"){
      x     <- A[,mvar]
      y     <- A[,dvar]
      # na.rm = FALSE for now to prevent misuse; will draw no line if NA present
      md1   <- c((median(y[1:floor(length(y)/3)], na.rm = FALSE)),
                 median(x[1:floor(length(x)/3)], na.rm = FALSE))
      md2   <- c((median(y[ceiling(length(y)/3*2+1):length(y)], na.rm = FALSE)),
                 median(x[ceiling(length(x)/3*2+1):length(x)], na.rm = FALSE))
      md    <- as.data.frame(rbind(md1, md2))
      names(md) <- c(dvar,mvar)
      formula <- as.formula(paste0(dvar,"~",mvar))
      model <- lm(formula, data = md, na.action = na.omit)
    }
    
    if(trend.method == "OLS"){
      formula <- as.formula(paste0(dvar, "~", mvar))
      model     <- lm(formula, data = A, na.action = na.omit)
    }
    
    trnd      <- predict(model, B, se.fit = TRUE)
    
    if(!decreasing) {
      cdc.exc[i] <- sum(B[, dvar] > trnd$fit + (conservative*sd(A[, dvar])) &
                         B[, dvar] > (mean(A[, dvar]) + (conservative*sd(A[, dvar]))))
      cdc.p[i]  <- binom.test(cdc.exc[i], cdc.nb[i], alternative = "greater")$p.value
      cdc[i]    <- if(cdc.p[i] < .05) {"systematic change"} else {"no change"}
    } else {
      cdc.exc[i] <- sum(B[, dvar] < trnd$fit - (conservative*sd(A[, dvar])) &
                         B[, dvar] < (mean(A[, dvar]) - (conservative*sd(A[, dvar]))))
      cdc.p[i]  <- binom.test(cdc.exc[i], cdc.nb[i], alternative = "greater")$p.value
      cdc[i]    <- if(cdc.p[i] < .05) {"systematic change"} else {"no change"}
    }
    cdc.all <- if(all(cdc == "systematic change", na.rm = TRUE)) {"systematic change"} else {"no change"}
  }

  out <- list(
    cdc = cdc,
    cdc.be = cdc.exc,
    cdc.b = cdc.nb,
    cdc.p = cdc.p,
    cdc.all = cdc.all,
    N = N,
    decreasing = decreasing,
    conservative = conservative,
    case.names = .case.names(names(data), length(data))
  )
  class(out) <- c("sc","CDC")
  out
}
