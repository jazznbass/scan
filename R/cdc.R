#' Conservative Dual-Criterion Method
#' 
#' The \code{cdc} function applies the Conservative Dual-Criterion Method
#' (Swoboda, Kratochwill, & Levin, 2010) to scdf objects. It compares phase B
#' data points to both phase A median and trend (OLS, bi-split, tri-split) with
#' an additional .25 SD. A binomial test against a 50/50 distribution is
#' computed and p-values below .05 are labeled "systematic change".
#' 
#' 
#' @inheritParams .inheritParams
#' @param trend.method Method used to calculate the trend line. Default is \code{trend.method = "OLS"}.
#' Possible values are: \code{"OLS"}, \code{"bisplit"}, and \code{"trisplit"}.
#' @return \item{cdc}{CDC Evaluation based on a p-value below .05.}
#' \item{cdc.exc}{Number of phase B datapoints indicating expected change.}
#' \item{cdc.nb}{Number of phase B datapoints.} \item{cdc.p}{P value of Binomial
#' Test.} \item{N}{Number of cases.} \item{decreasing}{Logical argument from
#' function call (see \code{Arguments} above).} \item{case.names}{Assigned name
#' of single-case.} \item{phases}{-}
#' @author Timo Lueke
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
#' 
#' @export
cdc <- function(data, dvar, pvar, mvar, decreasing = FALSE, trend.method = "OLS", phases = c(1, 2)) {
  
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
  #cdc.all <- NA  # CDC rule evaluation of overall change
  
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
      cdc.exc[i] <- sum(B[, dvar] > trnd$fit + (.25*sd(A[, dvar])) &
                         B[, dvar] > (mean(A[, dvar]) + (.25*sd(A[, dvar]))))
      cdc.p[i]  <- binom.test(cdc.exc[i], cdc.nb[i], alternative = "greater")$p.value
      cdc[i]    <- if(cdc.p[i] < .05) {"systematic change"} else {"no change"}
    } else {
      cdc.exc[i] <- sum(B[, dvar] < trnd$fit - (.25*sd(A[, dvar])) &
                         B[, dvar] < (mean(A[, dvar]) - (.25*sd(A[, dvar]))))
      cdc.p[i]  <- binom.test(cdc.exc[i], cdc.nb[i], alternative = "greater")$p.value
      cdc[i]    <- if(cdc.p[i] < .05) {"systematic change"} else {"no change"}
    }
    #cdc.all <- if(all(cdc == "systematic change", na.rm = TRUE)) {"systematic change"} else {"no change"}
  }

  out <- list(
    cdc = cdc,
    #cdc.all = cdc.all,
    cdc.be = cdc.exc,
    cdc.b = cdc.nb,
    cdc.p = cdc.p,
    N = N,
    decreasing = decreasing,
    case.names = .case.names(names(data), length(data))
  )
  class(out) <- c("sc","CDC")
  out
}
