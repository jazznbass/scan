#' Estimate single-case design
#'
#' This functions takes an scdf an extracts design parameters. The resulting in
#' object can be unsed to randomly create new scdf files with the same
#' underlying parameters. This might be usefull for monte-carlo studies and
#' bootstrapping procedures.
#'
#' @inheritParams .inheritParams
#' @param s The standard deviation depcting the between case variance of the overall performance. If more than two single-cases are included in the scdf, the variance is estimated if s is set to NULL.
#' @param rtt The reliability of the measurements. The reliability is estimated when rtt = NULL.
#' @param ... Further arguments passed to the lm function.
#'
#' @return A list of parameters for each single-case. Parameters include name, length, and starting measurementtime of each phase, trend level, and slope effects for each phase, mean, standarddeviation, and reliability for each case.
#' @export
#'
#' @examples
#' estimateSC(exampleABC)
#' @export

estimateSC <- function(data, dvar, pvar, mvar, s = NULL, rtt = NULL, model = "JW", ...) {
  
  warning(.opt$function_deprecated_warning)
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
  
  data <- .SCprepareData(data)
  cases <- length(data)
  case.names <- names(data)
  if (is.null(case.names)) case.names <- paste0("Case", 1:cases)
  
  phases <- lapply(
    data, function(case) as.data.frame(.phasestructure(case, pvar))
  )
  
  B.start <- unlist(lapply(phases, function(x) x$start[2]))
  MT      <- unlist(lapply(phases, function(x) sum(x$length)))
  
  level <- c()
  slope <- c()
  trend <- c()
  m <- c()
  rtt <- c()
  error <- c()
  fitted <- c()
  
  for(i in 1:cases) {
    plm.model <- plm(data[i], model = model, ...)$full
    res <- coef(plm.model)
    n.phases <- nrow(phases[[i]])
    phases[[i]]$m <- res[1]
    phases[[i]]$level <- c(res[1], res[3:(1+n.phases)])
    phases[[i]]$slope <- c(res[2], res[(2+n.phases):(2+2*(n.phases-1))])
    phases[[i]]$error <- var(plm.model$residual)
    phases[[i]]$true <- var(plm.model$fitted.values)
    phases[[i]]$rtt <- var(plm.model$fitted.values)/(var(plm.model$fitted.values)+var(plm.model$residuals))
    m <- c(m, res[1])
    trend <- c(trend,res[2])
    level <- c(level,res[3])
    slope <- c(slope,res[4])
    error <- c(error, var(plm.model$residuals))
    fitted <- c(fitted, var(plm.model$fitted.values))
    
  }
  
  if(cases > 2 && is.null(s))
    s <- sd(m, na.rm = TRUE)
  
  for(i in 1:cases) phases[[i]]$s <- s
  
  if(is.null(rtt)) rtt <- 1-(error/s^2)
  
  rtt.total <- sum(fitted) / (sum(fitted) + sum(error))
  level <- level / s
  slope <- slope / s
  trend <- trend / s
  
  for(i in 1:cases) {
    phases[[i]]$level <- phases[[i]]$level/s
    phases[[i]]$slope <- phases[[i]]$slope/s
  }
  
  out <- list(
    N = cases, case.names = case.names, MT = MT, B.start = B.start, 
    m = m, s = s, level = level, slope = slope, trend = trend, 
    rtt = rtt, design = phases, rtt.total = rtt.total
  )
  #class(out) <- c("sc","parameters")
  out
}


