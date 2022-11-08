.default_attributes <- function(attri = NULL) {
  out <- list()
  
  if (!is.null(attri)) out <- attri
  
  out$class <- c("scdf", "list")
  
  scdf <- list()
  scdf[[.opt$phase]]   <- "phase"
  scdf[[.opt$dv]]      <- "values"
  scdf[[.opt$mt]]      <- "mt"
  out[[.opt$scdf]]     <- scdf
  
  out
}  

.deprecated_warning <- function(new, old) {
  if (isTRUE(getOption("scan.deprecated.warning"))) {
    warning(
      .opt$function_deprecated_warning, 
      " Please name function '", new, "' instead of '", old, "'."
    )
  }   
} 


.moving_average <- function(x, xLag, FUN = mean) {
  for(i in (xLag + 1):(length(x) - xLag))
    x[i] <- FUN(x[(i - xLag):(i + xLag)], na.rm = TRUE)
  return(x)
}

.beta_weights <- function(model) {
  b <- model$coefficients[-1]
  sx <- apply(model$model[-1], 2, sd)
  sy <- apply(model$model[ 1], 2, sd)
  c(model$coefficients, b * sx / sy)
}

.phasestructure <- function(data, pvar) {
  phases <- rle(as.character(data[, pvar]))
  phases$start <- c(1, cumsum(phases$lengths) + 1)[1:length(phases$lengths)]
  phases$stop  <- cumsum(phases$lengths)
  class(phases) <- "list"
  phases
}

.phases_string <- function(A, B) {
  nomer_s = "phase "
  nomer_p = "phases "
  a_part <- 
    if (length(A) == 1)
      paste0(nomer_s, A, collapse = "")
  else 
    paste0( c(nomer_p, A[1], paste0(" + ",A[-1]) ), 
            collapse = "")
  
  b_part <- 
    if (length(B) == 1)
      paste0(nomer_s, B, collapse = "")
  else 
    paste0( c(nomer_p,B[1], paste0(" + ",B[-1])), 
            collapse = "")
  
  out <- paste0(c("Comparing ", a_part, " against ", b_part), collapse ="")
  out
}

.nice_p <- function(p, equal.sign = FALSE) {
  out <- rep(NA, length(p))
  for(i in 1:length(p)) {
    if (isTRUE(p[i] >= 0.05)) {
      out[i] <- substring(sprintf("%.2f", trunc(p[i] * 100) / 100), 2)
      if (equal.sign) out[i] <- paste0("= ", out[i])
    }
    if (isTRUE(p[i] == 1))    out[i] <- "1.00"    
    if (isTRUE(p[i] < 0.05))  out[i] <- "<.05"
    if (isTRUE(p[i] < 0.01))  out[i] <- "<.01"
    if (isTRUE(p[i] < 0.001)) out[i] <- "<.001"
  }
  out
} 

.case_names <- function(x, n) {
  if (is.null(x)) x <- paste0("Case", 1:n)
  nonames <- which(is.na(x))
  x[nonames] <- paste0("Case", nonames)
  x
}




.std_lm <- function(model) {
  
  coef <- coef(model)
  if (isTRUE(class(coef) == "numeric")) coef <- as.matrix(coef, ncol = 1)
  intercept <- attr(attr(model$model, "terms"), "intercept")
  .sd <- function(x) sqrt(
    sum((x - mean(x, na.rm = TRUE) * intercept)^2, na.rm = TRUE)
  )
  .sd_predictors <- apply(as.matrix(model.matrix(model)), 2, .sd)
  .sd_criteria <- apply(as.matrix(model.frame(model)[, 1]), 2, .sd)
  coef_std <- coef
  for(i in 1:ncol(coef)) {
    coef_std[, i] <- coef[, i] * .sd_predictors / .sd_criteria[i]
  }
  
  coef_std
}

