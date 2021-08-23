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

# .SCac <- function(x, lag = 1) {
#   m <- mean(x, na.rm = TRUE)
#   ax1 <- x[1:(length(x) - lag)] - m
#   ax2 <- x[(lag + 1):length(x)] - m
#   ac <- sum(ax1 * ax2, na.rm = TRUE) / sum((x - m)^2, na.rm = TRUE)
#   ac
# }

# .SClm <- function(x = NULL, y) {
#   if (is.null(x))
#     x <- 1:length(y)
#   mx <- mean(x)
#   my <- mean(y)
#   ss.xy <- sum( (x - mx) * (y - my) )
#   ss.xx <- sum( (x - mx)^2 )
#   b <- ss.xy / ss.xx
#   b
# }

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

.kendall <- function(x, y, tau_method = "b", continuity_correction = TRUE) {
  
  out <- list()
  dat <- data.frame(cbind(x, y))
  dat <- dat[sort.list(dat$x), ]
  C <- 0
  D <- 0
  N <- nrow(dat)
  for(i in 1:(N - 1)) {
    C <- C + sum(dat$y[(i + 1):N] > dat$y[i] & dat$x[(i + 1):N] > dat$x[i])
    D <- D + sum(dat$y[(i + 1):N] < dat$y[i] & dat$x[(i + 1):N] > dat$x[i])
  }
  
  tie.x <- rle(sort(x))$lengths
  tie.y <- rle(sort(y))$lengths
  
  ti <- sum(sapply(tie.x, function(x) (x * (x - 1) / 2)))
  ui <- sum(sapply(tie.y, function(x) (x * (x - 1) / 2)))
  
  S  <- C - D
  n0 <- N * (N - 1) / 2
  out$N  <- N
  out$n0 <- n0
  out$ti <- ti
  out$ui <- ui
  out$nC <- C
  out$nD <- D
  out$S  <- S
  
  if (tau_method == "a") {
    out$D <- n0
    out$tau   <- S / out$D
    out$se <- sqrt((2 * N + 5) / choose(N, 2)) / 3
    
    #out$varS <- (2 * (2 * N + 5)) / (9 * N * (N - 1))
    #out$sdS <- sqrt(out$varS)
    
    out$sdS <- out$S / (3 * out$S / sqrt( N * (N - 1) * (2* N + 5) / 2 ))
    out$varS <- out$sdS^2
    if (!continuity_correction) out$z <- 3 * out$S / sqrt( N * (N - 1) * (2* N + 5) / 2 )
    if (continuity_correction)  out$z <- 3 * (out$S - 1) / sqrt( N * (N - 1) * (2* N + 5) / 2 ) 
    
  }
  
  if (tau_method == "b") {
    
    out$D <- sqrt( (n0 - ti) * (n0 - ui) )
    out$tau <- S / out$D
    
    v0 <- N * (N - 1) * (2 * N + 5)
    vt <- sum(sapply(tie.x, function(x) (x * (x - 1)) * (2 * x + 5)))
    vu <- sum(sapply(tie.y, function(x) (x * (x - 1)) * (2 * x + 5)))
    v1 <- sum(sapply(tie.x, function(x) (x * (x - 1)))) * 
          sum(sapply(tie.y, function(x) (x * (x - 1))))
    v2 <- sum(sapply(tie.x, function(x) (x * (x - 1)) * (x - 2))) * 
          sum(sapply(tie.y, function(x) (x * (x - 1)) * (x - 2)))
    
    out$varS <- (v0 - vt - vu) / 18 + 
                (v1 / (2 * N * (N - 1))) +  
                (v2 / (9 * N * (N - 1) * (N - 2)))
    
    out$sdS <- sqrt(out$varS)
    out$se  <- out$sdS / out$D
    
    if (!continuity_correction) out$z <- out$S / out$sdS #out$tau.b / out$se
    if (continuity_correction)  out$z <- (out$S - 1) / out$sdS 
  }
  
  #out$tau   <- S / n0
  #out$tau.b <- S / sqrt( (n0 - ti) * (n0 - ui) )
  #out$D <- out$S / out$tau.b
  #out$varS_tau_a <- (2 * (2 * N + 5)) / (9 * N * (N - 1))
  #out$sdS_tau_a <- sqrt((2 * (2 * N + 5)) / (9 * N * (N - 1)))
  
  out$p <- pnorm(abs(out$z), lower.tail = FALSE) * 2
  
  if (is.infinite(out$z)) {
    out$p <- NA
    out$tau <- NA
  }

  out
}

.std_lm <- function(model) {
  
  coef <- coef(model)
  if (isTRUE(class(coef) == "numeric")) coef <- as.matrix(coef, ncol = 1)
  intercept <- attr(attr(model$model, "terms"), "intercept")
  .sd <- function(x) sqrt(sum((x - mean(x, na.rm = TRUE) * intercept)^2, na.rm = TRUE))
  .sd_predictors <- apply(as.matrix(model.matrix(model)), 2, .sd)
  .sd_criteria <- apply(as.matrix(model.frame(model)[, 1]), 2, .sd)
  coef_std <- coef
  for(i in 1:ncol(coef)) {
    coef_std[, i] <- coef[, i] * .sd_predictors / .sd_criteria[i]
  }
  
  coef_std
}
