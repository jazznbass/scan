.kendall <- function(x, y, 
                     tau_method = "b", 
                     continuity_correction = TRUE) {
  
  N <- length(x)
  
  if (N < 3) {
    warning("could not calculate p-values for tau. Less than two data points.")
  }
  
  if (all(x == x[1]) || all(x == x[2])) {
    warning("could not calculate tau. Variance is zero.")
  }
  
  .sort <- sort.list(x)
  x <- x[.sort]
  y <- y[.sort]
  C <- 0
  D <- 0
  
  for(i in 1:(N - 1)) {
    C <- C + sum(y[(i + 1):N] > y[i] & x[(i + 1):N] > x[i])
    D <- D + sum(y[(i + 1):N] < y[i] & x[(i + 1):N] > x[i])
  }
  
  S  <- C - D
  n0 <- N * (N - 1) / 2
  
  if (tau_method == "a") {
    Den <- n0
    sdS <- S / (3 * S / sqrt( N * (N - 1) * (2* N + 5) / 2 ))
  }
  
  if (tau_method == "b") {
    tie_x <- rle(x)$lengths
    tie_y <- rle(sort(y))$lengths
    
    ti <- sum(
      vapply(tie_x, FUN = function(x) x * (x - 1) / 2, FUN.VALUE = numeric(1))
    )
    ui <- sum(
      vapply(tie_y, FUN = function(x) x * (x - 1) / 2, FUN.VALUE = numeric(1))
    )
    
    Den <- sqrt( (n0 - ti) * (n0 - ui) )
    
    v0 <- N * (N - 1) * (2 * N + 5)
    vt <- sum(
      vapply(
        tie_x, function(x) (x * (x - 1)) * (2 * x + 5), FUN.VALUE = numeric(1)
      )
    )
    vu <- sum(
      vapply(
        tie_y, function(x) (x * (x - 1)) * (2 * x + 5), FUN.VALUE = numeric(1)
      )
    )
    v1 <- sum(vapply(tie_x, function(x) (x * (x - 1)), FUN.VALUE = numeric(1))) * 
      sum(vapply(tie_y, function(x) (x * (x - 1)), FUN.VALUE = numeric(1)))
    v2 <- sum(vapply(tie_x, function(x) (x * (x - 1)) * (x - 2), FUN.VALUE = numeric(1))) * 
      sum(vapply(tie_y, function(x) (x * (x - 1)) * (x - 2), FUN.VALUE = numeric(1)))
    
    varS <- (v0 - vt - vu) / 18 + 
      (v1 / (2 * N * (N - 1))) +  
      (v2 / (9 * N * (N - 1) * (N - 2)))
    
    sdS <- sqrt(varS)
  }
  
  list(
    D = Den,
    sdS = sdS,
    N = N
  )
  
}

.kendall_full <- function(x, y, 
                          tau_method = "b", 
                          continuity_correction = TRUE) {
  
  N <- length(x)
  
  if (N < 3) {
    warning("could not calculate p-values for tau. Less than two data points.")
  }
  
  if (all(x == x[1]) || all(x == x[2])) {
    warning("could not calculate tau. Variance is zero.")
  }
  
  .sort <- sort.list(x)
  x <- x[.sort]
  y <- y[.sort]
  C <- 0
  D <- 0
  
  for(i in 1:(N - 1)) {
    C <- C + sum(y[(i + 1):N] > y[i] & x[(i + 1):N] > x[i])
    D <- D + sum(y[(i + 1):N] < y[i] & x[(i + 1):N] > x[i])
  }
  
  tie_x <- rle(x)$lengths
  tie_y <- rle(sort(y))$lengths
  
  ti <- sum(vapply(tie_x, FUN = function(x) x * (x - 1) / 2, FUN.VALUE = numeric(1)))
  ui <- sum(vapply(tie_y, FUN = function(x) x * (x - 1) / 2, FUN.VALUE = numeric(1)))
  
  S  <- C - D
  n0 <- N * (N - 1) / 2
  
  if (tau_method == "a") {
    Den <- n0
    tau   <- S / Den
    se <- sqrt((2 * N + 5) / Den) / 3
    
    #out$varS <- (2 * (2 * N + 5)) / (9 * N * (N - 1))
    #out$sdS <- sqrt(out$varS)
    
    sdS <- S / (3 * S / sqrt( N * (N - 1) * (2* N + 5) / 2 ))
    varS <- sdS^2
    if (!continuity_correction) {
      z <- 3 * S / sqrt( N * (N - 1) * (2* N + 5) / 2 )
    }
    if (continuity_correction)  {
      z <- 3 * (sign(S) * (abs(S) - 1)) / sqrt( N * (N - 1) * (2* N + 5) / 2 ) 
    }
    
  }
  
  if (tau_method == "b") {
    
    Den <- sqrt( (n0 - ti) * (n0 - ui) )
    tau <- S / Den
    
    v0 <- N * (N - 1) * (2 * N + 5)
    vt <- sum(vapply(tie_x, function(x) (x * (x - 1)) * (2 * x + 5), FUN.VALUE = numeric(1)))
    vu <- sum(vapply(tie_y, function(x) (x * (x - 1)) * (2 * x + 5), FUN.VALUE = numeric(1)))
    v1 <- sum(vapply(tie_x, function(x) (x * (x - 1)), FUN.VALUE = numeric(1))) * 
      sum(vapply(tie_y, function(x) (x * (x - 1)), FUN.VALUE = numeric(1)))
    v2 <- sum(vapply(tie_x, function(x) (x * (x - 1)) * (x - 2), FUN.VALUE = numeric(1))) * 
      sum(vapply(tie_y, function(x) (x * (x - 1)) * (x - 2), FUN.VALUE = numeric(1)))
    
    varS <- (v0 - vt - vu) / 18 + 
      (v1 / (2 * N * (N - 1))) +  
      (v2 / (9 * N * (N - 1) * (N - 2)))
    
    sdS <- sqrt(varS)
    se  <- sdS / Den
    
    if (!continuity_correction) z <- S / sdS #out$tau.b / out$se
    if (continuity_correction)  z <- (sign(S) * (abs(S) - 1)) / sdS 
  }
  
  #out$tau   <- S / n0
  #out$tau.b <- S / sqrt( (n0 - ti) * (n0 - ui) )
  #out$D <- out$S / out$tau.b
  #out$varS_tau_a <- (2 * (2 * N + 5)) / (9 * N * (N - 1))
  #out$sdS_tau_a <- sqrt((2 * (2 * N + 5)) / (9 * N * (N - 1)))
  
  p <- pnorm(abs(z), lower.tail = FALSE) * 2
  
  if (is.infinite(z)) {
    p <- NA
    tau <- NA
  }
  
  list(
    N  = N,
    n0 = n0,
    ti = ti,
    ui = ui,
    nC = C,
    nD = D,
    S  = S,
    D = Den,
    tau = tau,
    varS = varS,
    sdS = sdS,
    se = se,
    z = z,
    p = p
  )
  
}
