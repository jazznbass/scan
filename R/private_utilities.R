.defaultAttributesSCDF <- function(attri = NULL) {
  out <- list()
  
  if (!is.null(attri)) out <- attri
  
  out$class <- c("scdf","list")
  #out[.opt$phase] <- "phase"
  #out[.opt$dv]    <- "values"
  #out[.opt$mt]    <- "mt"
  
  scdf <- list()
  scdf[[.opt$phase]]   <- "phase"
  scdf[[.opt$dv]]      <- "values"
  scdf[[.opt$mt]]      <- "mt"
  out[[.opt$scdf]]     <- scdf
  
  
  out
}  

.SCmovingAverage <- function(x, xLag, FUN = mean) {
  for(i in (xLag + 1):(length(x) - xLag))
    x[i] <- FUN(x[(i - xLag):(i + xLag)], na.rm = TRUE)
  return(x)
}

.SCac <- function(x, lag = 1) {
  m <- mean(x, na.rm = TRUE)
  ax1 <- x[1:(length(x) - lag)] - m
  ax2 <- x[(lag + 1):length(x)] - m
  ac <- sum(ax1 * ax2, na.rm = TRUE) / sum((x - m)^2, na.rm = TRUE)
  ac
}

.SClm <- function(x = NULL, y) {
  if (is.null(x))
    x <- 1:length(y)
  mx <- mean(x)
  my <- mean(y)
  ss.xy <- sum( (x - mx) * (y - my) )
  ss.xx <- sum( (x - mx)^2 )
  b <- ss.xy / ss.xx
  b
}

.SCbeta <- function(model) {
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

.keepphasesSC <- function(data, phases = c(1, 2), set.phases = TRUE, pvar = "phase") {
  
  if (is.data.frame(data)) data <- list(data)
  ATTRIBUTES <- attributes(data)
  
  res <- lapply(data, function(x) rle(as.character(x[, pvar]))$values)
  if (!all(unlist(lapply(res[-1], function(x) identical(x, res[[1]])))))
    warning("Single-cases do have differing desings.")
  
  if (class(phases) %in% c("character","numeric","integer")) {
    if (!length(phases) == 2) {
      stop("Phases argument not set correctly. Please provide a vector with two charcters or two numbers. E.g., phases = c(1,3).")
    }    
    phases.A <- phases[1]
    phases.B <- phases[2]
  }
  
  if (class(phases) == "list") {
    phases.A <- phases[[1]]
    phases.B <- phases[[2]]
  }
  
  phases.total <- c(phases.A, phases.B)
  design <- rle(as.character(data[[1]][, pvar]))
  
  if (class(phases.total) == "character") {
    tmp <- sapply(phases.total, function(x) sum(x == design$values)>1)
    if (any(tmp))
      stop(paste0("Phase names ", paste0(names(tmp[tmp]))," occure several times. Please give number of phases instead of characters."))
    
    tmp <- sapply(phases.total, function(x) any(x == design$values))
    if (!all(tmp))
      stop(paste0("Phase names ",  names(tmp[!tmp]) ," do not occure in the data. Please give different phase names."))
  }
  
  if (class(phases.total) == "character") {
    phases.A <- which(design$values %in% phases.A)
    phases.B <- which(design$values %in% phases.B)
  }
  
  N <- length(data)
  design.list <- list()
  
  for(case in 1:N) {
    design <- rle(as.character(data[[case]][,pvar]))
    design$start <- c(1,cumsum(design$lengths)+1)[1:length(design$lengths)]
    design$stop <- cumsum(design$lengths)
    class(design) <- "list"
    
    A <- unlist(lapply(phases.A, function(x) design$start[x]:design$stop[x]))
    B <- unlist(lapply(phases.B, function(x) design$start[x]:design$stop[x]))
    
    data[[case]][,pvar] <- as.character(data[[case]][,pvar])
    
    if (set.phases) {
      data[[case]][A ,pvar] <- "A"
      data[[case]][B ,pvar] <- "B"
    }
    data[[case]] <- data[[case]][c(A,B),]
    design.list[[case]] <- design
  }
  attributes(data) <- ATTRIBUTES
  out <- list(data = data, designs = design.list, N = N, phases.A = phases.A, phases.B = phases.B)
  return(out)
}

.stringPhasesSC <- function(A,B) {
  nomerS = "phase "
  nomerP = "phases "
  APART <- 
    if (length(A) == 1)
      paste0(nomerS, A, collapse = "")
  else 
    paste0( c(nomerP, A[1], paste0(" + ",A[-1]) ), 
            collapse = "")
  
  BPART <- 
    if (length(B) == 1)
      paste0(nomerS, B, collapse = "")
  else 
    paste0( c(nomerP,B[1], paste0(" + ",B[-1])), 
            collapse = "")
  
  out <- paste0(c("Comparing ", APART, " against ", BPART), collapse ="")
  out
}

.nice.p <- function(p, equal.sign = FALSE) {
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

.case.names <- function(x, n) {
  if (is.null(x)) x <- paste0("Case", 1:n)
  nonames <- which(is.na(x))
  x[nonames] <- paste0("Case", nonames)
  x
}

.kendall <- function(x, y, continuity_correction = TRUE) {
  
  out <- list()
  dat <- data.frame(cbind(x, y))
  dat <- dat[order(dat$x), ]
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
  out$.C <- C
  out$.D <- D
  out$S  <- S
  out$tau   <- S / n0
  out$tau.b <- S / sqrt( (n0 - ti) * (n0 - ui) )
  out$D <- out$S / out$tau.b
  v0 <- N * (N - 1) * (2 * N + 5)
  vt <- sum(sapply(tie.x, function(x) (x * (x - 1)) * (2 * x + 5)))
  vu <- sum(sapply(tie.y, function(x) (x * (x - 1)) * (2 * x + 5)))
  v1 <- sum(sapply(tie.x, function(x) (x * (x - 1)))) * sum(sapply(tie.y, function(x) (x * (x - 1))))
  v2 <- sum(sapply(tie.x, function(x) (x * (x - 1)) * (x - 2))) * sum(sapply(tie.y, function(x) (x * (x - 1)) * (x - 2)))
  
  out$varS <- (v0 - vt - vu) / 18 + (v1 / (2 * N * (N - 1))) + 
    (v2 / (9 * N * (N - 1) * (N - 2)))
  
  out$sdS <- sqrt(out$varS)
  out$se  <- out$sdS / out$D
  
  if (!continuity_correction) out$z <- out$S / out$sdS #out$tau.b / out$se
  if (continuity_correction)  out$z <- (out$S + 1) / out$sdS 
  
  out$p <- pnorm(abs(out$z), lower.tail = FALSE) * 2
  
  if (is.infinite(out$z)) {
    out$p <- NA
    out$tau <- NA
  }
  
  
  out
}