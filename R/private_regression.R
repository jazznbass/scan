
.create_fixed_formula <- function(dvar, mvar, 
                                  slope, level, trend, 
                                  var_phase, var_inter) {
  inter <- ""
  phase <- ""
  mt    <- ""
  if (slope) {
    inter <- paste0(var_inter, collapse = "+")
    inter <- paste0("+ ", inter)
  }
  if (level) {
    phase <- paste0(var_phase, collapse = "+")
    phase <- paste0("+ ", phase)
  }
  if (trend) mt <- paste0("+ ", mvar, " ")
  paste0(dvar, " ~ 1", mt, phase, inter)
}

.create_random_formula <- function(mvar, 
                                   slope, level, trend, 
                                   var_phase, var_inter) {
  inter <- ""
  phase <- ""
  mt    <- ""
  if (slope) {
    inter <- paste0(var_inter, collapse = "+")
    inter <- paste0("+ ", inter)
  }
  if (level) {
    phase <- paste0(var_phase, collapse = "+")
    phase <- paste0("+ ", phase)
  }
  if (trend)
    mt <- paste0("+ ", mvar, " ")
  paste0("~ 1", mt, phase, inter, "|case")
}



.plm.row.names <- function(rn, x) {
  out <- rn
  if (!is.na(match("mt", rn)))
    out[match("mt", rn)] <- "Trend"
  if (!is.na(match(attr(x, .opt$mt), rn)))
    out[match(attr(x, .opt$mt), rn)] <- paste0("Trend ", attr(x, .opt$mt))
  if (!is.na(match("(Intercept)", rn)))
    out[match("(Intercept)", rn)] <- "Intercept"
  
  phase <- attr(x, .opt$phase)
  out <- gsub(  phase, paste0("Level ", phase," "), out)
  out <- gsub("inter", paste0("Slope ", phase," "), out)
}



.plm.mt <- function(data, 
                    type = "level p", 
                    model = "B&L-B", 
                    dvar = "values", pvar = "phase", mvar = "mt", 
                    count.data = FALSE) {
  n <- length(data)
  if (n > 1) {
    stop("Multiple single-cases are given. Calculations could only be applied ",
         "to a single data set.\n")
  }
  
  if (inherits(data, "list")) data <- data[[1]]
  if (ncol(data) < 3) data[, mvar] <- 1:nrow(data)
  
  y  <- data[, dvar]
  n1 <- sum(data[, pvar] == "A")
  n2 <- sum(data[, pvar] == "B")
  mt <- data[, mvar]
  D  <- c(rep(0, n1), rep(1, n2))
  
  if (model == "H-M") {
    inter <- (mt - mt[n1 + 1]) * D	
  } else if (model == "B&L-B") {
    inter <- (mt - mt[n1]) * D	
  } else if (model == "Mohr#1") {
    inter <- mt * D	
  } else if (model == "Mohr#2") {
    inter <- (mt - mt[n1 + 1]) * D
    mt <- mt - mt[n1 + 1]
  } else if (model == "Manly") {
    inter <- mt * D
  }	
  
  if (count.data) {
    full <- glm(I(round(y)) ~ 1 + mt + D + inter, family = "poisson")
  } else full <- lm(y ~ 1 + mt + D + inter)
  
  if (type == "1" || type == "level p")
    return(summary(full)$coef[3, 4])
  if (type == "2" || type == "slope p")
    return(summary(full)$coef[4, 4])
  if (type == "3" || type == "level t") 
    return(summary(full)$coef[3, 3])
  if (type == "4" || type == "slope t")
    return(summary(full)$coef[4, 3])
  if (type == "5" || type == "level B")
    return(summary(full)$coef[3, 1])
  if (type == "6" || type == "slope B")
    return(summary(full)$coef[4, 1])
  if (type == "model")
    return(full)
  
}
