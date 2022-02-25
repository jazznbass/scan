
.create_fixed_formula <- function(dvar, mvar, slope, level, trend, VAR_PHASE, VAR_INTER) {
  INTER <- ""
  PHASE <- ""
  MT    <- ""
  if (slope) {
    INTER <- paste0(VAR_INTER, collapse = "+")
    INTER <- paste0("+ ", INTER)
  }
  if (level) {
    PHASE <- paste0(VAR_PHASE, collapse = "+")
    PHASE <- paste0("+ ", PHASE)
  }
  if (trend) MT <- paste0("+ ", mvar, " ")
  paste0(dvar, " ~ 1", MT, PHASE, INTER)
}

.create_random_formula <- function(mvar, slope, level, trend, VAR_PHASE, VAR_INTER) {
  INTER <- ""
  PHASE <- ""
  MT    <- ""
  if (slope) {
    INTER <- paste0(VAR_INTER, collapse = "+")
    INTER <- paste0("+ ", INTER)
  }
  if (level) {
    PHASE <- paste0(VAR_PHASE, collapse = "+")
    PHASE <- paste0("+ ", PHASE)
  }
  if (trend)
    MT <- paste0("+ ", mvar, " ")
  paste0("~ 1", MT, PHASE, INTER, "|case")
}

.add_model_dummies <- function(data, model, dvar = scdf_attr(data, .opt$dv), pvar = scdf_attr(data, .opt$phase), mvar = scdf_attr(data, .opt$mt)) {
  for(case in 1:length(data)) {
    dat_inter    <- .plm.dummy(data[[case]], model = model, dvar = dvar, pvar = pvar, mvar = mvar)
    data[[case]][, mvar] <- dat_inter$mt
    data[[case]]         <- cbind(data[[case]], dat_inter[, -1])
    n_Var        <- (ncol(dat_inter) - 1) / 2
    VAR_INTER    <- names(dat_inter)[(ncol(dat_inter) - n_Var + 1):ncol(dat_inter)]
    VAR_PHASE    <- names(dat_inter)[2:(n_Var + 1)]
  }
  out <- list(data = data, VAR_INTER = VAR_INTER, VAR_PHASE = VAR_PHASE)
  out
}

.plm.row.names <- function(rn, x) {
  out <- rn
  if (!is.na(match("mt", rn)))
    out[match("mt", rn)] <- "Trend"
  if (!is.na(match(attr(x, .opt$mt), rn)))
    out[match(attr(x, .opt$mt), rn)] <- paste0("Trend ", attr(x, .opt$mt))
  if (!is.na(match("(Intercept)", rn)))
    out[match("(Intercept)", rn)] <- "Intercept"
  
  PHASE <- attr(x, .opt$phase)
  out <- gsub(  PHASE, paste0("Level ", PHASE," "), out)
  out <- gsub("inter", paste0("Slope ", PHASE," "), out)
}

.plm.dummy <- function(data, model, phase.dummy = TRUE, dvar = "values", pvar = "phase", mvar = "mt") {
  
  if (!model %in% c("H-M", "B&L-B", "JW", "JW2")) {
    stop("Model ", model," unknown.\n")
  }
  
  MT <- data[, mvar]
  D  <- data[, pvar]
  N  <- nrow(data)
  
  out    <- data.frame(mt = MT)
  design <- rle(as.character(data[, pvar]))
  
  #dummy phases
  if (phase.dummy) {
    for(phase in 2:length(design$values)) {
      length.phase <- design$lengths[phase]
      pre <- sum(design$lengths[1:(phase - 1)])
      dummy <- rep(0, N)
      
      if (model == "JW") {
        dummy[(pre + 1):N] <- 1
      } else {
        dummy[(pre + 1):(pre + length.phase)] <- 1
      }
      
      out[, paste0(pvar, design$values[phase])] <- dummy
    } 
  }
  
  
  for(phase in 2:length(design$values)) {
    inter <- rep(0, N)
    length.phase <- design$lengths[phase]
    pre <- sum(design$lengths[1:(phase - 1)])
    
    if (model == "B&L-B") { 
      inter[(pre +1):(pre + length.phase)] <- MT[(pre + 1):(pre + length.phase)] - MT[(pre)]
    } else if (model == "H-M") {
      inter[(pre +1):(pre + length.phase)] <- MT[(pre + 1):(pre + length.phase)] - MT[(pre + 1)]
    } else if (model == "JW" || model == "JW2") {
      inter[(pre +1):N] <- MT[(pre +1):N]- MT[(pre)]
    }
    
    out[, paste0("inter",design$values[phase])] <- inter
  }
  
  out
}

.plm.mt <- function(data, type = "level p", model = "B&L-B", dvar = "values", pvar = "phase", mvar = "mt", count.data = FALSE) {
  N <- length(data)
  if (N > 1) {
    stop("Multiple single-cases are given. Calculations could only be applied to a single data set.\n")
  }
  
  if ("list" %in% class(data)) data <- data[[1]]
  if (ncol(data) < 3) data[, mvar] <- 1:nrow(data)
  
  y  <- data[, dvar]
  n1 <- sum(data[, pvar] == "A")
  n2 <- sum(data[, pvar] == "B")
  MT <- data[, mvar]
  D  <- c(rep(0, n1), rep(1, n2))
  
  if (model == "H-M") {
    inter <- (MT - MT[n1 + 1]) * D	
  } else if (model == "B&L-B") {
    inter <- (MT - MT[n1]) * D	
  } else if (model == "Mohr#1") {
    inter <- MT * D	
  } else if (model == "Mohr#2") {
    inter <- (MT - MT[n1 + 1]) * D
    MT <- MT - MT[n1 + 1]
  } else if (model == "Manly") {
    inter <- MT * D
  }	
  
  if (count.data) {
    full <- glm(I(round(y)) ~ 1 + MT + D + inter, family = "poisson")
  } else full <- lm(y ~ 1 + MT + D + inter)
  
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
