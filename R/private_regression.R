
.create_fixed_formula <- function(dvar, mvar, 
                                  slope, level, trend, 
                                  VAR_PHASE, VAR_INTER) {
  inter <- ""
  phase <- ""
  mt    <- ""
  if (slope) {
    inter <- paste0(VAR_INTER, collapse = "+")
    inter <- paste0("+ ", inter)
  }
  if (level) {
    phase <- paste0(VAR_PHASE, collapse = "+")
    phase <- paste0("+ ", phase)
  }
  if (trend) mt <- paste0("+ ", mvar, " ")
  paste0(dvar, " ~ 1", mt, phase, inter)
}

.create_random_formula <- function(mvar, 
                                   slope, level, trend, 
                                   VAR_PHASE, VAR_INTER) {
  inter <- ""
  phase <- ""
  mt    <- ""
  if (slope) {
    inter <- paste0(VAR_INTER, collapse = "+")
    inter <- paste0("+ ", inter)
  }
  if (level) {
    phase <- paste0(VAR_PHASE, collapse = "+")
    phase <- paste0("+ ", phase)
  }
  if (trend)
    mt <- paste0("+ ", mvar, " ")
  paste0("~ 1", mt, phase, inter, "|case")
}

.add_model_dummies <- function(data, model, 
                               dvar = scdf_attr(data, .opt$dv), 
                               pvar = scdf_attr(data, .opt$phase), 
                               mvar = scdf_attr(data, .opt$mt),
                               contrast = "first") {
  for(case in 1:length(data)) {
    dat_inter <- .plm.dummy(
      data[[case]], model = model, dvar = dvar, 
      pvar = pvar, mvar = mvar, contrast = contrast
    )
    data[[case]][, mvar] <- dat_inter$mt
    data[[case]] <- cbind(data[[case]], dat_inter[, -1])
    n_Var <- (ncol(dat_inter) - 1) / 2
    var_inter <- names(dat_inter)[(ncol(dat_inter) - n_Var + 1):ncol(dat_inter)]
    var_phase <- names(dat_inter)[2:(n_Var + 1)]
  }
  out <- list(data = data, VAR_INTER = var_inter, VAR_PHASE = var_phase)
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
  
  phase <- attr(x, .opt$phase)
  out <- gsub(  phase, paste0("Level ", phase," "), out)
  out <- gsub("inter", paste0("Slope ", phase," "), out)
}

.plm.dummy <- function(data, 
                       dvar = "values", 
                       pvar = "phase", 
                       mvar = "mt",
                       model,
                       contrast = "first") {
  
  
  if (model == "JW") {
    contrast <- "preceding"
    model <- "B&L-B"
  }
    
  mt <- data[[mvar]]
  n  <- nrow(data)
  
  if (model == "W") mt <- mt - mt[1]
  
  out    <- data.frame(mt = mt)
  design <- rle(as.character(data[[pvar]]))
  
  #dummy phases
  for(phase in 2:length(design$values)) {
    n_phase <- design$lengths[phase]
    start <- sum(design$lengths[1:(phase - 1)]) + 1
    
    if (contrast %in% c("preceding")) {
      end <- n
    } else {
      end <- start + n_phase - 1
    }
    
    dummy <- rep(0, n)
    dummy[start:end] <- 1
    
    out[, paste0(pvar, design$values[phase])] <- dummy
  } 

  #dummy slopes
  for(phase in 2:length(design$values)) {
    dummy <- rep(0, n)
    n_phase <- design$lengths[phase]
    start <- sum(design$lengths[1:(phase - 1)]) + 1
    
    if (contrast %in% c("preceding")) {
      end <- n
    } else {
      end <- start + n_phase - 1
    }
    
    if (model %in% c("B&L-B")) 
      dummy[start:end] <- mt[start:end] - mt[start - 1]
    
    if (model %in% c("H-M", "W"))
      dummy[start:end] <- mt[start:end] - mt[start]

    out[, paste0("inter",design$values[phase])] <- dummy
  }
  
  out
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
