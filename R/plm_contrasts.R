.add_model_dummies <- function(data, model, 
                          dvar = scdf_attr(data, .opt$dv), 
                          pvar = scdf_attr(data, .opt$phase), 
                          mvar = scdf_attr(data, .opt$mt),
                          contrast = c(level = "first", slope = "first")) {
  
  #if (length(contrast) == 1) contrast <- list(level = contrast, slope = contrast)
  
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
  out <- list(data = data, var_inter = var_inter, var_phase = var_phase)
  out
}

.plm.dummy <- function(data, 
                       dvar = "values", 
                       pvar = "phase", 
                       mvar = "mt",
                       model,
                       contrast = list(level = "first", slope = "first")) {
  
  
  if (inherits(contrast, "list") && is.null(names(contrast)))
    names(contrast) <- c("level", "slope")
  
  if (length(contrast) == 1 && inherits(contrast, "character")) 
    contrast <- list(level = contrast, slope = contrast)
  
  if (model == "JW") {
    contrast <- list(level = "preceding", slope = "preceding")
    model <- "B&L-B"
  }
  
  mt <- data[[mvar]]
  n  <- nrow(data)
  
  if (model == "W") mt <- mt - mt[1]
  
  out <- data.frame(mt = mt)
  design <- rle(as.character(data[[pvar]]))
  
  #dummy phases
  
  if (contrast$level == "first")
    contr <- contr.treatment(nlevels(data[[pvar]]))

  if (contrast$level == "preceding") 
    contr <- contr.cum(nlevels(data[[pvar]]))
  
  row.names(contr) <- levels(data[[pvar]])
  colnames(contr) <- levels(data[[pvar]])[-1]
  contrasts(data[[pvar]])<- contr 
  
  add <- .create_dummy(data[[pvar]])
  out <- cbind(out, add)

  #dummy slopes
  
  if (contrast$slope == "first")
    contr <- contr.treatment(nlevels(data[[pvar]]))
  
  if (contrast$slope == "preceding") 
    contr <- contr.cum(nlevels(data[[pvar]]))
  
  row.names(contr) <- levels(data[[pvar]])
  colnames(contr) <- levels(data[[pvar]])[-1]
  
  add <- .create_dummy_slopes(data[[pvar]], mt, contr, model)
  out <- cbind(out, add)
  
  out
}

contr.cum <- function(n) {
  out <- c() 
  for(i in 1:(n-1)) out <- c(out, c(rep(0, i), rep(1, n-i)))
  matrix(out, ncol = n-1)
}

.create_dummy <- function(fac, contrast, var_name = "phase") {
  
  if (missing(contrast)) contrast <- contrasts(fac)
  if(is.null(colnames(contrast))) {
    dummy_names <- paste0(var_name, 1:ncol(contrast))
  } else {
    dummy_names <- paste0(var_name, colnames(contrast))
  }
  
  vec_len <- length(fac)
  df <- list()
  
  for(i in 1:ncol(contrast)) {
    df[[dummy_names[i]]] <- numeric(vec_len)
    for(j in 1:nrow(contrast)) {
      id <- which(fac == levels(fac)[j])
      df[[dummy_names[i]]][id] <- contrast[j,i]
    }
  }
  
  df <- as.data.frame(df)
  attr(df, "formula") <- formula(paste(c(".", "~", paste(c(".",dummy_names), collapse = "+")), collapse = " "))
  df
}

.create_dummy_slopes <- function(phase, mt, contrast, model, var_name = "inter") {
  
  if(is.null(colnames(contrast))) {
    dummy_names <- paste0(var_name, 1:ncol(contrast))
  } else {
    dummy_names <- paste0(var_name, colnames(contrast))
  }
  
  df <- list()
  
  for(i in 1:ncol(contrast)) {
    df[[dummy_names[i]]] <- 0
    phase_str <- rle(contrast[,i])
    class(phase_str) <- "list"
    phase_str$start <- c(1, cumsum(phase_str$lengths) + 1)[1:length(phase_str$lengths)]
    phase_str$stop  <- cumsum(phase_str$lengths)
    
    for(j in 1:length(phase_str$lengths)) {
      selection_phases <- levels(phase)[phase_str$start[j]:phase_str$stop[j]]
      id <- which(phase %in% selection_phases)
      
      if (model %in% c("B&L-B")) 
        df[[dummy_names[i]]][id] <- (mt[1:length(id)] - mt[1] + 1) * phase_str$values[j]
      
      #  df[[dummy_names[i]]][id] <- mt[start:end] - mt[start - 1]
      
        if (model %in% c("H-M", "W"))
        df[[dummy_names[i]]][id] <- (mt[1:length(id)] - mt[1]) * phase_str$values[j]
      
      #  df[[dummy_names[i]]][id] <- mt[start:end] - mt[start]
      
      
      
    }
  }
  
  df <- as.data.frame(df)
  attr(df, "formula") <- formula(paste(c(".", "~", paste(c(".",dummy_names), collapse = "+")), collapse = " "))
  df
  
}







