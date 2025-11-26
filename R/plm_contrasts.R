#' Add Dummy Variables for Piecewise Linear Models
#' 
#' Adds dummy variables to an scdf for calculating piecewise linear models.
#' 
#' @inheritParams .inheritParams
#' @examples
#' add_dummy_variables(
#'  scdf = exampleABC, 
#'  model = "W", 
#'  contrast_level = "first", 
#'  contrast_slope = "first"
#' )
#' @export
add_dummy_variables <- function(scdf, 
                                model = c("W", "H-M", "B&L-B"), 
                                contrast_level = c("first", "preceding"),
                                contrast_slope = c("first", "preceding")) {
  
  check_args(
    by_call(model),
    by_call(contrast_level),
    by_call(contrast_slope)
  )
  
  out <- .add_dummy_variables(
    data = scdf, 
    model = model, 
    contrast_level = contrast_level, 
    contrast_slope = contrast_slope
  )
  out$data
}


.add_dummy_variables <- function(data, model, 
                                dvar = dv(data), 
                                pvar = phase(data), 
                                mvar = mt(data),
                                contrast_level,
                                contrast_slope) {
      

  for(case in 1:length(data)) {
    dat_inter <- .create_dummies(
      data[[case]], 
      model = model, 
      dvar = dvar, 
      pvar = pvar, 
      mvar = mvar, 
      contrast_level = contrast_level, 
      contrast_slope = contrast_slope
    )
    
    data[[case]][, mvar] <- dat_inter$mt
    data[[case]] <- cbind(data[[case]], dat_inter[, -1])
    n_Var <- (ncol(dat_inter) - 1) / 2
    var_inter <- names(dat_inter)[(ncol(dat_inter) - n_Var + 1):ncol(dat_inter)]
    var_phase <- names(dat_inter)[2:(n_Var + 1)]
  }
  out <- list(
    data = data, 
    var_inter = var_inter, 
    var_phase = var_phase
  )
  out
}

.create_dummies <- function(data, 
                            dvar, 
                            pvar, 
                            mvar,
                            model,
                            contrast_level,
                            contrast_slope) {

  mt <- data[[mvar]]
  n  <- nrow(data)
  
  if (model == "W") mt <- mt - mt[1]
  
  out <- data.frame(mt = mt)
  design <- rle(as.character(data[[pvar]]))
  
  # dummy phases
  
  if (identical(contrast_level, "first")) {
    contr <- contr.treatment(nlevels(data[[pvar]]))
  } else if (identical(contrast_level, "preceding")) {
    contr <- contr.cum(nlevels(data[[pvar]]))
  } else {
    stop("Wrong declaration of level contrast: ", contrast_level)
  }
  rownames(contr) <- levels(data[[pvar]])
  colnames(contr) <- levels(data[[pvar]])[-1]
  contrasts(data[[pvar]])<- contr 
  
  add <- .create_phase_dummies(data[[pvar]])
  out <- cbind(out, add)

  # dummy slopes
  
  if (identical(contrast_slope, "first")){
    contr <- contr.treatment(nlevels(data[[pvar]]))
  } else if (identical(contrast_slope, "preceding")) {
    contr <- contr.cum(nlevels(data[[pvar]]))
  } else {
    stop("Wrong declaration of slope contrast: ", contrast_slope)
  }
  
  rownames(contr) <- levels(data[[pvar]])
  colnames(contr) <- levels(data[[pvar]])[-1]
  
  add <- .create_slope_dummies(data[[pvar]], mt, contr, model)
  out <- cbind(out, add)
  
  out
}

contr.cum <- function(n) {
  out <- c() 
  for(i in 1:(n - 1)) out <- c(out, c(rep(0, i), rep(1, n - i)))
  matrix(out, ncol = n - 1)
}

.create_phase_dummies <- function(fac, 
                          contrast, 
                          var_name = getOption("scan.string.dummy.phase")) {
  
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
  attr(df, "formula") <- formula(paste(
      c(".", "~", paste(c(".", dummy_names), collapse = "+")), 
      collapse = " "
  ))
  df
}

.create_slope_dummies <- function(phase, 
                                  mt, 
                                  contrast, 
                                  model, 
                                  var_name = getOption("scan.string.dummy.slope")) {
  
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
      
      mt_dummy <- (mt[id] - mt[id[1]])
      if (model %in% c("B&L-B")) 
        df[[dummy_names[i]]][id] <- (mt_dummy + 1) * phase_str$values[j] #check: (mt[1:length(id)] - mt[1] + 1) * phase_str$values[j]
      
     
      if (model %in% c("H-M", "W"))
        df[[dummy_names[i]]][id] <- mt_dummy * phase_str$values[j] #check: (mt[1:length(id)] - mt[1]) * phase_str$values[j]
    }
  }
  
  df <- as.data.frame(df)
  attr(df, "formula") <- formula(paste(
    c(".", "~", paste(c(".",dummy_names), collapse = "+")), 
    collapse = " "
  ))
  df
}







