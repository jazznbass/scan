
.create_fixed_formula <- function(dvar, mvar, 
                                  slope, level, trend, 
                                  var_phase, 
                                  var_inter,
                                  intercept = TRUE) {
  parameters <- c()
  
  if (intercept) parameters <- "1"
  if (trend)  parameters <- c(parameters, mvar)
  if (level) parameters <- c(parameters, var_phase)
  if (slope) parameters <- c(parameters, var_inter)

  out <- paste0(dvar, " ~ ", paste0(parameters, collapse = " + "))
  
  as.formula(out, env = parent.frame())
}

.create_random_formula <- function(mvar, 
                                   slope, 
                                   level, 
                                   trend, 
                                   var_phase, 
                                   var_inter,
                                   intercept = TRUE,
                                   syntax = "lm") {
  
  parameters <- c()
  
  if (intercept) parameters <- "1"
  if (trend) parameters <- c(parameters, mvar)
  if (level) parameters <- c(parameters, var_phase)
  if (slope) parameters <- c(parameters, var_inter)

  if (syntax == "lm") {
    out <- paste0("~ ", paste0(parameters, collapse = " + "), "|case")
  }
  
  if (syntax == "mcmc") {
    out <- paste0("~ us(", paste0(parameters, collapse = " + "), "):case")
  }
  
  as.formula(out, env = parent.frame())
}

rename_predictors <- function(rn, x) {
  
  style <- getOption("scan.rename.predictors")
  if (identical(style, FALSE)) return(rn)
  if (identical(style, "no")) return(rn)
  
  str_mt <- attr(x, opt("mt"))
  str_phase <- attr(x, opt("phase"))
  
  
  if (style == "full") {
    rn <- gsub(str_mt, paste0("Trend (", str_mt, ")"), rn, fixed = TRUE)
    rn <- gsub("(Intercept)", "Intercept", rn, fixed = TRUE)
    rn <- gsub("phase(\\w+)", "Level phase \\1 (phase\\1)", rn)
    rn <- gsub("inter(\\w+)", "Slope phase \\1 (inter\\1)", rn)
  } else if (style == "concise"){
    rn <- gsub(str_mt, "Trend", rn, fixed = TRUE)
    rn <- gsub("(Intercept)", "Intercept", rn, fixed = TRUE)
    rn <- gsub("phase(\\w+)", "Level \\1", rn)
    rn <- gsub("inter(\\w+)", "Slope \\1", rn)
  } else {
    stop("Ill defined scan.rename.predictors option.", 
         "Must be one of 'concise', 'full' or, 'no'.")
  }
  
  #out <- gsub(
  #  opt("phase_dummy"), paste0("Level ", str_phase, " "), 
  #  out, 
  #  fixed = TRUE
  #)
  #out <- gsub(
  #  opt("inter_dummy"), paste0("Slope ", str_phase, " "), 
  #  out, 
  #  fixed = TRUE
  #)
  rn
}



