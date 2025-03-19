
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
  str_slope <- getOption("scan.string.dummy.slope")
  str_phase <- getOption("scan.string.dummy.phase")
  
  if (style == "full") {
    rn[which(rn == str_mt)] <- paste0("Trend (", str_mt, ")")
    rn <- gsub("(Intercept)", "Intercept", rn, fixed = TRUE)
    rn <- gsub(
      paste0(str_phase, "(\\w+)"), 
      paste0("Level phase \\1 (", str_phase, "\\1)"), 
      rn
    )
    rn <- gsub(
      paste0(str_slope, "(\\w+)"), 
      paste0("Slope phase \\1 (", str_slope, "\\1)"), 
      rn
    )
  } else if (style == "concise"){
    rn[which(rn == str_mt)] <- "Trend"
    rn <- gsub("(Intercept)", "Intercept", rn, fixed = TRUE)
    rn <- gsub(paste0(str_phase, "(\\w+)"), "Level \\1", rn)
    rn <- gsub(paste0(str_slope, "(\\w+)"), "Slope \\1", rn)
  } else {
    stop("Ill defined scan.rename.predictors option.", 
         "Must be one of 'concise', 'full' or, 'no'.")
  }
  
  rn
}



