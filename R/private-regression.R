
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

  str_mt    <- attr(x, opt("mt"))
  str_slope <- getOption("scan.string.dummy.slope")
  str_phase <- getOption("scan.string.dummy.phase")

  # Objects created by plm(), hplm(), mplm() and bplm() from version 0.63.3 on 
  # carry the names of
  # their dummy variables. Match these exactly. Older objects and objects from
  # other sources fall back to matching the dummy prefixes, which cannot tell a
  # covariate named e.g. 'intervention' from a slope dummy.
  dummy_phase <- attr(x, opt("dummy_phase"))
  dummy_slope <- attr(x, opt("dummy_slope"))

  id_phase <- if (!is.null(dummy_phase)) {
    which(rn %in% dummy_phase)
  } else {
    grep(paste0("^", str_phase, "\\w+$"), rn)
  }

  id_slope <- if (!is.null(dummy_slope)) {
    which(rn %in% dummy_slope)
  } else {
    grep(paste0("^", str_slope, "\\w+$"), rn)
  }

  .strip <- function(x, prefix) {
    ifelse(startsWith(x, prefix), substring(x, nchar(prefix) + 1), x)
  }
  name_phase <- .strip(rn[id_phase], str_phase)
  name_slope <- .strip(rn[id_slope], str_slope)
  dummy_phase_names <- rn[id_phase]
  dummy_slope_names <- rn[id_slope]

  if (style == "full") {
    rn[which(rn == str_mt)] <- paste0("Trend (", str_mt, ")")
    rn <- gsub("(Intercept)", "Intercept", rn, fixed = TRUE)
    rn[id_phase] <- paste0(
      "Level phase ", name_phase, " (", dummy_phase_names, ")"
    )
    rn[id_slope] <- paste0(
      "Slope phase ", name_slope, " (", dummy_slope_names, ")"
    )
  } else if (style == "concise") {
    rn[which(rn == str_mt)] <- "Trend"
    rn <- gsub("(Intercept)", "Intercept", rn, fixed = TRUE)
    rn[id_phase] <- paste0("Level ", name_phase)
    rn[id_slope] <- paste0("Slope ", name_slope)
  } else {
    abort("Ill defined scan.rename.predictors option. ",
          "Must be one of 'concise', 'full' or 'no'.")
  }

  rn
}



