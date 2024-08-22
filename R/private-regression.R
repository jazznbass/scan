
.create_fixed_formula <- function(dvar, mvar, 
                                  slope, level, trend, 
                                  var_phase, var_inter) {
  inter <- ""
  phase <- ""
  mt    <- ""
  browser()
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
  if (!is.na(match(attr(x, opt("mt")), rn)))
    out[match(attr(x, opt("mt")), rn)] <- paste0("Trend ", attr(x, opt("mt")))
  if (!is.na(match("(Intercept)", rn)))
    out[match("(Intercept)", rn)] <- "Intercept"
  
  phase <- attr(x, opt("phase"))
  out <- gsub("phase", paste0("Level ", phase," "), out)
  out <- gsub("inter", paste0("Slope ", phase," "), out)
}



