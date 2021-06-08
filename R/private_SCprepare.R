.SCprepareData <- function(data, 
                           na.rm = FALSE 
                           #change.var.phase = FALSE, 
                           #change.var.values = FALSE, 
                           #change.var.mt = FALSE
                           ) {
  
  
  # check class scdf validity
  if (.opt$rigorous_class_check) {
    results <- .check_scdf(data)
    if (!isTRUE(results)) {
      if(length(results$warnings) > 0) {
        warning(results$warnings)
      }
      if(length(results$errors) > 0) {
        stop(results$errors)
      }
    } 
  }

  pvar <- scdf_attr(data, .opt$phase)
  mvar <- scdf_attr(data, .opt$mt)
  dvar <- scdf_attr(data, .opt$dv)
  
  names(data) <- .case.names(names(data), length(data))
  
  for(case in 1:length(data)) {
    vars <- names(data[[case]])
    if (na.rm) data[[case]] <- data[[case]][!is.na(data[[case]][, dvar]), ]
    if (!is.factor(data[[case]][, pvar])) data[[case]][, pvar] <- as.factor(data[[case]][, pvar])
    
    #if (change.var.values && dvar != "values") {
    #  if ("values" %in% vars) {
    #    warning("Original values variable was renamed to values_renamed for this analysis.")
     #   names(data[[case]])[match("values", vars)] <- "values_renamed"
     # }
    #  names(data[[case]])[match(dvar, vars)] <- "values"
    #}
    
    #if (change.var.mt && !(mvar %in% vars)) {
    #  data[[case]][, mvar] <- 1:nrow(data[[case]])
    #}
    
    #if (change.var.mt && mvar != "mt") {
    #  if ("mt" %in% vars) {
    #    warning("Original mt variable was renamed to mt_renamed for this analysis.")
    #    names(data[[case]])[match("mt", vars)] <- "mt_renamed"
    #  }
    #  names(data[[case]])[match(mvar, vars)] <- "mt"
    #}
    
    #if (change.var.phase && pvar != "phase") {
    #  if ("phase" %in% vars) {
    #    warning("Original phase variable was renamed to phase_renamed for this analysis.")
    #    names(data[[case]])[match("phase", vars)] <- "phase_renamed"
    #  }
    #  names(data[[case]])[match(pvar, vars)] <- "phase"
    #}
    
    if (is.na(names(data)[case]))
      names(data)[case] <- paste0("Case ", case)
  }
  data
}


