
.prepare_scdf <- function(data, 
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
  
  names(data) <- .case_names(names(data), length(data))
  
  for(case in 1:length(data)) {
    vars <- names(data[[case]])
    if (na.rm) data[[case]] <- data[[case]][!is.na(data[[case]][, dvar]), ]
    if (!is.factor(data[[case]][, pvar])) {
      data[[case]][, pvar] <- as.factor(data[[case]][, pvar])
    }
    
    #if (is.na(names(data)[case])) names(data)[case] <- paste0("Case ", case)
  }
  data
}
