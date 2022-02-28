
.prepare_scdf <- function(data, na.rm = FALSE) {
  
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
    
    if ("tbl_df" %in% class(data[[case]])) {
      class(data[[case]]) <- "data.frame"
      message("Found tibble within scdf and changed it to data.frame.")
    }
    
    vars <- names(data[[case]])
    if (na.rm) data[[case]] <- data[[case]][!is.na(data[[case]][[dvar]]), ]
    if (!is.factor(data[[case]][[pvar]])) {
      data[[case]][[pvar]] <- as.factor(data[[case]][[pvar]])
    }
  }
  data
}
