#' Scaling values of an scdf file
#'
#' This function scales the measured values of an scdf file. It allows for mean
#' centering and standardization based on each s?ingle-case data set or a
#' scaling across all cases included in an scdf.
#'
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#'   this format.
#' @param var A character string or a vector of character strings with variable
#'   names that should be scaled.
#' @param center If set TRUE, data are mean centered.
#' @param scale If set TRUE, the standard deviation is set.
#' @param m The target mean for centering.
#' @param sd The target standard deviation for scaling
#' @param grand If set TRUE, scaling is based on the mean and standarddeviation
#'   of all measurements across all single-cases within the scdf.
#' @return An scdf with the scaled values.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @examples
#' 
#' ## Standardize a multiple case scdf and compute an hplm
#' ex_sc <- scaleSC(exampleAB_50, var = "values", center = TRUE, scale = TRUE)
#' hplm(ex_sc)
#' 
#' @export

scaleSC <- function(data, var, center = TRUE, scale = FALSE, m = 0, sd = 1, grand = TRUE) {

  data <- .SCprepareData(data)
  
  N    <- length(data)
  
  if(grand) {
    means <- c()
    sds   <- c()
    for(i in 1:length(var)) {
      means <- c(means, mean(unlist(lapply(data, function(x) x[, var[i]])), na.rm = TRUE))
      sds   <- c(sds,     sd(unlist(lapply(data, function(x) x[, var[i]])), na.rm = TRUE))
    }
    
    for(case in 1:N) {
      for(i in 1:length(var)) {
        if(center && scale)
          data[[case]][,var[i]] <- (data[[case]][, var[i]] - means[i]) / sds[i] * sd + m
        if(center && !scale)
          data[[case]][,var[i]] <- (data[[case]][, var[i]] - means[i]) + m
        if(!center && scale)
          data[[case]][,var[i]] <- ((data[[case]][, var[i]] - means[i]) / sds[i] * sd) + means[i]
      }
    }
  }
  
  if(!grand) {
    for(case in 1:N) {
      for(i in 1:length(var)) {
        mCase  <- mean(data[[case]][, var[i]], na.rm = TRUE)
        sdCase <-   sd(data[[case]][, var[i]], na.rm = TRUE)
        if(center && scale)
          data[[case]][, var[i]] <- (data[[case]][, var[i]] - mCase) / sdCase[i] * sd + m
        if(center && !scale)
          data[[case]][, var[i]] <- (data[[case]][, var[i]] - mCase) + m
        if(!center && scale)
          data[[case]][, var[i]] <- ((data[[case]][, var[i]] - mCase) / sdCase * sd) + mCase
      }
    }
  }

  data
}
