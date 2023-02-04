#' Standardize values of an scdf file
#'
#' *This function is superseded by the much more versatile `transform` function 
#' (see example below).*
#' This function scales the measured values of an scdf file. It allows for mean
#' centering and standardization based on each single-case data set or a
#' scaling across all cases included in an scdf.

#'
#' @inheritParams .inheritParams
#' @param var A character string or a vector of character strings with variable
#'   names that should be scaled.
#' @param center If set TRUE, data are mean centered.
#' @param scale If set TRUE, the standard deviation is set.
#' @param m The target mean for centering.
#' @param sd The target standard deviation for scaling
#' @param grand If set TRUE, scaling is based on the mean and standard deviation
#'   of all values across all single-cases within the scdf.
#' @return An scdf with the scaled values.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @examples
#'
#' ## Standardize a multiple case scdf and compute an hplm
#' exampleAB_50 %>%
#'   standardize("values", center = TRUE, scale = TRUE) %>%
#'   hplm(ex_sc)
#' 
#' ## The more versatile transform function supersedes standardize:
#' exampleAB_50 %>%
#'   tansform(values = (values - mean(all(values))) / sd(all(values))) %>%
#'   hplm()
#' @export

standardize <- function(data, 
                        var, 
                        center = TRUE, 
                        scale = FALSE, 
                        m = 0, 
                        sd = 1, 
                        grand = TRUE) {
  
  data <- .prepare_scdf(data)

  N <- length(data)

  if (grand) {
    means <- c()
    sds <- c()
    for (i in 1:length(var)) {
      means <- c(
        means, 
        mean(unlist(lapply(data, function(x) x[, var[i]])), na.rm = TRUE)
      )
      sds <- c(
        sds, 
        sd(unlist(lapply(data, function(x) x[, var[i]])), na.rm = TRUE)
      )
    }

    for (case in 1:N) {
      for (i in 1:length(var)) {
        values <- data[[case]][, var[i]]
        if (center && scale) {
          data[[case]][, var[i]] <- (values - means[i]) / sds[i] * sd + m
        }
        if (center && !scale) {
          data[[case]][, var[i]] <- (values - means[i]) + m
        }
        if (!center && scale) {
          data[[case]][, var[i]] <- ((values - means[i]) / sds[i] * sd) + means[i]
        }
      }
    }
  }

  if (!grand) {
    for (case in 1:N) {
      for (i in 1:length(var)) {
        values <- data[[case]][, var[i]]
        m_case <- mean(data[[case]][, var[i]], na.rm = TRUE)
        sd_case <- sd(data[[case]][, var[i]], na.rm = TRUE)
        if (center && scale) {
          data[[case]][, var[i]] <- (values - m_case) / sd_case[i] * sd + m
        }
        if (center && !scale) {
          data[[case]][, var[i]] <- (values - m_case) + m
        }
        if (!center && scale) {
          data[[case]][, var[i]] <- ((values - m_case) / sd_case * sd) + m_case
        }
      }
    }
  }

  data
}
