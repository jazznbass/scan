#' Transform variables in every single case of a single case data frame
#'
#' Takes a scdf and applies transformations to each individual case. This is
#' useful to calculate or modify new variables.
#'
#' This function is a method of the generic transformation function. Unlike this
#' one, expressions are calculated serially. This means that the results of the
#' calculation of an expression are the basis for the following calculations.
#' The `all` function is a helper function to calculate values across all 
#' cases. It also takes an expression as an argument. 
#' E.g., `mean(all(values))` will calculate the mean of values across all 
#' cases. `mean(all(values[phase == "A"]))` will calculate the mean of 
#' values where phase is A across all cases.
#'
#' @param _data A scdf.
#' @param ... Expressions.
#' @return A scdf.
#' @export
#' @examples
#' ## Creates a single-case with frequency distributions. The proportion and
#' ## percentage of the frequencies are calculated with transform:
#' design <- design(
#'  n = 3,
#'  level = 5,
#'  distribution = "binomial",
#'  n_trials = 20,
#'  start_value = 0.5
#' )
#' study <- random_scdf(design)
#' transform(study, proportion = values/trials, percentage = proportion * 100)
#' 
#' ## Z standardize the dependent variable and add two new variables:
#' exampleAB %>% 
#'   transform(
#'     values = scale(values), 
#'     mean_values = mean(values), 
#'     sd_values = sd(values)
#'   )
#'
#' ## Use `all` to calculate global variables.   
#' exampleAB %>% 
#'   transform(
#'     values_center_case = values - mean(values[phase == "A"]), 
#'     values_center_global = values - mean(all(values[phase == "A"])),
#'     value_dif = values_center_case - values_center_global
#'   )
#'
#' ## Standardize the values by the overall mean and standarddeviation
#' exampleABC %>%
#'   transform(values = (values - mean(all(values))) / sd(all(values)))


transform.scdf <- function(`_data`, ...) {
  f <- substitute(list(...))
  df_all <- as.data.frame(`_data`)
  
  for(i in seq_along(`_data`)) {
    .list <- as.list(`_data`[[i]])
    .list$all_cases <- .list$all <- function(x) {
      x <- substitute(x)
      eval(x, df_all)
    }
    
    for(j in 2:length(f)) {
      new <- eval(f[c(1,j)], .list, parent.frame())
      `_data`[[i]][[names(new)]] <- new[[1]]
      .list[[names(new)]] <- new[[1]]
      
    }
  }
  `_data`
}

