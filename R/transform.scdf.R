#' Transform every single case of a single case data frame
#'
#' Takes an scdf and applies transformations to each individual case. This is
#' useful to calculate or modify new variables.
#'
#' This function is a method of the generic `transform()` function.  
#' Unlike the generic version, expressions are evaluated **serially**:  
#' the result of one expression is used as the basis for subsequent computations.  
#'
#' Several helper functions can be used inside expressions:
#'
#' - **`n()`**: returns the number of measurements in a case.  
#' - **`all_cases()`**: extracts the values of a variable across all cases.  
#'   Takes an expression as argument.  
#'   For example:  
#'   - `mean(all_cases(values))` calculates the mean of `values` across all cases.  
#'   - `mean(all_cases(values[phase == "A"]))` calculates the mean of all values where `phase == "A"`.  
#' - **`rowwise()`**: applies a calculation separately to each row.  
#'   Example: `rowwise(sum(values, mt, na.rm = TRUE))`.  
#' - **`across_cases()`**: creates new variables or replaces existing ones across all cases.  
#'   Example: `across_cases(values_ranked = rank(values, na.last = "keep"))` 
#'
#' @param _data An scdf.
#' @param ... Expressions.
#' @return An scdf.
#' @family data manipulation functions
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
#' exampleAB |>
#'   transform(
#'     values = scale(values),
#'     mean_values = mean(values),
#'     sd_values = sd(values)
#'   )
#'
#' ## Use `all` to calculate global variables.
#' exampleAB |>
#'   transform(
#'     values_center_case = values - mean(values[phase == "A"]),
#'     values_center_global = values - mean(all(values[phase == "A"])),
#'     value_dif = values_center_case - values_center_global
#'   )
#'
#' ## Use `across_cases` to calculate or replace a variable with values from
#' ## all cases. E.g., standardize the dependent variable:
#' exampleABC |>
#'   transform(
#'     across_cases(values = scale(values))
#'   )
#'
#' ## Rank transform the values based on all cases vs. within each case:
#' exampleABC |>
#'   transform(
#'     across_cases(values_across = rank(values, na.last="keep")),
#'     value_within = rank(values, na.last="keep")
#'   )
#'
#' ## Three helper functions to smooth the data
#' Huber2014$Berta |>
#' transform(
#'   "compliance (moving median)" = moving_median(compliance),
#'   "compliance (moving mean)" = moving_mean(compliance),
#'   "compliance (local regression)" = local_regression(compliance, mt)
#' )
#'
#' ## Function first_of() helps to set NAs for specific phases.
#' ## E.g., you want to replace the first two values of phase A and the first
#' ## value of phase B and its preceding value.
#'
#' byHeart2011 |>
#'   transform(
#'     values = set_na_at(values, phase == "A", 0:1),
#'     values = set_na_at(values, phase == "B", -1:0)
#'   )
#' @export
transform.scdf <- function(`_data`, ...) {
  expressions <- substitute(list(...))
  original_attr <- attributes(`_data`)
  .df <- as.data.frame(`_data`)
  
  for(i_expression in 2:length(expressions)) {
    
    if(startsWith(deparse(expressions[[i_expression]]), "across_cases(")) {
      .list_env <- as.list(.df)
     
      # across cases
      .list_env$across_cases <- function(...) {
        exp_across <- substitute(list(...))
        for (i in 2:length(exp_across)) {
          new <- eval(exp_across[c(1,i)], .list_env)
          .df[[names(new)]] <- new[[1]]
          .list_env[[names(new)]] <- new[[1]]
        }
        .df
      }
      
      new <- eval(expressions[c(1,i_expression)], .list_env, parent.frame())
      .df <- new[[1]]
     
      `_data` <- suppressMessages(as_scdf(.df))
     
      #attributes(`_data`) <- original_attr
    } else {
      # For across case calculations
      .df <- as.data.frame(`_data`)
      
      # case by case loop
      for(i_case in seq_along(`_data`)) {
        .list_env <- as.list(`_data`[[i_case]])
        
        ## additional functions all(), all_cases() and n()
        .list_env$all_cases <- .list_env$all <- function(x) {
          x <- substitute(x)
          eval(x, .df)
        }
        .list_env$n <- function() {nrow(`_data`[[i_case]])}
        .list_env$rowwise <- function(expr) {
          expr <- substitute(expr)
          out <- c()
          for(i_row in 1:nrow(`_data`[[i_case]])) {
            out <- c(out, eval(expr, `_data`[[i_case]][i_row, ]))
          }
          out
        }
        
        # compute new variable
        new <- eval(expressions[c(1,i_expression)], .list_env, parent.frame())
        
        # Add new variable
        `_data`[[i_case]][[names(new)]] <- new[[1]]
        
        
      }
      .df <- as.data.frame(`_data`)
    }
  }
  `_data`
}




