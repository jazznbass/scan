#' Handling outliers in single-case data
#'
#' Identifies and drops outliers within a single-case data frame (scdf).
#'
#' @inheritParams .inheritParams
#' @param method Specifies the method for outlier identification. Set `method =
#'   "MAD"` for mean average deiviation, `method = "SD"` for standard
#'   deviations, `method = "CI"` for confidence intervals, `method = "Cook"` for
#'   Cook's Distance based on the Piecewise Linear Regression Model.
#' @param criteria Specifies the criteria for outlier identification. Based on
#'   the `method` setting.
#' @return \item{data}{A single-case data frame with substituted outliers.}
#' \item{dropped.n}{A list with the number of dropped data points for each
#' single-case.} \item{dropped.mt}{A list with the measurement-times of dropped
#' data points for each single-case (values are based on the `mt` variable
#' of each single-case data frame).} \item{sd.matrix}{A list with a matrix for
#' each case with values for the upper and lower boundaries based on the
#' standard deviation.} \item{ci.matrix}{A list with a matrix for each
#' single-case with values for the upper and lower boundaries based on the
#' confidence interval.} \item{cook}{A list of Cook's Distances for each
#' measurement of each single-case.} \item{criteria}{Criteria used for outlier
#' analysis.} \item{N}{Number of single-cases.} \item{case.names}{Case
#' identifier.}
#' @details For `method = "SD"`, `criteria = 2` would refer t0 two standard
#'   deviations. For `method = "MAD"`, `criteria = 3.5` would refer to 3.5 times
#'   the mean average deviation. For `method = "CI"`, `criteria = 0.99` would
#'   refer to a 99 percent confidence interval. For `method = "cook"`, `criteria
#'   = "4/n"` would refer to a Cook's Distance greater than 4/n.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
#' @examples
#'
#' ## Identify outliers using 1.5 standard deviations as criterion
#' susanne <- random_scdf(level = 1.0)
#' res_outlier <- outlier(susanne, method = "SD", criteria = 1.5)
#' res_outlier
#'
#' ## Identify outliers in the original data from Grosche (2011)
#' ## using Cook's Distance greater than 4/n as criterion
#' res_outlier <- outlier(Grosche2011, method = "Cook", criteria = "4/n")
#' res_outlier
#'
#' @export
outlier <- function(data, dvar, pvar, mvar, 
                    method = c("MAD", "Cook", "SD", "CI"),
                    criteria = 3.5) {
  
  if (length(criteria) == 2) {
    method <- criteria[1]
    criteria <- criteria[2]
  }
  
  check_args(
    one_of(method, c("MAD", "Cook", "SD", "CI"))
  )

  method <- method[1]
  
  # set defaults attributes
  if (missing(dvar)) dvar <- dv(data) 
  if (missing(pvar)) pvar <- phase(data) 
  if (missing(mvar)) mvar <- mt(data) 
  
  dv(data) <- dvar
  phase(data) <- pvar
  mt(data) <- mvar
  
  data_list <- .prepare_scdf(data)
 
  out <- list()
  
  N <- length(data_list)
  case.names <- names(data_list)
  dropped.mts <- list()
  dropped.n <- list()
  ci.matrix <- list()
  sd.matrix <- list()
  mad.matrix <- list()
  cook <- list()
  
  for(i in 1:N) {
    data <- data_list[[i]]
    
    phases <- rle(as.character(data[, pvar]))$value
    values <- lapply(phases, function(x) data[data[, pvar] == x, dvar])

# CI ----------------------------------------------------------------------
    
    if (identical(method, "CI")) {
      cut_off <- as.numeric(criteria)
      mat <- matrix(NA, length(values), ncol = 5)
      colnames(mat) <- c("phase","m","se","lower", "upper")
      rownames(mat) <- names(values)
      filter <- c()
      fac <- qnorm((1 - cut_off) / 2, lower.tail = FALSE)
      
      for(p in 1:length(values)) {
        x <- values[[p]]
        mat[p,"m"] <- mean(x)
        mat[p,"se"] <- sd(x)/sqrt(length(x))
        mat[p,"lower"] <- mean(x) - fac * (sd(x)/sqrt(length(x)))
        mat[p,"upper"] <- mean(x) + fac * (sd(x)/sqrt(length(x)))
        filter <- c(filter, (x < mat[p, "lower"]) | (x > mat[p, "upper"]))
      }
      mat <- as.data.frame(mat)
      mat$phase <- phases
      ci.matrix[[i]] <- mat
    }

# MAD ---------------------------------------------------------------------

    if (identical(method, "MAD")) {
      fac <- as.numeric(criteria)
      mat <- matrix(NA, length(values), ncol = 5)
      colnames(mat) <- c("phase", "md", "mad", "lower", "upper")
      filter <- c()
      for(p in 1:length(values)) {
        x <- values[[p]]
        mat[p,"md"] <- median(x)
        mat[p,"mad"] <- mad(x,constant = 1)
        mat[p,"lower"] <- median(x) - fac * mad(x)
        mat[p,"upper"] <- median(x) + fac * mad(x)
        filter <- c(filter, (x < mat[p,"lower"]) | (x > mat[p,"upper"]))
      }
      mat <- as.data.frame(mat)
      mat$phase <- phases
      mad.matrix[[i]] <- mat
    }	

# SD ----------------------------------------------------------------------

    if (identical(method, "SD")) {
      SD <- as.numeric(criteria)
      mat <- matrix(NA, length(values), ncol = 5)
      colnames(mat) <- c("phase", "m", "sd", "lower", "upper")
      filter <- c()
      for(p in 1:length(values)) {
        x <- values[[p]]
        mat[p,"m"] <- mean(x)
        mat[p,"sd"] <- sd(x)
        mat[p,"lower"] <- mean(x) - SD * sd(x)
        mat[p,"upper"] <- mean(x) + SD * sd(x)
        filter <- c(filter, (x < mat[p,"lower"]) | (x > mat[p, "upper"]))
      }
      mat <- as.data.frame(mat)
      mat$phase <- phases
      sd.matrix[[i]] <- mat
    }		

# Cook --------------------------------------------------------------------

    if (identical(method, "Cook")) {
      
      if (criteria == "4/n") 
        cut_off <- 4/nrow(data)
      else 
        cut_off <- as.numeric(criteria)

      reg <- plm(data_list[i], dvar = dvar, pvar = pvar, mvar = mvar)$full.model
      
      cd <- cooks.distance(reg)
      filter <- cd >= cut_off
      cook[[i]] <- data.frame(Cook = round(cd, 2), MT = data_list[[i]][, mvar])
    }		
    
    dropped.mts[[i]] <- data_list[[i]][filter, mvar]
    dropped.n[[i]]   <- sum(filter)
    
    data_list[[i]] <- data_list[[i]][!filter, ]
  }
  
  out$data <- data_list
  out$dropped.mt <- dropped.mts
  out$dropped.n <- dropped.n
  out$ci.matrix <- ci.matrix
  out$sd.matrix <- sd.matrix
  out$mad.matrix <- mad.matrix
  out$cook <- cook
  out$criteria <- criteria
  out$N <- N
  out$case.names <- case.names
  
  class(out) <- c("sc_outlier")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("mt")) <- mvar
  attr(out, opt("dv")) <- dvar
  out
}
