#' Handling outliers in single-case data
#' 
#' Identifies and drops outliers within a single-case data frame (scdf).
#' 
#' 
#' @inheritParams .inheritParams
#' @param criteria Specifies the criteria for outlier identification. Set
#' \code{criteria = c("SD", 2)} to define two standard deviations as limit.
#' This is also the default setting. To use the 99\% Confidence Interval use
#' \code{criteria = c("CI", 0.99)}. Set \code{criteria = c("Cook", "4/n")} to
#' define any data point with a Cook's Distance greater than 4/n as an outlier,
#' based on the Piecewise Linear Regression Model.
#' @return 
#' \item{data}{A single-case data frame with substituted outliers.}
#' \item{dropped.n}{A list with the number of dropped data points for each
#' single-case.}
#' \item{dropped.mt}{A list with the measurement-times of dropped
#' data points for each single-case (values are based on the \code{mt} variable
#' of each single-case data frame).} 
#' \item{sd.matrix}{A list with a matrix for each case with values for the 
#' upper and lower boundaries based on the standard deviation.} 
#' \item{ci.matrix}{A list with a matrix for each single-case with values 
#' for the upper and lower boundaries based on the confidence interval.} 
#' \item{cook}{A list of Cook's Distances for each measurement of each single-case.} 
#' \item{criteria}{Criteria used for outlier analysis.} 
#' \item{N}{Number of single-cases.} 
#' \item{case.names}{Case identifier.}
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
#' @examples
#' 
#' ## Identify outliers using 1.5 standard deviations as criterion
#' susanne <- rSC(level = 1.0)
#' res.outlier <- outlierSC(susanne, criteria = c("SD", 1.5))
#' plotSC(susanne, marks = res.outlier)
#' 
#' ## Identify outliers in the original data from Grosche (2011) using Cook's Distance
#' ## greater than 4/n as criterion
#' res.outlier <- outlierSC(Grosche2011, criteria = c("Cook", "4/n"))
#' plotSC(Grosche2011, marks = res.outlier)
#' 
#' @export
outlierSC <- function(data, dvar, pvar, mvar, criteria = c("MAD", "3.5")) {
  
  if(!any(criteria[1] %in% c("MAD", "Cook", "SD", "CI"))) {
    stop("Unknown criteria. Please check.")
  }
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
  
  data.list <- .SCprepareData(data)
 
  out <- list()
  
  N <- length(data.list)
  case.names <- names(data.list)
  dropped.mts <- list()
  dropped.n <- list()
  ci.matrix <- list()
  sd.matrix <- list()
  mad.matrix <- list()
  cook <- list()
  
  for(i in 1:N) {
    data <- data.list[[i]]
    
    phases <- rle(as.character(data[, pvar]))$value
    values <- lapply(phases, function(x) data[data[, pvar] == x, dvar])

# CI ----------------------------------------------------------------------
    
    if (identical(criteria[1], "CI")) {
      cut.off <- as.numeric(criteria[2])
      mat <- matrix(NA, length(values), ncol = 5)
      colnames(mat) <- c("phase","m","se","lower", "upper")
      rownames(mat) <- names(values)
      filter <- c()
      fac <- qnorm((1 - cut.off) / 2, lower.tail = FALSE)
      
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

    if (identical(criteria[1], "MAD")) {
      fac <- as.numeric(criteria[2])
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

    if (identical(criteria[1], "SD")) {
      SD <- as.numeric(criteria[2])
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

    if (identical(criteria[1], "Cook")) {
      
      if (criteria[2] == "4/n") 
        cut.off <- 4/nrow(data)
      else 
        cut.off <- as.numeric(criteria[2])

      reg <- plm(data.list[i], dvar = dvar, pvar = pvar, mvar = mvar)$full.model
      
      cd <- cooks.distance(reg)
      filter <- cd >= cut.off
      cook[[i]] <- data.frame(Cook = round(cd, 2), MT = data.list[[i]][, mvar])
    }		
    
    dropped.mts[[i]] <- data.list[[i]][filter, mvar]
    dropped.n[[i]]   <- sum(filter)
    
    data.list[[i]] <- data.list[[i]][!filter, ]
  }
  
  out$data <- data.list
  out$dropped.mt <- dropped.mts
  out$dropped.n <- dropped.n
  out$ci.matrix <- ci.matrix
  out$sd.matrix <- sd.matrix
  out$mad.matrix <- mad.matrix
  out$cook <- cook
  out$criteria <- criteria
  out$N <- N
  out$case.names <- case.names
  class(out) <- c("sc","outlier")
  out
}
