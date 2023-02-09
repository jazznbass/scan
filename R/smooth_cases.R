#' Smoothing single-case data
#'
#' *This function is superseded by the more versatile 
#' \code{\link{transform.scdf}} function.*
#' The \code{smooth_cases} function provides procedures to smooth single-case 
#' data (i.e., to eliminate noise). A moving average function (mean- or
#' median-based) replaces each data point by the average of the surrounding
#' data points step-by-step. With a local regression function, each data point
#' is regressed by its surrounding data points.
#'
#' \code{moving_median}, \code{moving_mean}, and \code{local_regression} are 
#' helper function for \code{\link{transform.scdf}} returning the smoothed 
#' values of a numeric vector.
#'
#' @inheritParams .inheritParams
#' @param method,FUN Method determining the smoothed scores. Default \code{method =
#' "median"} is a moving median function. Further possible values are:
#' \code{"mean"} and a non-parametric \code{"regression"}.
#' @param intensity For \code{method = "median"} and \code{"mean"} it
#' is the lag used for computing the average. Default is \code{intensity = 1}.
#' In case of \code{method = "regression"} it is the proportion of
#' surrounding data influencing each data point, which is \code{intensity =
#' 0.2} by default.
#' @return Returns a data frame (for each single-case) with smoothed data
#' points. See \code{\link{scdf}} to learn about the format of these data
#' frames.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
#' @examples
#'
#' ## Use the three different smoothing functions and compare the results
#' study <- c(
#'   "Original" = Huber2014$Berta,
#'   "Moving median" = smooth_cases(Huber2014$Berta, method = "median"),
#'   "Moving mean" = smooth_cases(Huber2014$Berta, method = "mean"),
#'   "Local regression" = smooth_cases(Huber2014$Berta, method = "regression")
#' )
#' plot(study)
#' 
#' Huber2014$Berta %>% 
#' transform(
#'   "compliance (moving median)" = moving_median(compliance),
#'   "compliance (moving mean)" = moving_mean(compliance),
#'   "compliance (local regression)" = local_regression(compliance, mt)
#' )
#' 
#' @export
smooth_cases <- function(data, dvar, mvar, 
                         method = "mean", 
                         intensity = NULL, 
                         FUN = NULL) {

  if (!is.null(FUN)) method <- FUN
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv)
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) 

  scdf_attr(data, .opt$dv) <- dvar
  scdf_attr(data, .opt$mt) <- mvar
  
  data <- .prepare_scdf(data)
  copy_attributes <- attributes(data)
  copy_names <- names(data)
  if (method %in% c("mean", "movingMean")) {
    if (is.null(intensity)) intensity <- 1
    out <- lapply(data, function(x) {
      x[, dvar] <- .moving_average(x[[dvar]], intensity, mean)
      x
    })
  }
  if (method %in% c("median", "movingMedian")) {
    if (is.null(intensity)) intensity <- 1
    out <- lapply(data, function(x) {
      x[, dvar] <- .moving_average(x[[dvar]], intensity, median)
      x
    })
  }
  if (method %in% c("regression", "localRegression")) {
    if (is.null(intensity)) intensity <- 0.2
    out <- lapply(data, function(x) {
      xval <- x[[mvar]][!is.na(x[[dvar]])]
      yval <- x[[dvar]][!is.na(x[[dvar]])]
      x[, dvar] <- lowess(yval ~ xval, f = intensity)$y
      x
    })
  }
  attributes(out) <- copy_attributes
  names(out) <- copy_names
  out
}

