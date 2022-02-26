#' Trend analysis for single-cases data
#' 
#' The \code{trend} function provides an overview of linear trends in
#' single-case data.  By default, it gives you the intercept and slope of a
#' linear and a squared regression of measurement-time on scores.  Models are
#' computed separately for each phase and across all phases. For a
#' more advanced application, you can add regression models using the R
#' specific formula class.
#' 
#' @inheritParams .inheritParams
#' @param offset An offset for the first measurement-time of each phase (MT). If
#' \code{offset = 0}, the phase measurement is handled as MT 1.
#' Default is \code{offset = -1}, setting the first value of MT to 0.
#' @param model A string or a list of (named) strings each depicting one
#' regression model. This is a formula expression of the standard R class. The
#' parameters of the model are \code{values}, \code{mt} and \code{phase}.
#' @return \item{trend}{A matrix containing the results (Intercept, B and beta)
#' of separate regression models for phase A, phase B, and the whole data.}
#' \item{offset}{Numeric argument from function call (see \code{Arguments}
#' section).}
#' @author Juergen Wilbert
#' @seealso \code{\link{describe}}, \code{\link{autocorr}},
#' \code{\link{plm}}
#' @examples
#' 
#' ## Compute the linear and squared regression for a random single-case
#' design <- design(slope = 0.5)
#' matthea <- random_scdf(design)
#' trend(matthea)
#' 
#' ## Besides the linear and squared regression models compute two custom models:
#' ## a) a cubic model, and b) the values predicted by the natural logarithm of the
#' ## measurement time.
#' design <- design(slope = 0.3)
#' ben <- random_scdf(design)
#' trend(ben, offset = 0, model = c("Cubic" = values ~ I(mt^3), "Log Time" = values ~ log(mt)))
#' 
#' @export
trend <- function(data, dvar, pvar, mvar, offset = -1, model = NULL) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) 
    dvar <- scdf_attr(data, .opt$dv)    else scdf_attr(data, .opt$dv)    <- dvar
  if (missing(pvar)) 
    pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) 
    mvar <- scdf_attr(data, .opt$mt)    else scdf_attr(data, .opt$mt)    <- mvar
  
  data <- .prepare_scdf(data)

  phase <- NULL
  N <- length(data)
  if(N > 1) {
    stop("Multiple single-cases are given. ",          
         "Calculations can only be applied to one single-case data set.\n")
  }
  data <- data[[1]]
  
  design <- rle(as.character(data[, pvar]))$values
  while(any(duplicated(design))) {
    design[anyDuplicated(design)] <- paste0(
      design[anyDuplicated(design)], 
      ".phase", 
      anyDuplicated(design)
    )
  }
  
  phases <- .phasestructure(data, pvar = pvar)
  
  fomulas <- c(
    formula(paste0(dvar, " ~ ", mvar)) , 
    formula(paste0(dvar, " ~ I(", mvar, "^2)"))
  )
  fomulas_names <- c("Linear", "Squared")
  if(!is.null(model)) {
    fomulas <- c(fomulas, model)
    fomulas_names <- c(fomulas_names, names(model))
  }
  tmp <- length(design) + 1
  rows <- paste0( paste0(rep(fomulas_names, each = tmp), ".") ,c("ALL", design))
  
  ma <- matrix(NA, nrow = length(rows), ncol = 3)
  row.names(ma) <- rows
  colnames(ma) <- c("Intercept", "B", "Beta")
  ma <- as.data.frame(ma)
  
  for(f in 1:length(fomulas)) {
    var <- paste0(fomulas_names[f], ".ALL")
    data.phase <- data
    data.phase[, mvar] <- data.phase[, mvar] - 
                          min(data.phase[, mvar], na.rm = TRUE) + 1 + offset
    ma[which(rows == var), 1:3] <- .beta_weights(
      lm(fomulas[[f]], data = data.phase)
    )
    for(p in 1: length(design)) {
      data.phase <- data[phases$start[p]:phases$stop[p], ]
      data.phase[, mvar] <- data.phase[,mvar] - 
                            min(data.phase[, mvar], na.rm = TRUE) + 1 + offset
      var <- paste0(fomulas_names[f], ".", design[p])
      ma[which(rows == var), 1:3] <- .beta_weights(
        lm(fomulas[[f]], data = data.phase)
      )
    }
  }
  
  out <- list(
    trend = ma, 
    offset = offset, 
    formulas = fomulas_names, 
    design = design
  )
  class(out) <- c("sc_trend")
  attr(out, .opt$phase) <- pvar
  attr(out, .opt$mt) <- mvar
  attr(out, .opt$dv) <- dvar
  out
}

#' @rdname trend
#' @export
trendSC <- function(...) {
  .deprecated_warning("trend", "trendSC")
  trend(...)
}
