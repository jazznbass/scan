#' Trend analysis for single-cases data
#'
#' The \code{trend} function provides an overview of linear trends in single
#' case data. By default, it provides the intercept and slope of a linear and
#' quadratic regression of measurement time on scores. Models are calculated
#' separately for each phase and across all phases. For more advanced use, you
#' can add regression models using the R-specific formula class.
#'
#' @inheritParams .inheritParams
#' @param first_mt A numeric setting the value for the first measurement-time.
#'   Default = 0.
#' @param offset (Deprecated. Please use first_mt). An offset for the first
#'   measurement-time of each phase. If \code{offset = 0}, the phase measurement
#'   is handled as MT 1. Default is \code{offset = -1}, setting the first value
#'   of MT to 0.
#' @param model A string or a list of (named) strings each depicting one
#'   regression model. This is a formula expression of the standard R class. The
#'   parameters of the model are \code{values}, \code{mt} and \code{phase}.
#' @return \item{trend}{A matrix containing the results (Intercept, B and beta)
#'   of separate regression models for phase A, phase B, and the whole data.}
#' \item{offset}{Numeric argument from function call (see \code{Arguments}
#' section).}
#' @author Juergen Wilbert
#' @seealso \code{\link{describe}}
#' @family regression functions
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
trend <- function(data, dvar, pvar, mvar, 
                  offset = "deprecated",
                  first_mt = 0,
                  model = NULL) {

  
  if (is.numeric(offset)) first_mt <- offset + 1
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data)
  if (missing(mvar)) mvar <- mt(data)
  dv(data)    <- dvar
  phase(data) <- pvar
  mt(data)    <- mvar
  
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
  fomulas_names <- c("Linear", "Quadratic")
  if(!is.null(model)) {
    fomulas <- c(fomulas, model)
    fomulas_names <- c(fomulas_names, names(model))
  }
  tmp <- length(design) + 1
  rows <- paste0(paste0(rep(fomulas_names, each = tmp), "."), c("ALL", design))
  
  ma <- matrix(NA, nrow = length(rows), ncol = 3)
  row.names(ma) <- rows
  colnames(ma) <- c("Intercept", "B", "Beta")
  ma <- as.data.frame(ma)
  
  for(i_formula in 1:length(fomulas)) {
    data_phase <- data
    mvar_correction <- min(data_phase[[mvar]], na.rm = TRUE) - first_mt
    data_phase[[mvar]] <- data_phase[[mvar]] - mvar_correction
    
    .row <- which(rows == paste0(fomulas_names[i_formula], ".ALL"))
    ma[.row, 1:3] <- .beta_weights(lm(fomulas[[i_formula]], data = data_phase))
    for(p in 1:length(design)) {
      data_phase <- data[phases$start[p]:phases$stop[p], ]
      mvar_correction <- min(data_phase[[mvar]], na.rm = TRUE) - first_mt
      data_phase[[mvar]] <- data_phase[[mvar]] - mvar_correction 
      .row <- which(rows == paste0(fomulas_names[i_formula], ".", design[p]))
      ma[.row, 1:3] <- .beta_weights(lm(fomulas[[i_formula]], data=data_phase))
    }
  }
  
  out <- list(
    trend = ma, 
    offset = offset, 
    first_mt = first_mt,
    formulas = fomulas_names, 
    design = design
  )
  class(out) <- c("sc_trend")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("mt")) <- mvar
  attr(out, opt("dv")) <- dvar
  out
}
