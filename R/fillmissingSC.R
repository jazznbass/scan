#' Replacing missing measurement times in single-case data
#' 
#' The \code{fillmissingSC} function replaces missing measurements in
#' single-case data.
#' 
#' This procedure is recommended if there are gaps between measurement times
#' (e.g. MT: 1, 2, 3, 4, 5, ... 8, 9) or explicitly missing values in your
#' single-case data and you want to calculate overlap indices
#' (\code{\link{overlapSC}}) or a randomization test (\code{\link{randSC}}).
#' 
#' @inheritParams .inheritParams
#' @param interpolation Alternative options not yet included. Default is
#' \code{interpolation = "linear"}.
#' @param na.rm If set \code{TRUE}, \code{NA} values are also interpolated.
#' Default is \code{na.rm = TRUE}.
#' @return A single-case data frame (SCDF) with missing data points
#' interpolated.  See \code{\link{scdf}} to learn about the SCDF Format.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
#' @examples
#' 
#' ## In his study, Grosche (2011) could not realize measurements each single week for 
#' ## all participants. During the course of 100 weeks, about 20 measurements per person 
#' ## at different times were administered.
#' 
#' ## Fill missing values in a single-case dataset with discontinuous measurement times
#' Grosche2011filled <- fillmissingSC(Grosche2011)
#' study <- c(Grosche2011[2], Grosche2011filled[2])
#' names(study) <- c("Original", "Filled")
#' plot(study, style = "grid")
#' 
#' ## Fill missing values in a single-case dataset that are NA
#' Maggie <- rSC(design_rSC(level = list(0,1)), seed = 123)
#' Maggie_n <- Maggie
#' replace.positions <- c(10,16,18)
#' Maggie_n[[1]][replace.positions,"values"] <- NA
#' Maggie_f <- fillmissingSC(Maggie_n)
#' study <- c(Maggie, Maggie_n, Maggie_f)
#' names(study) <- c("original", "missing", "interpolated")
#' plot(study, marks = list(positions = replace.positions), style = "grid2")
#' 
#' @export
fillmissingSC <- function(data, dvar, mvar, interpolation = "linear", na.rm = TRUE) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
  
  ATTRIBUTES <- attributes(data)
  
  data <- .SCprepareData(data)

  N <- length(data)
  
  for(i in 1:N) {
    dat <- data[[i]]
    if (na.rm) dat <- dat[!is.na(dat[, dvar]), ]
    new.dat <- dat
    for(j in 1 : (nrow(dat) - 1)) {
      if (dat[j + 1, mvar] - dat[j, mvar] != 1){
        
        if (interpolation == "linear") {
          step.size <- (dat[j + 1, dvar] - dat[j, dvar]) / 
                       (dat[j + 1, mvar] - dat[j, mvar])
        }
        for(k in (dat[j, mvar] + 1) : (dat[j + 1, mvar] - 1)) {
          tmp <- dat[j, ]
          tmp[, mvar] <- k
          if (interpolation == "linear")
            tmp[, dvar] <- dat[j, dvar] + step.size * (k - dat[j, mvar])
          new.dat <- rbind(new.dat, linear = tmp) 
        }
      }
    }
    data[[i]] <- new.dat[order(new.dat[, mvar]), ]
  }
  attributes(data) <- ATTRIBUTES
  data
}
