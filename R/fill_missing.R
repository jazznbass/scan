#' Replacing missing measurement times in single-case data
#'
#' The `fillmissing()` function replaces missing measurements in single-case
#' data.
#'
#' This procedure is recommended if there are gaps between measurement times
#' (e.g. MT: 1, 2, 3, 4, 5, ... 8, 9) or explicitly missing values in your
#' single-case data and you want to calculate overlap indices ([overlap()]) or a
#' randomization test ([rand_test()]).
#'
#' @inheritParams .inheritParams
#' @param na.rm If set `TRUE`, `NA` values are also interpolated. Default is
#'   `na.rm = TRUE`.
#' @return A single-case data frame with interpolated missing data points.  See
#'   [scdf()] to learn about the SCDF Format.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
#' @examples
#'
#' ## In his study, Grosche (2011) could not realize measurements each
#' ## single week for all participants. During the course of 100 weeks,
#' ## about 20 measurements per person at different times were administered.
#'
#' ## Fill missing values in a single-case dataset with discontinuous
#' ## measurement times
#' Grosche2011filled <- fill_missing(Grosche2011)
#' study <- c(Grosche2011[2], Grosche2011filled[2])
#' names(study) <- c("Original", "Filled")
#' plot(study)
#'
#' ## Fill missing values in a single-case dataset that are NA
#' Maggie <- random_scdf(design(level = list(0,1)), seed = 123)
#' Maggie_n <- Maggie
#' replace.positions <- c(10,16,18)
#' Maggie_n[[1]][replace.positions,"values"] <- NA
#' Maggie_f <- fill_missing(Maggie_n)
#' study <- c(Maggie, Maggie_n, Maggie_f)
#' names(study) <- c("original", "missing", "interpolated")
#' plot(study, marks = list(positions = replace.positions), style = "grid2")
#'
#' @export
fill_missing <- function(data, dvar, mvar, 
                         na.rm = TRUE) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar

  source_attributes <- attributes(data)
  
  data <- .prepare_scdf(data)

  N <- length(data)
  
  for(i in 1:N) {
    dat <- data[[i]]
    if (na.rm) dat <- dat[!is.na(dat[[dvar]]), ]
    new_dat <- dat
    for(j in 1:(nrow(dat) - 1)) {
      if (dat[j + 1, mvar] - dat[j, mvar] != 1){
        step_size <- (dat[j + 1, dvar] - dat[j, dvar]) / 
                     (dat[j + 1, mvar] - dat[j, mvar])
        
        for(k in (dat[j, mvar] + 1) : (dat[j + 1, mvar] - 1)) {
          tmp <- dat[j, ]
          tmp[[mvar]] <- k
          tmp[[dvar]] <- dat[j, dvar] + step_size * (k - dat[j, mvar])
          new_dat <- rbind(new_dat, linear = tmp) 
        }
      }
    }
    data[[i]] <- new_dat[sort.list(new_dat[[mvar]]), ]
  }
  attributes(data) <- source_attributes
  data
}

