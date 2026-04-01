#' Replacing missing measurement points in single-case data
#'
#' The `fillmissing()` function replaces missing measurements in single-case
#' data. It linearly interpolates missing data points between two existing
#' measurements for all variables except the measurement time and phase. The
#' measurement time variable is filled with the missing time points. The phase
#' variable is copied from the previous measurement time point. If mt values are
#' missing (`NA`), they are also interpolated if `interpolate_na = TRUE`.
#'
#' @details The `fill_missing()` function is designed to handle single-case data
#' with missing measurement points. It performs linear interpolation to estimate
#' the missing values based on the existing data points. The function iterates
#' through each single-case in the provided single-case data frame (scdf) and
#' identifies gaps in the measurement time variable. For each gap, it calculates
#' the step size for linear interpolation and fills in the missing values for
#' all target variables (i.e., all variables except the measurement time and
#' phase). The interpolated data points are then added to the single-case data
#' frame, and the final result is sorted by measurement time. This function is
#' particularly useful for preparing single-case data for further analysis, such
#' as calculating overlap indices or conducting randomization tests, where
#' continuous measurement times are required. It ensures that the data is
#' complete by filling in the missing measurement points in a systematic manner.

#' 
#' @inheritParams .inheritParams
#' @param interpolate_na If set `TRUE`, `NA` values in the measurement time variable are
#'   also interpolated. Default is `TRUE`.
#' @return A single-case data frame with interpolated missing data points.
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
#' study
#' 
#' ## An example with multiple interpolated variables
#' 
#' rolf_n <- exampleAB_add
#' rolf_n[[1]] <- rolf_n[[1]][-c(3,7,8),]
#' rolf_f <- fill_missing(rolf_n)
#' study1 <- c("original" = exampleAB_add, "interpolated" = rolf_f)
#' study1
#' 
#' ## Example with missing NAs in measurement time
#' Maggie2 <- random_scdf(design(level = list(0,1)), seed = 123)
#' Maggie2_n <- Maggie2
#' Maggie2_n[[1]][c(5,12:14,20), "mt"] <- NA
#' Maggie2_f <- fill_missing(Maggie2_n)
#' study2 <- c("original" = Maggie2, "interpolated" = Maggie2_f)
#' study2
#'
#' @export
fill_missing <- function(data, 
                         dvar, 
                         mvar, 
                         pvar,
                         interpolate_na = TRUE) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  #  preserve attributes
  source_attributes <- attributes(data)
  
  data <- .prepare_scdf(data)
  N <- length(data)
  
  for(i_case in 1:N) {
    dat <- data[[i_case]]
    
    # get variable names of all variables that are not mvar or pvar
    target_vars <- names(dat)[!(names(dat) %in% c(mvar, pvar))]
    
    # interpolate missing measurement times
    if (interpolate_na) {
      dat[[mvar]] <- .interpolate(dat[[mvar]]) |> round()
    }
    
    new_dat <- dat[0, , drop = FALSE]  # empty data frame to store new data

    for(i_row in 1:(nrow(dat) - 1)) {
      
      # skip if current or next measurement time is NA
      if (is.na(dat[[mvar]][i_row]) || is.na(dat[[mvar]][i_row + 1])) next
      
      # check for missing measurement times
      if (dat[i_row + 1, mvar] - dat[i_row, mvar] == 1) next
      
      # interpolate missing values for all target variables
      new_rows <- (dat[i_row, mvar] + 1) : (dat[i_row + 1, mvar] - 1)
      
      tmp <- dat[rep(NA_integer_, length(new_rows)), , drop = FALSE]
      tmp[pvar] <- dat[i_row, pvar]
      new_mt <- .interpolate(
        c(dat[[mvar]][i_row], tmp[[mvar]], dat[[mvar]][i_row + 1] )
      ) |> round()
      tmp[[mvar]] <- new_mt[2:(length(new_mt) -1)]
      
      for(i_col in target_vars) {
        new_values <- .interpolate(
          c(dat[[i_col]][i_row], tmp[[i_col]], dat[[i_col]][i_row + 1] )
        )
        tmp[[i_col]] <- new_values[2:(length(new_values) -1)]
      }
      new_dat <- rbind(new_dat, tmp)
    }
    # combine original and new data
    new_dat <- rbind(dat, new_dat)
    
    # sort by measurement time and store
    data[[i_case]] <- new_dat[sort.list(new_dat[[mvar]]), ]
  }
  
  # restore attributes
  attributes(data) <- source_attributes

  return(data)
}

.interpolate <- function(mt) {
  
  if (!any(is.na(mt))) return(mt)

  x <- mt
  n <- length(x)
  na <- is.na(x)
  idx <- seq_len(n)
  
  # Only interpolate NAs that have a following non-NA value
  ok <- na & (idx < max(which(!na)))
  
  x[ok] <- approx(x = idx[!na], y = x[!na], xout = idx[ok], method = "linear")$y
  
  return(x)
}


