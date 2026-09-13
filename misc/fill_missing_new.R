# Proposed replacement for the function body of R/fill_missing.R

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
  case_names <- names(data)

  for (i_case in seq_along(data)) {

    dat <- data[[i_case]]

    # variables to interpolate: everything except measurement time and phase.
    # Only numeric variables can be interpolated.
    target_vars <- names(dat)[!(names(dat) %in% c(mvar, pvar))]
    numeric_vars <- target_vars[
      vapply(dat[target_vars], is.numeric, logical(1))
    ]

    # 1. interpolate missing measurement times ------------------------------
    if (interpolate_na && anyNA(dat[[mvar]])) {
      missing_mt <- is.na(dat[[mvar]])
      interpolated_mt <- round(.interpolate(dat[[mvar]], extrapolate = TRUE))
      dat[[mvar]][missing_mt] <- interpolated_mt[missing_mt]
    }

    # 2. gaps can only be filled when all measurement times are known -------
    if (anyNA(dat[[mvar]])) {
      warn(
        "Case '", case_names[i_case], "': ", sum(is.na(dat[[mvar]])),
        " measurement time(s) unknown. The case is returned unchanged."
      )
      next
    }

    dat <- dat[order(dat[[mvar]]), , drop = FALSE]

    # 3. add a row for every missing measurement time -----------------------
    #    The added rows carry the measurement time and the phase of the
    #    preceding observation; all other variables are filled in step 5.
    new_dat <- dat[0, , drop = FALSE]

    for (i_row in seq_len(max(0L, nrow(dat) - 1L))) {

      mt_from <- dat[[mvar]][i_row]
      mt_to   <- dat[[mvar]][i_row + 1]

      if (mt_to - mt_from <= 1) next

      new_mt <- seq(mt_from + 1, mt_to - 1)

      tmp <- dat[rep(NA_integer_, length(new_mt)), , drop = FALSE]
      tmp[[mvar]] <- new_mt
      tmp[[pvar]] <- rep(dat[[pvar]][i_row], length(new_mt))

      new_dat <- rbind(new_dat, tmp)
    }

    # 4. combine, sort, and renumber the rows -------------------------------
    new_dat <- rbind(dat, new_dat)
    new_dat <- new_dat[order(new_dat[[mvar]]), , drop = FALSE]
    row.names(new_dat) <- NULL

    # 5. interpolate all missing values -------------------------------------
    #    This covers both the rows added in step 3 and NAs that were already
    #    present in the data.
    for (i_col in numeric_vars) {
      new_dat[[i_col]] <- .interpolate(
        new_dat[[i_col]], x = new_dat[[mvar]]
      )
    }

    data[[i_case]] <- new_dat
  }

  # restore attributes
  attributes(data) <- source_attributes

  data
}


#' Linear interpolation of missing values
#'
#' @param y Vector with the values to interpolate.
#' @param x Vector with the coordinates the values are placed on.
#' @param extrapolate If TRUE, values before the first and after the last known
#'   value are continued linearly. Only meaningful for the measurement-time
#'   variable; interpolating beyond the observed range of a measured variable
#'   would invent data.
#' @return `y` with missing values replaced where possible.
#' @keywords internal
#' @noRd
.interpolate <- function(y, x = seq_along(y), extrapolate = FALSE) {

  known <- which(!is.na(y) & !is.na(x))
  if (length(known) < 2L) return(y)

  fill <- which(is.na(y) & !is.na(x))
  if (length(fill) == 0L) return(y)

  y[fill] <- approx(
    x = x[known], y = y[known], xout = x[fill], method = "linear"
  )$y

  # approx() returns NA outside the range of the known values.
  if (extrapolate && anyNA(y[fill])) {
    lo <- known[1]
    hi <- known[length(known)]

    before <- fill[x[fill] < x[lo]]
    if (length(before) > 0L) {
      slope <- (y[known[2]] - y[lo]) / (x[known[2]] - x[lo])
      y[before] <- y[lo] + (x[before] - x[lo]) * slope
    }

    after <- fill[x[fill] > x[hi]]
    if (length(after) > 0L) {
      prev <- known[length(known) - 1L]
      slope <- (y[hi] - y[prev]) / (x[hi] - x[prev])
      y[after] <- y[hi] + (x[after] - x[hi]) * slope
    }
  }

  y
}
