#' Replacing missing measurement points in single-case data
#'
#' The `fill_missing()` function replaces missing values in single-case data. It
#' linearly interpolates missing values of all variables except the measurement
#' time and the phase variable. Measurement times that are not part of the data
#' are added and the phase variable is copied from the previous measurement
#' time point. If mt values are missing (`NA`), they are also interpolated if
#' `interpolate_na = TRUE`.
#'
#' @details The `fill_missing()` function is designed to handle single-case data
#'   with missing measurement points. It performs linear interpolation to
#'   estimate the missing values based on the existing data points. The function
#'   iterates through each single-case in the provided single-case data frame
#'   (scdf) and identifies gaps in the measurement time variable. For each gap, a
#'   new measurement is added which carries the phase of the preceding
#'   observation. Afterwards, all missing values are interpolated. This covers
#'   both the added measurement times and `NA` values that were already present
#'   in the data. The final result is sorted by measurement time. This function
#'   is particularly useful for preparing single-case data for further analysis,
#'   such as calculating overlap indices or conducting randomization tests, where
#'   continuous measurement times are required.
#'
#'   Only numeric variables are interpolated; other variables remain `NA` in
#'   added measurement times. Values at the very beginning or the very end of a
#'   series cannot be interpolated and remain `NA`. If measurement times remain
#'   unknown after interpolation, the case is returned unchanged with a warning,
#'   because filling gaps would replace these observations with interpolated
#'   values.
#'
#'   Interpolation runs across the whole series and does not take the phase
#'   structure into account. This is deliberate: interpolating within phases
#'   would impose the very level difference that the analysis sets out to
#'   estimate. Note that the resulting values are conservative with respect to
#'   level effects, and that a gap spanning a phase change produces a run of
#'   interpolated values that rises or falls towards the level of the adjacent
#'   phase. With large gaps this can affect trend based measures. Use `mark =
#'   TRUE` to keep track of which values were interpolated.
#'
#' @inheritParams .inheritParams
#' @param interpolate_na If set `TRUE`, `NA` values in the measurement time
#'   variable are also interpolated. Default is `TRUE`.
#' @param mark If set `TRUE`, a logical variable `interpolated` is added to each
#'   case which is `TRUE` for every measurement that contains at least one
#'   interpolated value. Default is `FALSE`. The function stops if a variable of
#'   that name already exists.
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
#' ## Missing values in the dependent variable, marked in the output
#' case <- scdf(
#'   c(3, 6, 2, 4, 3, 5, 2, NA, 3, 2, 6, 7, 5, 8, 6, NA, 4, 8, 5, 6),
#'   phase_design = c(A = 10, B = 10)
#' )
#' fill_missing(case, mark = TRUE)
#'
#' @export
fill_missing <- function(data,
                         dvar,
                         mvar,
                         pvar,
                         interpolate_na = TRUE,
                         mark = FALSE) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar

  check_args(
    is_logical(interpolate_na),
    is_logical(mark)
  )

  mark_var <- "interpolated"

  #  preserve attributes
  source_attributes <- attributes(data)

  data <- .prepare_scdf(data)
  case_names <- names(data)

  if (mark) {
    has_var <- vapply(data, function(x) mark_var %in% names(x), logical(1))
    if (any(has_var)) {
      abort(
        "Variable '", mark_var, "' already exists. ",
        "Rename it before calling fill_missing(mark = TRUE)."
      )
    }
  }

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
      if (mark) dat[[mark_var]] <- FALSE
      data[[i_case]] <- dat
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
    #    present in the data. Phases are deliberately ignored here.
    was_interpolated <- rep(FALSE, nrow(new_dat))

    for (i_col in numeric_vars) {
      before <- is.na(new_dat[[i_col]])
      new_dat[[i_col]] <- .interpolate(
        new_dat[[i_col]], x = new_dat[[mvar]]
      )
      was_interpolated <- was_interpolated |
        (before & !is.na(new_dat[[i_col]]))
    }

    # 6. mark interpolated measurements -------------------------------------
    if (mark) new_dat[[mark_var]] <- was_interpolated

    data[[i_case]] <- new_dat
  }

  # restore attributes
  attributes(data) <- source_attributes

  data
}


# Linear interpolation of missing values.
#
# y            Vector with the values to interpolate.
# x            Vector with the coordinates the values are placed on.
# extrapolate  If TRUE, values before the first and after the last known value
#              are continued linearly. Only meaningful for the measurement-time
#              variable; extrapolating a measured variable would invent data.
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
