#' Single case data frame
#'
#' `scdf()` is the constructor for the `scdf` class. It stores single-case study
#' data with one or more single-cases.
#'
#' @aliases scdf scdf-class as.scdf
#' @param values A vector containing measurement values of the dependent
#'   variable.
#' @param B_start The first measurement of phase B (simple coding if
#'   design is strictly AB).
#' @inheritParams .inheritParams
#' @param mt A vector defining measurement times. Default is `mt =
#'   (1,2,3,...,n)`.
#' @param phase_design A list defining the length and label of
#'   each phase. E.g., `phase_design = c(A1 = 10, B1 = 10, A2 = 10, B2 = 10)`.
#' @param phase_starts A vector defining the label and measurement time of each 
#'  phase start. E.g., `phase_starts = c(A1 = 1, B1 = 6, A2 = 14, B2 = 19)`.
#' @param name A name for the case.
#' @param phase A vector defining phase assignments.
#' @param ...  Additional variables. E.g., `teacher = c(0,1,0,1,0,0,1),
#'   lesson = c(1,3,4,5,2,3)`.
#' @return Returns a single-case data frame `scdf` suitable for all
#'   functions of the `scan` package. Multiple data sets (e.g. from
#'   Multiple Baseline Designs) can be listed.
#' @details If the dependent variable is a named vector then the names are
#'   extracted to create a phase design (e.g., `values = c(A = 2,3,5,4,3, B =
#'   6,5,4,3)` will create an AB phase design with five and four measurements).
#'   An scdf contains several attributes: `dvar` The name of the dependent
#'   variable. `phase` The name of the phase variable. `mt` The name
#'   of the measurement time variable. `author` Information on the author
#'   of the data. `info` Further information on the data. E.g., a
#'   publication. `dvar, phase`, and `mt` are the defaults most of the
#'   `scan` function use. You can change the values of the attributes with
#'   the `scdf_attr` function (e.g., `scdf_attr(exampleAB_add, "dvar")
#'   <- "depression"` defines depression as the dependent variable. Please
#'   notice that all `scan` functions have arguments to define `dvar`,
#'   `phase`, and `mt` for a given analysis.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @examples
#'
#' ## Scores on a letter naming task were collected on eleven days in a row.
#' ## The intervention started after the fifth measurement,
#' ## so the first B phase measurement was 6 (B_start = 6).
#' klaas <- scdf(
#'   c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
#'   B_start = 6, name = "Klaas"
#' )
#' describe(klaas)
#'
#' # Alternative coding 1:
#' klaas <- scdf(
#'   c(A = 5, 7, 8, 5, 7, B = 12, 16, 18, 15, 14, 19),
#'   name = "Klaas"
#' )
#'
#' # Alternative coding 2:
#' klaas <- scdf(
#'   c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
#'   phase_design = c(A = 5, B = 6), name = "Klaas"
#' )
#'
#' # Alternative coding 3:
#' klaas <- scdf(
#'   c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
#'   phase_starts = c(A = 1, B = 7), name = "Klaas"
#' )
#' 
#' ## Unfortunately in a similar study there were no data collected on
#' ## days 3 and 9. Use NA to pass them to the function:
#' emmi <- scdf(c(5, 7, NA, 5, 7, 12, 16, 18, NA, 14, 19),
#'   phase_design = c(A = 5, B = 6), name = "Emmi"
#' )
#' describe(emmi)
#'
#' ## In a MBD over three cases, data were collected eleven days in a row.
#' ## Intervention starting points differ between subjects as they were
#' ## randomly assigned. The three SCDFs are then combined in a list for
#' ## further conjoined analyses.
#' charlotte <- scdf(c(A = 5, 7, 10, 5, 12, B = 7, 10, 18, 15, 14, 19))
#' theresa <- scdf(c(A = 3, 4, 3, 5, B = 7, 4, 7, 9, 8, 10, 12))
#' antonia <- scdf(c(A = 9, 8, 8, 7, 5, 7, B = 6, 14, 15, 12, 16))
#' mbd <- c(charlotte, theresa, antonia)
#' names(mbd) <- c("Charlotte", "Theresa", "Antonia")
#' overlap(mbd)
#'
#' ## In a classroom-based intervention it was not possible to measure outcomes
#' ## every day, but only on schooldays. The sequence of measurements is passed
#' ## to the package by using a vector of measurement times.
#' frida <- scdf(
#'   c(A = 3, 2, 4, 2, 2, 3, 5, 6, B = 8, 10, 8, 12, 14, 13, 12),
#'   mt = c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18)
#' )
#' summary(frida)
#' describe(frida)
#'
#' ## example with two independent variables and four phases
#' jim <- scdf(
#'   zvt = c(47, 58, 76, 63, 71, 59, 64, 69, 72, 77, 76, 73),
#'   d2 = c(131, 134, 141, 141, 140, 140, 138, 140, 141, 140, 138, 140),
#'   phase_design = c(A1 = 3, B1 = 3, A2 = 3, B2 = 3), dvar = "zvt"
#' )
#' overlap(jim, phases = list(c("A1", "A2"), c("B1", "B2")))
#' @export
scdf <- function(values,
                 B_start,
                 mt,
                 phase,
                 phase_design = NULL,
                 phase_starts = NULL,
                 name = NULL,
                 dvar = "values",
                 pvar = "phase",
                 mvar = "mt",
                 ...) {
  
  on.exit(print_messages())
  
  df <- list(...)

  if ("var.values" %in% names(df)) {
    stop("Argument 'var.values' is deprecated. Please use 'dvar' instead.", call. = FALSE)
  }
  if ("phase.design" %in% names(df)) {
    stop("Argument 'phase.design' is deprecated. Please use 'phase_design' instead.", call. = FALSE)
  }
  
  if ("B.start" %in% names(df)) {
    stop("Argument 'B.start' is deprecated. Please use 'B_start' instead.", call. = FALSE)
  }
  
  if (!missing(mt)) df <- c(mt = list(mt), df)
  if (!missing(phase)) df <- c(phase = list(phase), df)
  if (!missing(values)) df <- c(values = list(values), df)

  if (!(dvar %in% names(df))) {
    stop("Dependent variable not defined correctly!", call. = FALSE)
  }

  # create phase_design from a named vector
  if (!is.null(names(df[[dvar]]))) {
    tmp_names <- names(df[[dvar]])
    tmp <- c(which(tmp_names != ""), length(tmp_names) + 1)
    phase_design <- tmp[-1] - tmp[-length(tmp)]
    names(phase_design) <- tmp_names[which(tmp_names != "")]
  }

  # create phase_design from phase variable
  if (!missing(phase)) {
    tmp_phase <- rle(phase)
    phase_design <- tmp_phase$lengths
    names(phase_design) <- tmp_phase$values
  }

  data <- as.data.frame(df)

  ### for backward compatibility
  if (("MT" %in% names(data)) && missing(mt) && mvar == "mt") {
    stop("Please rename argument 'MT' to 'mt'.", call. = FALSE)
  }
  ### END : for backward compatibility

  # create default mt row
  if (!(mvar %in% names(data))) data[, mvar] <- 1:nrow(data)

  # convert B_start phase_design
  if (!missing(B_start)) {
    phase_design <- phase_starts2phase_design(
      list(A = data[[mvar]][1], B = B_start), 
      data[[mvar]]
    )
  }

  if (!is.null(phase_starts)) 
    phase_design <- phase_starts2phase_design(phase_starts, data[[mvar]])
  
  if (is.null(phase_design)) {
    stop("Phase design not defined correctly!", call. = FALSE)
  }

  if (!(mvar %in% names(data))) {
    stop("Measurement-time variable not defined correctly!", call. = FALSE)
  }

  data[, pvar] <- factor(
    rep(names(phase_design), phase_design),
    levels = unique(names(phase_design))
  )

  data <- list(data)

  attributes(data) <- .default_attributes()

  dv(data) <- dvar
  phase(data) <- pvar
  mt(data) <- mvar

  if (!is.null(name)) names(data) <- name
  
  data
}

phase_starts2phase_design <- function(starts, mt) {
  ids <- lapply(starts, \(.) which(. == mt))
  check <- lapply(ids, \(.) {
    if (length(.) == 0) 
      stop("phase_starts not defined correctly. ", 
           "Measurement time does not exist.", call. = FALSE)
  })
  
  if (ids[1] != mt[1]) {
    stop("phase_starts not defined correctly. ", 
         "First phase must start at the first measurement time which is ", 
         mt[1], ".", 
         call. = FALSE)
  }
    
  
  phase_design <- list()
  for (i in 2:length(ids)) {
    phase_design[[names(starts)[i - 1]]] <- ids[[i]] - ids[[i - 1]]
  }
  phase_design[[names(starts)[length(starts)]]] <- length(mt) - sum(unlist(phase_design))
  phase_design
}
