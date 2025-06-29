#' Single case data frame
#'
#' `scdf()` is the constructor for objects of class `scdf`. It stores data from
#' single-case studies with one or more cases in a structured format suitable
#' for analysis with the `scan` package.
#'
#' @aliases scdf scdf-class as.scdf
#' @inheritParams .inheritParams
#'
#' @param ... One or more vectors representing measurement variables. See the
#'   *Details* section.
#' @param B_start The first measurement point of phase B (simple coding; only
#'   applicable if the design follows a strict AB pattern).
#' @param phase_design A named vector defining the length and label of each
#'   phase. For example: `phase_design = c(A1 = 10, B1 = 10, A2 = 10, B2 = 10)`.
#' @param phase_starts A named vector defining the label and measurement time of
#'   each phase start. For example: `phase_starts = c(A1 = 1, B1 = 6, A2 = 14,
#'   B2 = 19)`.
#' @param name Optional name for the case.
#'
#' @return Returns a single-case data frame `scdf` suitable for all functions in
#'   the `scan` package.
#'
#' @details If no variable matching the name of the dependent variable is
#'   provided (the default name is `values`, which can be changed via the `dvar`
#'   argument), and the first provided variable is unnamed, that variable will
#'   be interpreted as the dependent variable.
#'
#'   If no measurement-time variable is provided (default name `mt`,
#'   configurable via the `mvar` argument), measurement times are automatically
#'   defined as a sequence `(1, 2, 3, ..., n)`.
#'
#'   If the dependent variable is a **named vector**, the names will be used to
#'   define a phase design. For example, `values = c(A = 2, 3, 5, 4, 3, B = 6,
#'   5, 4, 3)` will be interpreted as an AB phase design with five measurements
#'   in phase A and four in phase B.
#'
#'   If a vector matching the name of the phase variable is provided, it will be
#'   used to define the phase design directly.
#'
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @examples
#' ## Scores on a letter naming task were collected on eleven days in a row.
#' ## The intervention started after the fifth measurement,
#' ## so the first B phase measurement was 6 (B_start = 6).
#' klaas <- scdf(
#'   c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
#'   B_start = 6, name = "Klaas"
#' )
#' describe(klaas)
#'
#' # Alternative: using named vector
#' klaas <- scdf(
#'   c(A = 5, 7, 8, 5, 7, B = 12, 16, 18, 15, 14, 19),
#'   name = "Klaas"
#' )
#'
#' # Alternative: using phase_design
#' klaas <- scdf(
#'   c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
#'   phase_design = c(A = 5, B = 6), name = "Klaas"
#' )
#'
#' # Alternative: using phase_starts
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
#' tonia <- scdf(c(A = 9, 8, 8, 7, 5, 7, B = 6, 14, 15, 12, 16))
#' mbd <- c(charlotte, theresa, tonia)
#' names(mbd) <- c("Charlotte", "Theresa", "Tonia")
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
#'
#' @export
scdf <- function(...,
                 B_start = NULL,
                 phase_starts = NULL,
                 phase_design = NULL,
                 name = NULL,
                 dvar = "values",
                 pvar = "phase",
                 mvar = "mt") {
  
  on.exit(print_messages())
  
  # Catch ellipse ----
  
  df <- list(...)
  
  ## set first entry to dvar if unnamed and dvar not within df 
  if (is.null(names(df)[1]) || identical(names(df)[1], "")) {
    if (!(dvar %in% names(df))) names(df)[1] <- dvar
  }
  
  # check arguments and df ----

  check_args(
    has_length(pvar, 1),
    has_length(mvar, 1),
    not("var.values" %in% names(df), "'var.values' is deprecated. Use 'dvar'."),
    not(
      "phase.design" %in% names(df), 
      "'phase.design' is deprecated. Use 'phase_design'."
    ),
    not("B.start" %in% names(df), "'B.start' is deprecated. Use 'B_start'."),
    not(("MT" %in% names(df)) && mvar == "mt", "Rename argument 'MT' to 'mt'."),
    is_true(
      all(dvar %in% names(df)), 
      "Dependent variable ", dvar, " not found in scdf definition."
    ),
    is_true(
      all(lengths(df) == lengths(df)[1]), 
      "All variables must have the same length. \nActual lengths are: ", 
      paste0(names(lengths(df)), " = ", lengths(df), collapse = ", "), "."
    )
  )
  
  # create default mt column ----
  if (!(mvar %in% names(df))) df[[mvar]] <- 1:length(df[[dvar]])
  
  # create phase_design ----
  
  ## from a named vector
  if (!is.null(names(df[[dvar]]))) {
    tmp_names <- names(df[[dvar]])
    tmp <- c(which(tmp_names != ""), length(tmp_names) + 1)
    phase_design <- tmp[-1] - tmp[-length(tmp)]
    names(phase_design) <- tmp_names[which(tmp_names != "")]
  }

  ## from phase variable
  if (!is.null(df[[pvar]])) {
    tmp_phase <- rle(df[[pvar]])
    phase_design <- tmp_phase$lengths
    names(phase_design) <- tmp_phase$values
  }
  
  # from B_start
  if (!is.null(B_start)) {
    phase_design <- phase_starts2phase_design(
      list(A = df[[mvar]][1], B = B_start), 
      df[[mvar]]
    )
  }
  
  # from phase_starts
  if (!is.null(phase_starts)) 
    phase_design <- phase_starts2phase_design(phase_starts, df[[mvar]])
  
  if (is.null(phase_design)) {
    stop("Phase design not defined correctly!", call. = FALSE)
  }
  
  df[[pvar]] <- factor(
    rep(names(phase_design), phase_design),
    levels = unique(names(phase_design))
  )
  
  # return ----
  
  data <- list(as.data.frame(df))
  attributes(data) <- .default_attributes()
  dv(data) <- dvar
  phase(data) <- pvar
  mt(data) <- mvar
  if (!is.null(name)) names(data) <- name

  data
}

phase_starts2phase_design <- function(starts, mt) {
  ids <- lapply(starts, function(x) which(x == mt))
  check <- lapply(ids, function(x) {
    if (length(x) == 0) 
      stop("phase_starts not defined correctly. ", 
           "Measurement time does not exist.", call. = FALSE)
  })

  if (ids[1] != 1) {
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
