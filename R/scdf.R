methods::setOldClass(c("scdf", "list"))

#' Single case data frame
#'
#' The class \code{scdf} stores single-case study data with one or more
#' single-cases.
#'
#' The \code{scdf} class is a wrapper for a list containing a dataframe for
#' each case.
#'
#' @aliases scdf scdf-class as.scdf checkSCDF
#' makeSCDF
#' @param values A vector containing measurement values of the dependent variable.
#' @param B.start The first measurement of phase B (simple coding if design is
#' strictly AB).
#' @inheritParams .inheritParams
#' @param mt A vector defining measurement times. Default is \code{mt =
#' (1,2,3,...,n)}.
#' @param phase.design A vector defining the length and label of each phase.
#' E.g., \code{phase.length = c(A1 = 10, B1 = 10, A2 = 10, B2 = 10)}.
#' @param name A name for the case.
#' @param phase A vector defining phase assignments.
#' @param ...  Additional variables. E.g., \code{teacher = c(0,1,0,1,0,0,1),
#' lesson = c(1,3,4,5,2,3)}.
#' @return Returns a single-case data frame \code{scdf} suitable for all
#' functions of the \code{scan} package. Multiple data sets (e.g. from Multiple
#' Baseline Designs) can be listed.
#' @details If the dependent variable is a named vector then the names are extracted
#' to create a phase design (e.g., values = c(A = 2,3,5,4,3, B = 6,5,4,3) will create
#' an AB phase design with five and four measuresments).
#' An scdf contains several attributes:
#'  \code{dvar} The name of the dependent variable.
#'  \code{phase} The name of the phase variable.
#'  \code{mt} The name of the measurement time variable.
#'  \code{author} Information on the author of the data.
#'  \code{info} Further information on the data. E.g., a publication.
#'  \code{dvar, phase, and mt} are the defaults most of the \code{scan} function
#'  use. You can change the values of the attributes with the \code{scdf_attr} function
#'  (e.g., \code{scdf_attr(exampleAB_add, "dvar") <- "depression"} defines depression
#'  as the dependent variable. Please notice that all \code{scan} functions
#'  have arguments to define \code{dvar, phase, and mt} for a given analysis.
#' @author Juergen Wilbert
#' @family data manipulation function
#' @examples
#'
#' ## Scores on a letter naming task were collected on eleven days in a row. The intervention
#' ## started after the fifth measurement, so the first B phase measurement was 6 (B.start = 6).
#' klaas <- scdf(
#'   c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
#'   B.start = 6, name = "Klaas"
#' )
#' plot(klaas)
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
#'   phase.design = c(A = 5, B = 6), name = "Klaas"
#' )
#'
#' ## Unfortunately in a similar SCDR there were no data collected on days 3 and 9. Use NA to
#' ## pass them to the package.
#' emmi <- scdf(c(5, 7, NA, 5, 7, 12, 16, 18, NA, 14, 19),
#'   phase.design = c(A = 5, B = 6), name = "Emmi"
#' )
#' describeSC(emmi)
#'
#' ## In a MBD over three persons, data were again collected eleven days in a row. Intervention
#' ## starting points differ between subjects as they were randomly assigned. The three SCDFs
#' ## are then combined in a list for further conjoined analyses.
#' charlotte <- scdf(c(A = 5, 7, 10, 5, 12, B = 7, 10, 18, 15, 14, 19))
#' theresa <- scdf(c(A = 3, 4, 3, 5, B = 7, 4, 7, 9, 8, 10, 12))
#' antonia <- scdf(c(A = 9, 8, 8, 7, 5, 7, B = 6, 14, 15, 12, 16))
#' mbd <- c(charlotte, theresa, antonia)
#' names(mbd) <- c("Charlotte", "Theresa", "Antonia")
#' overlapSC(mbd)
#'
#' ## In a classroom-based intervention it was not possible to measure outcomes every day, but
#' ## only on schooldays. The sequence of measurements is passed to the package by using a
#' ## vector of measurement times.
#' frida <- scdf(
#'   c(A = 3, 2, 4, 2, 2, 3, 5, 6, B = 8, 10, 8, 12, 14, 13, 12),
#'   mt = c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18)
#' )
#' summary(frida)
#' plot(frida)
#' describeSC(frida)
#'
#' ## example with two independent variables and four phases
#' jim <- scdf(
#'   zvt = c(47, 58, 76, 63, 71, 59, 64, 69, 72, 77, 76, 73),
#'   d2 = c(131, 134, 141, 141, 140, 140, 138, 140, 141, 140, 138, 140),
#'   phase.design = c(A1 = 3, B1 = 3, A2 = 3, B2 = 3), dvar = "zvt"
#' )
#' overlapSC(jim, phases = list(c("A1", "A2"), c("B1", "B2")))
#' @export
scdf <- function(values, B.start, mt, phase, phase.design, name, dvar = "values", pvar = "phase", mvar = "mt", ...) {
  df <- list(...)

  if ("var.values" %in% names(df)) {
    stop("'var.values' is deprecated. Please use 'dvar' instead.")
  }
  if (!missing(mt)) df <- c(mt = list(mt), df)
  if (!missing(phase)) df <- c(phase = list(phase), df)
  if (!missing(values)) df <- c(values = list(values), df)

  if (!(dvar %in% names(df))) {
    stop("Independent variable not defined correctly!")
  }

  # create phase.design from a named vector
  if (!is.null(names(df[[dvar]]))) {
    tmp.names <- names(df[[dvar]])
    tmp <- c(which(tmp.names != ""), length(tmp.names) + 1)
    phase.design <- tmp[-1] - tmp[-length(tmp)]
    names(phase.design) <- tmp.names[which(tmp.names != "")]
  }

  # create phase.design from phase variable
  if (!missing(phase)) {
    tmp.phase <- rle(phase)
    phase.design <- tmp.phase$lengths
    names(phase.design) <- tmp.phase$values
  }

  data <- as.data.frame(df)

  ### for backward campatibility
  if (("MT" %in% names(data)) && missing(mt) && mvar == "mt") {
    warning("Please rename argument 'MT' to 'mt'.")
    mvar <- "MT"
  }
  ### END : for backward campatibility

  # create default mt row
  if (!(mvar %in% names(data))) data[, mvar] <- 1:nrow(data)

  if (!missing(B.start)) {
    B.start <- match(B.start, data[, mvar])
    if (is.na(B.start)) {
      stop("No values provided at the measurement.time of B.start in var '", mvar, "'.")
    }
    phase.design <- c("A" = B.start - 1, "B" = nrow(data) - B.start + 1)
  }

  if (missing(phase.design)) {
    stop("Phase design not defined correctly!")
  }

  if (!(mvar %in% names(data))) {
    stop("Measurement-time variable not defined correctly!")
  }

  data[, pvar] <- factor(rep(names(phase.design), phase.design), levels = unique(names(phase.design)))

  data <- list(data)

  attributes(data) <- .defaultAttributesSCDF()

  scdf_attr(data, .opt$dv) <- dvar
  scdf_attr(data, .opt$phase) <- pvar
  scdf_attr(data, .opt$mt) <- mvar

  if (!missing(name)) names(data) <- name

  data
}