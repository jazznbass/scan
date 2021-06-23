#' Plot single-case data
#' 
#' This function provides a plot of a single-case or multiple
#' single-cases.
#'
#' @inheritParams .inheritParams
#'
#' @return An scdf plot object that creates aplot when printed.
#' @export

scplot <- function(data, dvar, pvar, mvar) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
  
  data <- .prepare_scdf(data)
  
  xlab <- mvar
  ylab <- dvar
  if (xlab == "mt") xlab <- "Measurement time"
  
  out <- list(
    scdf = data,
    dvar = dvar, 
    pvar = pvar, 
    mvar = mvar,
    style = style_plot(getOption("scan.plot.style")),
    title = "",
    xaxis = list(lim = NULL, increase = 1, label = xlab),
    yaxis = list(lim = NULL, label = ylab),
    lines = NULL, 
    marks = NULL, 
    texts = NULL,
    arrows = NULL,
    phase_names = NULL,
    case_names = list(labels = .case_names(names(data), length(data)))
  )
  class(out) <- "scplot"
  out
}



#' @rdname scplot
#' @export
set_style <- function(object, style = NULL, ...) {
  
  args <- list(...)
  
  if (!is.null(style)) {
      if (!(all(style %in% names(.opt$style)))) {
        stop("Unknown style template.")
      }
    
    cum_style <- list()
    for(i in rev(style)) cum_style <- c(cum_style, .opt$style[[i]])
    style <- cum_style
  }  
  
  out <- c(args, style, object$style)
  
  out <- out[unique(names(out))]
  
  object$style <- out
  object
}

#' @rdname scplot
#' @param lines A list defining one or multiple lines or curves to be
#' plotted. The argument is passed as a list (e.g., \code{list(type = "median")}).
#' Some of the procedures can be refined with an additional argument (e.g.,
#' \code{lines = list(type = "mean", trim = 0.2)} adds a 20\% trimmed mean line.
#' For multiple lines, provide a list element for each line (e.g., \code{list( list(type = "median", col = "red"), list(type = "trend", col = "blue"))}. 
#' Possible lines are: \itemize{
#' \item\code{"median"} Separate lines for phase A and B medians.
#' \item\code{"mean"} Separate lines for phase A and B means. By default it is
#' 10\%-trimmed. Other trims can be set, using a trim parameter (e.g.,
#' \code{lines = list(type = "mean", trim = 0.2)} draws a 20\%-trimmed mean line).
#' \item\code{"trend"} Separate lines for phase A and B trends.
#' \item\code{"trendA"} OLS trend line for phase A, extrapolated throughout phase B.
#' \item\code{"trendA_bisplit"} Split middle (bi-split) trend line for phase A, extrapolated throughout phase B.
#' \item\code{"trendA_trisplit"} Tukey tri-split trend line for phase A, extrapolated throughout phase B.
#' \item\code{"maxA/minA"} Line at the level of the highest or lowest phase A score.
#' \item\code{"medianA"} Line at the phase A median score.  \item\code{"meanA"}
#' Line at the phase A 10\%-trimmed mean score. Apply a different trim, by
#' using the additional argument (e.g., \code{lines = list(type = "meanA", trim = 0.2)}).
#' \item\code{"plm"} Regression lines for piecewise linear regression model.
#' \item\code{"plm.ar"} Regression lines for piecewise autoregression model.
#' The lag is specified like this: \code{lines = list(type = "plm.ar", ar = 2)}. Default lag is set to 2.
#' \item\code{"movingMean"} Draws a moving mean curve, with a specified lag:
#' \code{lines = list(type = "movingMean", lag = 2)}. Default is a lag 1 curve.
#' \item\code{"movingMedian"} Draws a moving median curve, with a specified
#' lag: \code{lines = list(type = "movingMedian", lag = 3)}. Default is a lag 1 curve.
#' \item\code{"loreg"} Draws a non-parametric local regression line. The
#' proportion of data influencing each data point can be specified using
#' \code{lines = list(type = "loreg"m f = 0.66)}. The default is 0.5.  \item\code{"lty"}
#' Use this argument to define the line type. Examples are: \code{"solid"},
#' \code{"dashed"}, \code{"dotted"}.  \item\code{"lwd"} Use this argument to
#' define the line's thickness, e.g., \code{lwd = 4}.  \item\code{"col"} Use
#' this argument to define the line's color, e.g., \code{col = "red"}.  }
#' @export
add_line <- function(object, type = NA, ...) {
  if (is.na(type)) stop("A linetype must be specified.")
  lines <- list(type = type, ...)
  object$lines <- c(object$lines, list(lines))
  object
}

#' @rdname scplot
#' @export
set_xaxis <- function(object, label, lim, increase, col = NULL, cex = NULL) {
  
  if (!is.null(col)) object$style$col.xlab <- col
  if (!is.null(cex)) object$style$cex.xlab <- cex
  
  if (!missing(label)) object$xaxis <- c(list(label = label), object$xaxis)
  if (!missing(lim)) object$xaxis <- c(list(lim = lim), object$xaxis)
  if (!missing(increase)) object$xaxis <- c(list(increase = increase), object$xaxis)
  
  object$xaxis <- object$xaxis[unique(names(object$xaxis))]
  
  object
}

#' @rdname scplot
#' @export
set_yaxis <- function(object, label, lim, col = NULL, cex = NULL, orientation = NULL) {
  
  if (!is.null(orientation)) object$style$ylab.orientation <- orientation
  if (!is.null(col)) object$style$col.ylab <- col
  if (!is.null(cex)) object$style$cex.ylab <- cex
  
  if (!missing(label)) object$yaxis <- c(list(label = label), object$yaxis)
  if (!missing(lim)) object$yaxis <- c(list(lim = lim), object$yaxis)

  object$yaxis <- object$yaxis[unique(names(object$yaxis))]
  
  object
}

#' @rdname scplot
#' @param label Character string.
#' @export
add_title <- function(object, label = "", col = NULL, cex = NULL, font = NULL) {
  
  if (!is.null(col)) object$style$col.main <- col
  if (!is.null(cex)) object$style$cex.main <- cex
  if (!is.null(font)) object$style$font.main <- font
  
  object$title <- label
  object
}

#' @rdname scplot
#' @param marks A list of parameters defining markings of certain data points.
#' \itemize{ \item\code{"positions"} A vector or a list of vectors indicating
#' measurement-times to be highlighted. In case of a vector, the marked
#' measurement-times are the same for all plotted cases. In case of a list of
#' vectors, marks are set differently for each case. The list must have the
#' same length as there are cases in the data file.  \item\code{"col"} Color of
#' the marks.  \item\code{"cex"} Size of the marks.  } Use for example
#' \code{marks = list(positions = c(1, 8, 15), col = "red", cex = 3)} to make
#' the MTs one, eight and 18 appear big and red.
#' @export
add_marks <- function(object, case  = NULL, positions = NULL, col = "red", cex = 1, pch = 1) {
  
  # Marks on the outliers from outlier()
  if (identical(class(positions), c("sc","outlier"))) {
    for(i in seq_along(positions$dropped.mt))
      object$marks <- c(
        object$marks, 
        list(
          list(case = i, positions = positions$dropped.mt[[i]], 
               col = col, cex = cex, pch = pch)
        )
      )
    return(object)
  }
  
  object$marks <- c(
    object$marks, 
    list(
      list(case = case, positions = positions, col = col, cex = cex, pch = pch)
    )
  )
  object
}

#' @rdname scplot
#' @export
set_phasenames <- function(object, ..., col = NULL, cex = NULL) {
  
  if (!is.null(col)) object$style$col.phasenames <- col
  if (!is.null(cex)) object$style$cex.phasenames <- cex
  object$phase_names$labels <- c(...)
  object
}

#' @rdname scplot
#' @export
set_casenames <- function(object, ..., col = NULL, side = NULL, cex = NULL) {
  
  if (!is.null(col)) object$style$col.casenames <- col
  if (!is.null(side)) object$style$names$side <- side
  if (!is.null(cex)) object$style$cex.casenames <- cex
  
  
  object$case_names$labels <- c(...)
  object
}

#' @rdname scplot
#' @param x x position
#' @param y y position
#' @export
add_text <- function(object, case = 1, x, y, label, ...) {
  dots <- list(...)
  text <- c(list(case = case, labels = label, x = x, y = y), dots)
  object$texts <- c(object$texts, list(text))
  object
}

#' @rdname scplot
#' @export
add_arrow <- function(object, case = 1, x0, y0, x1, y1, length = 0.1, ...) {
  arrow <- list(case = case, x0 = x0, y0 = y0, x1 = x1, y1 = y1, length = length, ...)
  object$arrows <- c(object$arrows, list(arrow))
  object
}

.check_style <- function(style) {
  
  if (is.null(style$cex.xlab)) style$cex.xlab <- style$cex.lab
  if (is.null(style$cex.ylab)) style$cex.ylab <- style$cex.lab
  
  if (is.null(style$col.xlab)) style$col.xlab <- style$col.text
  if (is.null(style$col.ylab)) style$col.ylab <- style$col.text
  
  if (is.null(style$cex.main)) style$cex.main <- 1
  if (is.null(style$col.main)) style$col.main <- style$col.text
  
  style
}

