#' Plot single-case data
#' 
#' This function provides a plot of a single-case or multiple
#' single-cases.
#'
#' @inheritParams .inheritParams
#' @param scdf A single-case data frame.
#' @return An scdf plot object that creates a plot when printed.
#' @export

scplot <- function(scdf) {

  warning(.opt$function_experimental_warning)
  
  scdf <- .prepare_scdf(scdf)
  
  xlab <- scdf_attr(scdf, .opt$mt)
  ylab <- scdf_attr(scdf, .opt$dv)
  if (xlab == "mt") xlab <- "Measurement time"
  
  out <- list(
    scdf = scdf,
    dvar = scdf_attr(scdf, .opt$dv), 
    pvar = scdf_attr(scdf, .opt$phase), 
    mvar = scdf_attr(scdf, .opt$mt),
    style = style_plot(getOption("scan.plot.style")),
    title = NULL,
    caption = NULL,
    xaxis = list(lim = NULL, inc = 1),
    yaxis = list(lim = NULL),
    xlabel = xlab,
    ylabel = ylab,
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
set_style <- function(object, style, ...) {
  
  args <- list(...)
  
  if (!missing(style)) {
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
add_statline <- function(object, stat, colour = NULL, ...) {
  lines <- list(stat = stat, col = colour, ...)
  object$lines <- c(object$lines, list(lines))
  object
}

#' @rdname scplot
#' @export
set_xlabel <- function(object, label, colour, size) {
  
  if (!missing(colour)) object$style$col.xlab <- colour
  if (!missing(size)) object$style$cex.xlab <- size
  
  object$xlabel <- label
  
  object
}

#' @rdname scplot
#' @export
set_ylabel <- function(object, label, colour, size, orientation) {
  
  if (!missing(orientation)) object$style$ylab.orientation <- orientation
  if (!missing(colour)) object$style$col.ylab <- colour
  if (!missing(size)) object$style$cex.ylab <- size

  object$ylabel <- label
  
  object
}

#' @rdname scplot
#' @export
set_xaxis <- function(object, limits, increment, colour, size) {
  
  if (!missing(colour)) object$style$col.xaxis <- colour
  if (!missing(size)) object$style$cex.xaxis <- size
  
  if (!missing(limits)) object$xaxis <- c(list(lim = limits), object$xaxis)
  if (!missing(increment)) 
    object$xaxis <- c(list(inc = increment), object$xaxis)
  
  object$xaxis <- object$xaxis[unique(names(object$xaxis))]
  
  object
}

#' @rdname scplot
#' @export
set_yaxis <- function(object, limits, colour, size) {
  
  if (!missing(colour)) object$style$col.yaxis <- colour
  if (!missing(size)) object$style$cex.yaxis <- size
  
  if (!missing(limits)) object$yaxis <- c(list(lim = limits), object$yaxis)
  
  object$yaxis <- object$yaxis[unique(names(object$yaxis))]
  
  object
}


#' @rdname scplot
#' @param label Character string.
#' @export
add_title <- function(object, label, colour, size, font, align, parse) {
  
  if (!missing(colour)) object$style$col.main <- colour
  if (!missing(size)) object$style$cex.main <- size
  if (!missing(font)) object$style$font.main <- font
  if (!missing(align)) object$style$align.main <- align
  if (!missing(parse)) object$style$parse.main <- parse
  
  object$title <- label
  object
}

#' @rdname scplot
#' @param label Character string.
#' @export
add_caption <- function(object, label, colour, size, font, align, wrap, margin, parse) {
  
  if (!missing(colour)) object$style$col.caption <- colour
  if (!missing(size)) object$style$cex.caption <- size
  if (!missing(font)) object$style$font.caption <- font
  if (!missing(align)) object$style$align.caption <- align
  if (!missing(wrap)) object$style$wrap.caption <- wrap
  if (!missing(margin)) object$style$margin.caption <- margin
  if (!missing(parse)) object$style$parse.caption <- parse
  object$caption <- label
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
add_marks <- function(object, case, positions, colour = "red", size = 1, shape = 1) {
  
  # Marks on the outliers from outlier()
  if (identical(class(positions), c("sc","outlier"))) {
    for(i in seq_along(positions$dropped.mt))
      object$marks <- c(
        object$marks, 
        list(
          list(case = i, positions = positions$dropped.mt[[i]], 
               col = colour, cex = size, pch = shape)
        )
      )
    return(object)
  }
  
  for(i in case) {
    object$marks <- c(
      object$marks, 
      list(
        list(case = i, positions = positions, col = colour, cex = size, pch = shape)
      )
    )
  }
  
  
  object
}

#' @rdname scplot
#' @export
set_phasenames <- function(object, ..., colour, size) {
  
  if (!missing(colour)) object$style$col.phasenames <- colour
  if (!missing(size)) object$style$cex.phasenames <- size
  object$phase_names$labels <- c(...)
  object
}

#' @rdname scplot
#' @export
set_casenames <- function(object, ..., colour, side, size) {
  
  if (!missing(colour)) object$style$col.casenames <- colour
  if (!missing(side)) object$style$names$side <- side
  if (!missing(size)) object$style$cex.casenames <- size
  
  
  object$case_names$labels <- c(...)
  object
}

#' @rdname scplot
#' @param x x position
#' @param y y position
#' @export
add_text <- function(object, case = 1, x, y, label, colour = NULL, size = NULL, angle = 0) {
  
  text <- list(case = case, labels = label, x = x, y = y, col = colour, cex = size, angle = angle)
  object$texts <- c(object$texts, list(text))
  object
}

#' @rdname scplot
#' @export
add_arrow <- function(object, case = 1, x0, y0, x1, y1, length = 0.1, colour = NULL, ...) {
  arrow <- list(
    case = case, 
    x0 = x0, 
    y0 = y0, 
    x1 = x1, 
    y1 = y1, 
    length = length, 
    col= colour,
    ...
  )
  object$arrows <- c(object$arrows, list(arrow))
  object
}

#' @rdname scplot
#' @export
add_grid <- function(object, type, width, colour) {

  object$style$grid <- TRUE
  
  if (!missing(colour)) object$style$col.grid <- colour
  if (!missing(width)) object$style$lwd.grid <- width
  if (!missing(type)) object$style$lty.grid <- type

  object
}

#' @rdname scplot
#' @export
set_background <- function(object, colour) {
  
  object$style$fill.bg <- TRUE
  
  if (!missing(colour)) object$style$col.fill.bg <- colour
  
  object
}

#' @rdname scplot
#' @export
add_frame <- function(object, colour = "black") {
  
  object$style$col.frame <- colour
  
  object
}

#' @rdname scplot
#' @export
add_box <- function(object, colour = "black", type = "solid", width = 1) {
  
  object$style$col.box <- colour
  object$style$lty.box <- type
  object$style$lwd.box <- width
  
  object
}

#' @rdname scplot
#' @export
add_labels <- function(object, 
                       colour = "black", 
                       size = 0.6, 
                       offset = 0.4, 
                       position = 3, 
                       round = 1) {
  
  object$style$annotations <- list(
    col = colour, cex = size, offset = offset, round = round, pos = position
  )
  object
}

#' @rdname scplot
#' @export
set_line <- function(object, colour, width, type) {
  
  if (!missing(colour)) object$style$col.lines <- colour
  if (!missing(width)) object$style$lwd <- width
  if (!missing(type)) object$style$lty <- type
  
  object
}

#' @rdname scplot
#' @export
set_dots <- function(object, colour, size, shape) {
  
  if (!missing(shape)) object$style$pch <- shape
  if (!missing(colour)) object$style$col.dots <- colour
  if (!missing(size)) object$style$cex.dots <- size
  
  object
}

#' @rdname scplot
#' @export
add_ridge <- function(object, colour = "grey98") {
  
  object$style$col.ridge <- colour
  object
}

#' @rdname scplot
#' @export
set_seperator <- function(object, colour, width, type, extent) {
  
  if (!missing(colour)) object$style$col.seperators <- colour
  if (!missing(width)) object$style$lwd.seperators <- width
  if (!missing(type)) object$style$lty.seperators <- type
  if (!missing(extent)) object$style$seperators.extent <- extent
  
  object
}


.check_style <- function(style) {
  
  if (is.null(style$cex.xlab)) style$cex.xlab <- style$cex.lab
  if (is.null(style$cex.ylab)) style$cex.ylab <- style$cex.lab
  
  if (is.null(style$col.xlab)) style$col.xlab <- style$col.text
  if (is.null(style$col.ylab)) style$col.ylab <- style$col.text
  
  if (is.null(style$cex.main)) style$cex.main <- 1
  if (is.null(style$col.main)) style$col.main <- style$col.text
  
  if (!is.null(style$annotations)) {
    if(is.null(style$annotations$round)) style$annotations$round <- 1
    if(is.null(style$annotations$offset)) style$annotations$offset <- 0.5
  }
  
  style
}

