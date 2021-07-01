#' Plot single-case data
#' 
#' This function provides a plot of a single-case or multiple
#' single-cases.
#'
#' @inheritParams .inheritParams
#' @return An scdf plot object that creates a plot when printed.
#' @export

scplot <- function(data) {

  warning(.opt$function_experimental_warning)
  
  data <- .prepare_scdf(data)
  
  xlab <- scdf_attr(data, .opt$mt)
  ylab <- scdf_attr(data, .opt$dv)
  if (xlab == "mt") xlab <- "Measurement time"
  
  theme <- modifyList(
    .opt$scplot_themes$default, 
    .opt$scplot_themes[[getOption("scan.scplot.theme")]], 
    keep.null = TRUE
  )
  
  out <- list(
    scdf = data,
    dvar = scdf_attr(data, .opt$dv), 
    pvar = scdf_attr(data, .opt$phase), 
    mvar = scdf_attr(data, .opt$mt),
    theme = theme,
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
#' @param theme A character string with a predefined graphical theme. 
#' Possible values: default, yaxis, tiny, small, big, chart, ridge, 
#' annotate, grid, grid2, dark, nodots, sienna, phase_color, phase_shade
#' 
#' @export
add_theme <- function(object, theme, ...) {
  
  args <- list(...)
  new_theme <- object$theme
  
  if (!missing(theme)) {
    if (!(all(theme %in% names(.opt$scplot_themes)))) {
      stop("Unknown theme template.")
    }
  
    for(i in rev(theme)) {
      new_theme <- modifyList(new_theme, .opt$scplot_themes[[i]], keep.null = TRUE) 
    }
  }  
  
  new_theme <- modifyList(new_theme, args, keep.null = TRUE)
  
  object$theme <- new_theme
  object
}

#' @rdname scplot
#' @param stat A character string defining a line or curve to be
#' plotted. Possible values: "median", "mean", "trend", "trendA", 
#' "trendA_bisplit", "trendA_trisplit", "maxA", "minA", "meanA", "medianA", 
#' "plm", "movingMean", "movingMedian", "loreg"
#' @param width A number defining the line width
#' @param colour A character string or a number defining the color of an element.
#' @param size A number deifning the size of an element.
#' @param type A character string with the line type: "solid", "dashed", "dotted"
#' @export
add_statline <- function(object, stat, colour = NULL, width = NULL, type = NULL, ...) {
  lines <- list(stat = stat, col = colour, lwd = width, lty = type, ...)
  object$lines <- c(object$lines, list(lines))
  object
}

#' @rdname scplot
#' @export
set_xlabel <- function(object, label, colour, size) {
  
  if (!missing(colour)) object$theme$col.xlab <- colour
  if (!missing(size)) object$theme$cex.xlab <- size
  
  object$xlabel <- label
  
  object
}

#' @rdname scplot
#' @param orientation of the label: 0 = vertical; 1 = horizontal
#' @export
set_ylabel <- function(object, label, colour, size, line, orientation) {
  
  if (!missing(orientation)) object$theme$ylab.orientation <- orientation
  if (!missing(colour)) object$theme$col.ylab <- colour
  if (!missing(size)) object$theme$cex.ylab <- size
  if (!missing(line)) object$theme$vjust.ylab <- line
  

  object$ylabel <- label
  
  object
}

#' @rdname scplot
#' @param limits Lower and upper limits of the axis (e.g., \code{limits = c(0,
#' 20)} sets the axis to a scale from 0 to 20). With multiple single-cases
#' you can use \code{limits = c(0, NA)} to scale the axis from 0 to the maximum
#' of each case. \code{limits} is not set by default, which makes \code{scan} set
#' a proper scale based on the given data.
#' @param increment An integer. Increment of the x-axis. 1 :each mt value will be printed, 2 : every other value, 3 : every third values etc.
#' @export
set_xaxis <- function(object, limits, increment, colour, size, line) {
  
  if (!missing(colour)) object$theme$col.xaxis <- colour
  if (!missing(size)) object$theme$cex.xaxis <- size
  if (!missing(line)) object$theme$vjust.xlab <- line
  if (!missing(limits)) object$xaxis <- c(list(lim = limits), object$xaxis)
  if (!missing(increment)) 
    object$xaxis <- c(list(inc = increment), object$xaxis)
  
  object$xaxis <- object$xaxis[unique(names(object$xaxis))]
  
  object
}

#' @rdname scplot
#' @export
set_yaxis <- function(object, limits, colour, size) {
  
  if (!missing(colour)) object$theme$col.yaxis <- colour
  if (!missing(size)) object$theme$cex.yaxis <- size
  
  if (!missing(limits)) object$yaxis <- c(list(lim = limits), object$yaxis)
  
  object$yaxis <- object$yaxis[unique(names(object$yaxis))]
  
  object
}


#' @rdname scplot
#' @param label Character string.
#' @param align Character string. One of "left", "right", "center"
#' @param wrap Number that defines the maximum characters per line before a break. 
#' If set to FALSE, no automatic linebreak is set.
#' @param parse If TRUE, the label is interpreted as an expression. Default = FALSE.
#' @export
add_title <- function(object, label, colour, size, font, align, parse) {
  
  if (!missing(colour)) object$theme$col.main <- colour
  if (!missing(size)) object$theme$cex.main <- size
  if (!missing(font)) object$theme$font.main <- font
  if (!missing(align)) object$theme$align.main <- align
  if (!missing(parse)) object$theme$parse.main <- parse
  
  object$title <- label
  object
}

#' @rdname scplot
#' @param label Character string.
#' @export
add_caption <- function(object, label, colour, size, font, align, wrap, margin, 
                        parse) {
  
  if (!missing(colour)) object$theme$col.caption <- colour
  if (!missing(size)) object$theme$cex.caption <- size
  if (!missing(font)) object$theme$font.caption <- font
  if (!missing(align)) object$theme$align.caption <- align
  if (!missing(wrap)) object$theme$wrap.caption <- wrap
  if (!missing(margin)) object$theme$margin.caption <- margin
  if (!missing(parse)) object$theme$parse.caption <- parse
  object$caption <- label
  object
}

#' @rdname scplot
#' @param positions Either a vector indicating the dot to be highlighted or a 
#' character string with a logical expression (e.g. values < mean(values))
#' @param shape Number. See pch graphical parameter on par help page.  
#' @export
add_marks <- function(object, case, positions, 
                      colour = "red", size = 1, shape = 1) {
  
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
  
  if (!missing(colour)) object$theme$col.phasenames <- colour
  if (!missing(size)) object$theme$cex.phasenames <- size
  object$phase_names$labels <- c(...)
  object
}

#' @rdname scplot
#' @export
set_casenames <- function(object, ..., colour, side, size) {
  
  if (!missing(colour)) object$theme$col.casenames <- colour
  if (!missing(side)) object$theme$names$side <- side
  if (!missing(size)) object$theme$cex.casenames <- size
  
  
  object$case_names$labels <- c(...)
  object
}

#' @rdname scplot
#' @param x x position
#' @param y y position
#' @export
add_text <- function(object, case = 1, x, y, label, colour = NULL, size = NULL, 
                     angle = 0) {
  
  text <- list(case = case, labels = label, x = x, y = y, col = colour, cex = size, angle = angle)
  object$texts <- c(object$texts, list(text))
  object
}

#' @rdname scplot
#' @param case Numerical vector with the csae number or character string "all" for all cases.
#' @param x0 Origin x position of the line.
#' @param y0 Origin y position of the line.
#' @param x1 End x position of the line.
#' @param y1 End y position of the line.
#' @param length Size of the aroow angels.
#' @export
add_arrow <- function(object, case = 1, x0, y0, x1, y1, length = 0.1, 
                      colour = NULL, ...) {
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

  object$theme$grid <- TRUE
  
  if (!missing(colour)) object$theme$col.grid <- colour
  if (!missing(width)) object$theme$lwd.grid <- width
  if (!missing(type)) object$theme$lty.grid <- type

  object
}

#' @rdname scplot
#' @export
set_background <- function(object, colour) {
  
  object$theme$fill.bg <- TRUE
  
  if (!missing(colour)) object$theme$col.fill.bg <- colour
  
  object
}

#' @rdname scplot
#' @export
add_frame <- function(object, colour = "black", size = 1, type = "solid") {
  
  object$theme$col.frame <- colour
  object$theme$lty.frame <- type
  object$theme$lwd.frame <- size
  
  object
}

#' @rdname scplot
#' @export
add_box <- function(object, colour = "black", type = "solid", width = 1) {
  
  object$theme$col.box <- colour
  object$theme$lty.box <- type
  object$theme$lwd.box <- width
  
  object
}

#' @rdname scplot
#' @param offset Number with shift from exact position of the label.
#' @param round Number of digits of the labels.
#' @param position Number 1, 2, 3,or 4, indicate positions below, to the left of, above and to the right of the exact value coordinate. 
#' @export
add_labels <- function(object, 
                       colour = "black", 
                       size = 0.6, 
                       offset = 0.4, 
                       position = 3, 
                       round = 1) {
  
  object$theme$annotations <- list(
    col = colour, cex = size, offset = offset, round = round, pos = position
  )
  object
}

#' @rdname scplot
#' @export
set_line <- function(object, colour, width, type) {
  
  if (!missing(colour)) object$theme$col.line <- colour
  if (!missing(width)) object$theme$lwd.line <- width
  if (!missing(type)) object$theme$lty.line <- type
  
  object
}

#' @rdname scplot
#' @export
set_dots <- function(object, colour, size, shape) {
  
  if (!missing(shape)) object$theme$pch <- shape
  if (!missing(colour)) object$theme$col.dots <- colour
  if (!missing(size)) object$theme$cex.dots <- size
  
  object
}

#' @rdname scplot
#' @export
add_ridge <- function(object, colour = "grey98") {
  
  object$theme$col.ridge <- colour
  object
}

#' @rdname scplot
#' @param extent A number between 0 and 1 given the proportion of the plot that is covert by the line or character string "full" or "scale".
#' @export
set_seperator <- function(object, colour, width, type, extent, label, size) {
  
  if (!missing(size)) object$theme$size.seperators <- size
  if (!missing(label)) object$label.seperators <- label
  if (!missing(colour)) object$theme$col.seperators <- colour
  if (!missing(width)) object$theme$lwd.seperators <- width
  if (!missing(type)) object$theme$lty.seperators <- type
  if (!missing(extent)) object$theme$extent.seperators <- extent
  
  object
}


.check_theme <- function(theme) {
  
  if (!is.null(theme$annotations)) {
    if(is.null(theme$annotations$round)) theme$annotations$round <- 1
    if(is.null(theme$annotations$offset)) theme$annotations$offset <- 0.5
  }
  
  theme
}

