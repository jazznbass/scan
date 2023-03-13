#' Plot single-case data
#'
#' This function provides a plot of a single-case or multiple single-cases.
#'
#'
#' @aliases plotSC plot.scdf
#' @inheritParams .inheritParams
#' @param ylim Lower and upper limits of the y-axis (e.g., \code{ylim = c(0,
#'   20)} sets the y-axis to a scale from 0 to 20). With multiple single-cases
#'   you can use \code{ylim = c(0, NA)} to scale the y-axis from 0 to the
#'   maximum of each case. \code{ylim} is not set by default, which makes
#'   \code{scan} set a proper scale based on the given data.
#' @param xlim Lower and upper limits of the x-axis (e.g., \code{xlim = c(0,
#'   20)} sets the x-axis to a scale from 0 to 20). With multiple single-cases
#'   you can use \code{ylim = c(0, NA)} to scale the x-axis from 0 to the
#'   maximum of each case. \code{xlim} is not set by default, which makes
#'   \code{scan} set a proper scale based on the given data.
#' @param xinc An integer. Increment of the x-axis. 1 :each mt value will be
#'   printed, 2 : every other value, 3 : every third values etc.
#' @param style Either a character with the name of a pre-implemented style or a
#'   style object. See \code{\link{style_plot}} to learn about this format.
#' @param lines A list defining one or multiple lines or curves to be plotted.
#'   The argument is passed as a list (e.g., \code{list(type = "median")}). Some
#'   of the procedures can be refined with an additional argument (e.g.,
#'   \code{lines = list(type = "mean", trim = 0.2)} adds a 20\% trimmed mean
#'   line. For multiple lines, provide a list element for each line (e.g.,
#'   \code{list( list(type = "median", col = "red"), list(type = "trend", col =
#'   "blue"))}.
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
#' @param marks A list of parameters defining markings of certain data points.
#' \itemize{ \item\code{"positions"} A vector or a list of vectors indicating
#' measurement-times to be highlighted. In case of a vector, the marked
#' measurement-times are the same for all plotted cases. In case of a list of
#' vectors, marks are set differently for each case. The list must have the
#' same length as there are cases in the data file.  \item\code{"col"} Color of
#' the marks.  \item\code{"cex"} Size of the marks.  } Use for example
#'   \code{marks = list(positions = c(1, 8, 15), col = "red", cex = 3)} to make
#'   the MTs one, eight and 18 appear big and red.
#' @param phase.names By default phases are labeled based on the levels of the
#'   phase variable.
#' Use this argument to specify different labels: \code{phase.names = c("Baseline",
#' "Intervention")}.
#' @param xlab The label of the x-axis. Default is \code{xlab = "Measurement
#'   time"}.
#' @param ylab The labels of the y-axis. Default is \code{ylab = "Score"}.
#' @param main Main title of the plot.
#' @param case.names Case names. If not provided, names are taken from the scdf.
#'   Set \code{case.names = ""} if you don't like to include case names.
#' @param ... Further arguments passed to the plot command.
#' @return Returns a plot of one or multiple single-cases.
#' @author Juergen Wilbert
#' @seealso \code{\link{style_plot}}, \code{\link{describeSC}},
#'   \code{\link{overlapSC}}
#' @examples
#'
#' ## Request the default plot of the data from Borckhardt (2014)
#' plot(Borckardt2014)
#'
#' ## Plot the three cases from Grosche (2011) and visualize the phase A trend
#' plot(Grosche2011, style = "grid", lines = "trendA")
#'
#' ## Request the local regression line for Georg from that data set and customize the plot
#' plot(Grosche2011$Georg, style = "sienna", ylim = c(0,NA),
#'        xlab = "Training session", ylab = "Words per minute",
#'        phase.names = c("Baseline", "Intervention"), xinc = 5,
#'        lines = list(type = "loreg", f = 0.2, lty = "solid", col = "black", lwd = 3))
#'
#' ## Plot a random MBD over three cases and mark interesting MTs
#' dat <- random_scdf(design = design(3))
#' plot(dat, marks = list(positions = list(c(2,4,5),c(1,2,3),c(7,8,9)), col = "blue",
#'        cex = 1.4), style = c("grid", "annotate", "tiny"))
#'
#' @export
plot.scdf <- function(...) {
  plotSC(...)
}

#' @rdname plot.scdf
#' @export
plotSC <- function(data, dvar, pvar, mvar, 
                   ylim = NULL, xlim = NULL, xinc = 1, 
                   lines = NULL, marks = NULL, 
                   phase.names = NULL, xlab = NULL, ylab = NULL, 
                   main = "", case.names = NULL, 
                   style = getOption("scan.plot.style"), 
                   ...) {
  
  
  # prepare scdf ------------------------------------------------------------
  
  dots <- list(...)
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar
  
  data_list <- .prepare_scdf(data)
  
  N <- length(data_list)
  
  if (N > 1) par(mfrow = c(N, 1))
  
  # define style ------------------------------------------------------------
  
  if (is.list(style)) {
    ref.style <- "default"
    if ("style" %in% names(style)) ref.style <- style$style
    style <- c(style, style_plot(ref.style))
    style <- style[unique(names(style))]
  }
  
  if (is.character(style)) style <- style_plot(style)
  
  #for pre style backwards compatibility
  sty_names <- c("fill", "fill.bg", "frame", "grid", "lwd", "pch", "text.ABlag", "type")
  if (any(names(dots) %in% sty_names)) {
    stop("Using style parameters directly as arguments (like 'fill') is deprectated. ",
         "Please use the 'stlye' argument to provide these parameters. ",
         "E.g., style = list(fill = 'blue', pch = 19)")
  }
  annotations <- style$annotations
  
  if (is.na(style$frame)) style$bty <- "n"
  
  par("bg"       = style$col.bg)
  par("col"      = style$col)
  par("family"   = style$font)
  par("cex"      = style$cex)
  par("cex.lab"  = style$cex.lab)
  par("cex.axis" = style$cex.axis)
  par("las"      = style$las)
  par("bty"      = style$bty)
  par("col.lab"  = style$col.text)
  par("col.axis" = style$col.text)
  
  if (isTRUE(style$frame == ""))   style$frame <- NA
  if (isTRUE(style$grid == ""))    style$grid  <- FALSE
  if (isTRUE(style$fill.bg == "")) style$fill.bg  <- FALSE
  
  ### END: define style
  
  # Marks on the outliers from outlierandom_scdf
  if (inherits(marks, "sc_outlier")) 
    marks <- list(positions = marks$dropped.mt)
  
  # name cases
  if (!is.null(case.names)) names(data_list) <- case.names
  case.names <- names(data_list)
  
  # set x ans y axis labels
  if (is.null(xlab)) xlab <- mvar
  if (is.null(ylab)) ylab <- dvar
  if (is.null(xlab)) xlab <- "Measurement time"
  if (is.null(ylab)) ylab <- "Score"
  if (xlab == "mt") xlab <- "Measurement time"
  
  # prepare lines definitions
  lines <- .prepare_arg_lines(lines)

  # set xlim and ylim
  values.tmp <- unlist(lapply(data_list, function(x) x[, dvar]))
  mt.tmp     <- unlist(lapply(data_list, function(x) x[, mvar]))
  
  if (is.null(ylim))
    ylim <- c(min(values.tmp, na.rm = TRUE), max(values.tmp, na.rm = TRUE))
  if (is.null(xlim))
    xlim <- c(min(mt.tmp, na.rm = TRUE), max(mt.tmp, na.rm = TRUE))
  
  
  # Plotting cases ----------------------------------------------------------
  
  par(mgp = c(2, 1, 0))
  
  for(case in 1:N) {
    data <- data_list[[case]]
    
    design <- .phasestructure(data, pvar)
    
    # plot ylim
    y.lim <- ylim
    if (is.na(ylim[2])) y.lim[2] <- max(data[, dvar])
    if (is.na(ylim[1])) y.lim[1] <- min(data[, dvar])
    
    # one plot
    if (N == 1) {
      if (main != "") par(mai = c(style$mai[1:2], style$mai[3] + 0.4, style$mai[4]))
      if (main == "") par(mai = style$mai)
      plot(
        data[[mvar]], data[[dvar]], type = "n", 
        xlim = xlim, ylim = y.lim, ann = FALSE,
        #xaxp = c(xlim[1], xlim[2], xlim[2] - xlim[1]), 
        yaxt = "n", 
        xaxt = "n", ...
      )
      
      xticks_pos <- seq(xlim[1], xlim[2], 1)
      axis(side = 1, at = xticks_pos, labels = FALSE)
      text(
        x = seq(xlim[1], xlim[2], xinc),  
        y = par("usr")[3], 
        cex = style$cex.axis, 
        labels = seq(xlim[1], xlim[2], xinc), 
        srt = 0, 
        pos = 1, 
        offset = style$cex.axis, 
        xpd = TRUE
      )
      
      yticks_pos <- axTicks(2, usr = c(y.lim[1], ylim[2]))
      axis(side = 2, at = yticks_pos, labels = NA)
      text(
        x = par("usr")[1], 
        y = yticks_pos, 
        labels = yticks_pos, 
        offset = style$cex.axis, 
        srt = 0, 
        pos = 2, 
        cex = style$cex.axis, 
        xpd = TRUE
      )
      
      if (style$ylab.orientation == 0) 
        mtext(text = ylab, side = 2, line = 2, las = 0, cex = style$cex.lab)
      
      if (style$ylab.orientation == 1) {
        mtext(
          text = ylab, side = 2, line = 2, las = 1, at = max(y.lim), 
          cex = style$cex.lab
        )
      }
      
      mtext(text = xlab, side = 1, line = 2, las = 0, cex = style$cex.lab)
    } 
    
    # multple plots, first to secondlast
    if (N > 1 && case != N) {
      if (case == 1) { # first plot
        if (main != "") {
          par(mai = c(style$mai[1] * 2 / 6, style$mai[2], style$mai[3] * 3, style$mai[4]))
        }
        if (main == "") { 
          par(mai = c(style$mai[1] * 2 / 6, style$mai[2], style$mai[3] * 3, style$mai[4])) 
        }
      } else { # middle plot
        par(mai = c(style$mai[1] * 4 / 6, style$mai[2], style$mai[3] * 2, style$mai[4])) 
      }
      
      plot(
        data[[mvar]], data[[dvar]], xaxt = "n", yaxt = "n", type = "n", 
        xlim = xlim, ylim = y.lim, ann = FALSE, ...
      )
      
      yticks_pos <- axTicks(2, usr = c(y.lim[1], ylim[2]))
      axis(side = 2, at = yticks_pos, labels = NA)
      text(
        x = par("usr")[1], 
        y = yticks_pos, 
        labels = yticks_pos, 
        offset = style$cex.axis, 
        srt = 0, 
        pos = 2, 
        cex = style$cex.axis, 
        xpd = TRUE
      )
      
      if (style$ylab.orientation == 0) 
        mtext(ylab, side = 2, line = 2, las = 0, cex = style$cex.lab)
      
      if (style$ylab.orientation == 1)
        mtext(ylab, side = 2, line = 2, las = 1, at = max(y.lim), cex = style$cex.lab)
    }
    # multiple plots, last plot
    if (N > 1 && case == N) {
      par(mai = style$mai)
      plot(
        data[[mvar]], data[[dvar]], type = "n", 
        xlim = xlim, ylim = y.lim, ann = FALSE,
        xaxp = c(xlim[1], xlim[2], xlim[2] - xlim[1]),
        yaxt = "n", xaxt = "n",
        ...
      )
      
      xticks_pos <- seq(xlim[1], xlim[2], 1)
      axis(side = 1, at = xticks_pos, labels = FALSE)
      text(
        x = seq(xlim[1], xlim[2], xinc),  
        y = par("usr")[3], 
        cex = style$cex.axis, 
        labels = seq(xlim[1], xlim[2], xinc), 
        srt = 0, 
        pos = 1, 
        offset = style$cex.axis, 
        xpd = TRUE
      )
      
      yticks_pos <- axTicks(2, usr = c(y.lim[1], ylim[2]))
      axis(side = 2, at = yticks_pos, labels = NA)
      text(
        x = par("usr")[1], 
        y = yticks_pos, 
        labels = yticks_pos, 
        offset = style$cex.axis, 
        srt = 0, 
        pos = 2, 
        cex = style$cex.axis, xpd = TRUE
      )
      
      if (style$ylab.orientation == 0) 
        mtext(ylab, side = 2, line = 2, las = 0, cex = style$cex.lab)
      
      if (style$ylab.orientation == 1)
        mtext(ylab, side = 2, line = 2, las = 1, at = max(y.lim), cex = style$cex.lab)
      
      mtext(xlab, side = 1, line = 2, las = 0, cex = style$cex.lab)
      
    } 
    usr <- par("usr")
    
    # styling ------------------------------------------------------------
    
    # fill bg
    if(inherits(style$fill.bg, "character")) {
      style$col.fill.bg <- style$fill.bg
      style$fill.bg <- TRUE
    }
    
    if (isTRUE(style$fill.bg)){
      #rect(usr[1], usr[3], usr[2], usr[4], col = style$col.fill.bg, border = NA)
      
      type_phases <- unique(design$values)
      col <- rep(style$col.fill.bg, length = length(type_phases))
      
      for(i in seq_along(design$values)) {
        x <- data[design$start[i]:design$stop[i], mvar]
        y <- data[design$start[i]:design$stop[i], dvar]
        
        xmin <- min(x, na.rm = TRUE)
        xmax <- max(x, na.rm = TRUE) + 0.5
        if (i == length(design$values)) xmax <- usr[2]
        if (i > 1) xmin <- xmin - 0.5
        if (i == 1) xmin <- usr[1]
        .col <- col[which(type_phases == design$values[i])[1]]
        rect(xmin, usr[3], xmax, usr[4], col = .col, border = NA)
      }
    }
    
    # grid
    if(inherits(style$grid, "character")) {
      style$col.grid <- style$grid
      style$grid <- TRUE
    }
    
    if (isTRUE(style$grid)) 
      grid(NULL, NULL, col = style$col.grid, lty = style$lty.grid, lwd = style$lwd.grid)
    
    if (!is.na(style$frame))
      rect(usr[1],usr[3],usr[2],usr[4], col = NA, border = style$frame)
    
    if (is.na(style$frame) && isTRUE(style$fill.bg))
      rect(usr[1],usr[3],usr[2],usr[4], col = NA, border = style$col.fill.bg)
    
    if (is.na(style$frame) && !isTRUE(style$fill.bg))
      rect(usr[1],usr[3],usr[2],usr[4], col = NA, border = par("bg"))
    
    # fill array below lines
    
    if (style$fill == "" || is.na(style$fill)) style$fill <- FALSE
    
    if(inherits(style$fill, "character")) {
      style$col.fill <- style$fill
      style$fill <- TRUE
    }
    
    if (isTRUE(style$fill)) {
      for(i in 1:length(design$values)) {
        x <- data[design$start[i]:design$stop[i], mvar]
        y <- data[design$start[i]:design$stop[i], dvar]
        
        for(i in 1:length(x)) {
          x_values <- c(x[i], x[i+1], x[i+1], x[i])
          y_values <- c(y.lim[1], y.lim[1], y[i+1], y[i])
          polygon(x_values, y_values, col = style$col.fill, border = NA)      
        }
      }
    }
    
    # draw lines
    for(i in 1:length(design$values)) {
      x <- data[design$start[i]:design$stop[i], mvar]
      y <- data[design$start[i]:design$stop[i], dvar]
      if (style$col.lines != "") {
        lines(
          x, y, type = "l", pch = style$pch, lty = style$lty, lwd = style$lwd, 
          col = style$col.lines, ...
        )
      }
      if (style$col.dots != "") {
        lines(
          x, y, type = "p", pch = style$pch, lty = style$lty, lwd = style$lwd, 
          col = style$col.dots, ...
        )
      }
    }
    
    if (case == 1) title(main)
    
    # marks -------------------------------------------------------------------
    
    if (!is.null(marks)) {
      marks.cex <- 1
      marks.col <- "red"
      marks.pch <- style$pch
      
      if (any(names(marks) == "positions")) {
        marks.pos <- marks[[which(names(marks) == "positions")]]
      } else {stop("Positions of marks must be defined.")}
      
      if (any(names(marks) == "cex")) {
        marks.cex <- marks[[which(names(marks) == "cex")]]
      }
      if (any(names(marks) == "col")) {
        marks.col <- marks[[which(names(marks) == "col")]]
      }
      if (any(names(marks) == "pch")) {
        marks.pch <- marks[[which(names(marks) == "pch")]]
      }
      
      if (inherits(marks.pos, "numeric")) {
        mks <- marks.pos
      } else {
        mks <- marks.pos[[case]]
      }
      marks.x <- data[data[,mvar] %in% mks,mvar]
      marks.y <- data[data[,mvar] %in% mks,dvar]
      points(x = marks.x, y = marks.y, pch = marks.pch, cex = marks.cex, col = marks.col)
    }
    
    # annotations -------------------------------------------------------------
    
    if (!is.null(annotations)) {
      annotations.cex <- 1
      annotations.round <- 1
      annotations.col <- "black"
      annotations.pos <- 3
      annotations.offset <- 0.5
      
      if (any(names(annotations) == "cex")) {
        annotations.cex <- annotations[[which(names(annotations) == "cex")]]
      }
      if (any(names(annotations) == "col")) {
        annotations.col <- annotations[[which(names(annotations) == "col")]]
      }
      if (any(names(annotations) == "round")) {
        annotations.round <- annotations[[which(names(annotations) == "round")]]
      }
      if (any(names(annotations) == "pos")) {
        annotations.pos <- annotations[[which(names(annotations) == "pos")]]
      }
      if (any(names(annotations) == "offset")) {
        annotations.offset <- annotations[[which(names(annotations) == "offset")]]
      }
      
      annotations.label <- round(data[,dvar], annotations.round)
      ### not yet implemented
      #if (any(names(annotations) == "label")) {
      #  id <- which(names(annotations) == "label")
      #  if (annotations[[id]]=="values") {
      #  } else {
      #  }
      #}
      
      text(
        x = data[,mvar], 
        y = data[,dvar], 
        label = annotations.label, 
        col = annotations.col, 
        pos = annotations.pos, 
        offset = annotations.offset, 
        cex = annotations.cex, 
        ...
      )
    }
    
    # lines -------------------------------------------------------------------
    
    if (!is.null(lines)) {
      for(i_lines in seq_along(lines)) {
        
        line <- lines[[i_lines]]
        
        if (is.null(line[["lty"]])) line[["lty"]] <- "dashed"
        if (is.null(line[["lwd"]])) line[["lwd"]] <- 2
        if (is.null(line[["col"]])) line[["col"]] <- "black"
        
        lty.line <- line[["lty"]]
        lwd.line <- line[["lwd"]]
        col.line <- line[["col"]]
        
        if (line[["type"]] == "trend") {
          for(i in 1:length(design$values)) {
            x <- data[design$start[i]:design$stop[i], mvar]
            y <- data[design$start[i]:design$stop[i], dvar]
            reg <- lm(y~x)
            lines(
              x = c(min(x), max(x)), 
              y = c(
                reg$coefficients[1] + min(x) * reg$coefficients[2], 
                reg$coefficients[1] + max(x) * reg$coefficients[2]
              ), 
              lty = lty.line, 
              col = col.line, 
              lwd = lwd.line
            )
          }
        }
        if (line[["type"]] == "median") {
          for(i in 1:length(design$values)) {
            x <- data[design$start[i]:design$stop[i], mvar]
            y <- data[design$start[i]:design$stop[i], dvar]
            lines(
              x = c(min(x), max(x)), 
              y = c(median(y, na.rm = TRUE), median(y, na.rm = TRUE)), 
              lty = lty.line, 
              col = col.line, 
              lwd = lwd.line
            )
          }      
        }
        if (line[["type"]] == "mean") {
          if (is.null(line[["trim"]])) line[["trim"]] <- 0.1
          lines.par <- line[["trim"]]

          for(i in 1:length(design$values)) {
            x <- data[design$start[i]:design$stop[i],mvar]
            y <- data[design$start[i]:design$stop[i],dvar]
            lines(
              x = c(min(x), max(x)), 
              y = c(
                mean(y, trim = lines.par, na.rm = TRUE), 
                mean(y, trim = lines.par, na.rm = TRUE)
              ), 
              lty = lty.line, 
              col = col.line, 
              lwd = lwd.line
            )
          }
        }
        if (line[["type"]] == "trendA") {
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          reg <- lm(y~x)
          lines(
            x = c(min(x), maxMT), 
            y = c(
              reg$coefficients[1]  + min(x) * reg$coefficients[2], 
              reg$coefficients[1] + maxMT * reg$coefficients[2]
            ), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )
        }
        if (line[["type"]] == "trendA_bisplit") {
          x     <- data[design$start[1]:design$stop[1],mvar]
          y     <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          # na.rm = FALSE for now to prevent misuse; 
          # will draw no line if NA present
          md1   <- c(median(y[1:floor(length(y)/2)], na.rm = FALSE),
                     median(x[1:floor(length(x)/2)], na.rm = FALSE))
          md2   <- c(median(y[ceiling(length(y)/2+1):length(y)], na.rm = FALSE),
                     median(x[ceiling(length(x)/2+1):length(x)], na.rm = FALSE))
          md    <- rbind(md1, md2)
          reg <- lm(md[,1]~md[,2])
          lines(
            x = c(min(x), maxMT), 
            y = c(
              reg$coefficients[1]  + min(x) * reg$coefficients[2], 
              reg$coefficients[1] + maxMT * reg$coefficients[2]
            ), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )
        }
        if (line[["type"]] == "trendA_trisplit") {
          x     <- data[design$start[1]:design$stop[1],mvar]
          y     <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          # na.rm = FALSE for now to prevent misuse; 
          # will draw no line if NA present
          md1   <- c(
            median(y[1:floor(length(y) / 3)], na.rm = FALSE),
            median(x[1:floor(length(x) / 3)], na.rm = FALSE)
          )
          md2   <- c(
            median(y[ceiling(length(y) / 3 * 2 + 1):length(y)], na.rm = FALSE),
            median(x[ceiling(length(x) / 3 * 2 + 1):length(x)], na.rm = FALSE)
          )
          md    <- rbind(md1, md2)
          reg <- lm(md[,1] ~ md[,2])
          lines(
            x = c(min(x), maxMT), 
            y = c(
              reg$coefficients[1]  + min(x) * reg$coefficients[2], 
              reg$coefficients[1] + maxMT * reg$coefficients[2]
            ), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )
        }
        if (line[["type"]] == "loreg") {
          if (is.null(line[["f"]])) line[["f"]] <- 0.5
          lines.par <- line[["f"]]
          reg <- lowess(data[,dvar] ~ data[,mvar], f = lines.par)
          lines(reg, lty = lty.line, col = col.line, lwd = lwd.line)
        }
        
        if (line[["type"]] %in% c("maxA", "pnd")) {
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          lines(
            x = c(min(x), maxMT), 
            y = c(max(y), max(y)), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )		
        }
        
        if (line[["type"]] == "minA") {
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          lines(
            x = c(min(x), maxMT), 
            y = c(min(y), min(y)), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )		
        }
        if (line[["type"]] == "medianA") {
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          
          lines(
            x = c(min(x), maxMT), 
            y = c(median(y, na.rm = TRUE), median(y, na.rm = TRUE)), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )		
        }
        if (line[["type"]] == "meanA") {
          if (is.null(line[["trim"]])) line[["trim"]] <- 0.1
          lines.par <- line[["trim"]]
          
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          lines(
            x = c(min(x), maxMT), 
            y = c(
              mean(y, trim = lines.par, na.rm = TRUE), 
              mean(y, trim = lines.par, na.rm = TRUE)
            ), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )		
        }
        
        if (line[["type"]] == "plm") {
          pr <- plm(data_list[case])
          y <- fitted(pr$full.model)
          lines(data[[mvar]], y, lty = lty.line, col = col.line, lwd = lwd.line)
        }
        if (line[["type"]] == "plm.ar") {
          if (is.null(line[["ar"]])) line[["ar"]] <-2
          lines.par <- line[["ar"]]
          pr <- plm(data_list[case], AR = lines.par)
          y <- fitted(pr$full.model)
          lines(data[[mvar]], y, lty = lty.line, col = col.line, lwd = lwd.line)
        }
        
        if (line[["type"]] == "movingMean") {
          if (is.null(line[["lag"]])) line[["lag"]] <- 1
          lines.par <- line[["lag"]]
          y <- .moving_average(data[, dvar],lines.par, mean)
          lines(data[, mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
        }
        if (line[["type"]] == "movingMedian") {
          if (is.null(line[["lag"]])) line[["lag"]] <- 1
          lines.par <- line[["lag"]]
          y <- .moving_average(data[, dvar],lines.par, median)
          lines(data[, mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
        }
      }
    }#### END: Adding help-lines
    
    
    # phase names -------------------------------------------------------------
    if (is.null(phase.names)) {
      case_phase_names <- design$values
    } else {
      case_phase_names <- phase.names
    }
    
    for(i in 1:length(design$values)) {
      mtext(
        text = case_phase_names[i], 
        side = 3, 
        at = (data[design$stop[i], mvar] - data[design$start[i], mvar]) / 2 + 
              data[design$start[i], mvar], 
        cex = style$cex.text, ...
      )
    }
    
    # line between phases -----------------------------------------------------
    if (is.null(style$text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        abline(
          v = data[design$stop[i] + 1, mvar] - 0.5, 
          lty = style$lty.seperators, 
          lwd = style$lwd.seperators, 
          col = style$col.seperators
        )
      }
    }
    
    if (!is.null(style$text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        tex <- paste(unlist(strsplit(style$text.ABlag[i], "")), collapse = "\n")
        text(
          x = data[design$stop[i] + 1, mvar] - 0.5, 
          y = (y.lim[2] - y.lim[1]) / 2 + y.lim[1], 
          labels = tex, 
          cex = 0.8, ...)
      }
      
    }
    
    # Adding case name --------------------------------------------------------
    if (length(case.names) ==  N) {
      args <- c(
        list(text = case.names[case]), 
        style$names, 
        cex = style$cex.text, 
        at = min(xlim)
      )
      args <- args[!duplicated(names(args))]
      do.call(mtext, args)
    }
    
  }
  
}

.prepare_arg_lines <- function(lines) {
  
  
  types <- list(
    "mean",
    "median", 
    "pnd", 
    "maxA", 
    "minA", 
    "medianA", 
    "trend", 
    "trendA", 
    "plm", 
    "plm.ar",
    "trendA_bisplit", 
    "trendA_trisplit",
    "loreg", 
    "movingMean", 
    "movingMedian"
  )
  
  if (!inherits(lines, "list")) {
    lines <- lapply(lines, function(x) x)
  }
  
  if(!all(sapply(lines, is.list))) {
    lines <- list(lines)
  }
  
  if (length(lines) == 1) {
    tmp_args <- unlist(lines[[1]])
    id <- which(tmp_args %in% names(types))
    if (length(id) > 1) {
      warning("More than one line type detected in one list element!")
      for (i in 2:length(id)) {
        lines <- c(lines, list(lines[[1]][id[i]]))
        lines[[1]] <- lines[[1]][-id[i]]
      }
    }
    
    tmp_args <- names(lines[[1]])
    id <- which(tmp_args %in% names(types))
    if (length(id) > 1) {
      warning("More than one line type detected in one list element!")
      for (i in 2:length(id)) {
        lines <- c(lines, list(lines[[1]][id[i]]))
        lines[[1]] <- lines[[1]][-id[i]]
      }
    }
    
  }
  
  for (i in seq_along(lines)) {
    arg_names <- names(lines[[i]])
    
    if (is.null(arg_names)) {
      names(lines[[i]]) <- rep("", length(lines[[i]]))
      arg_names <- names(lines[[i]])
    }
    
    if (any(arg_names == "mean")) {
      id <- which(arg_names == "mean")
      lines[[i]] <- c(lines[[i]], trim = as.numeric(unname(lines[[i]][id])))
      lines[[i]][id] <- names(lines[[i]])[id]
      names(lines[[i]])[id] <- "type"
    }
    
    if (any(arg_names == "loreg")) {
      id <- which(arg_names == "loreg")
      lines[[i]] <- c(lines[[i]], f = as.numeric(unname(lines[[i]][id])))
      lines[[i]][id] <- names(lines[[i]])[id]
      names(lines[[i]])[id] <- "type"
    }
    
    if (any(arg_names == "movingMedian")) {
      id <- which(arg_names == "movingMedian")
      lines[[i]] <- c(lines[[i]], lag = as.numeric(unname(lines[[i]][id])))
      lines[[i]][id] <- names(lines[[i]])[id]
      names(lines[[i]])[id] <- "type"
    }
    
    if (any(arg_names == "movingMean")) {
      id <- which(arg_names == "movingMean")
      lines[[i]] <- c(lines[[i]], lag = as.numeric(unname(lines[[i]][id])))
      lines[[i]][id] <- names(lines[[i]])[id]
      names(lines[[i]])[id] <- "type"
    }
    
    if (any(arg_names == "plm.ar")) {
      id <- which(arg_names == "plm.ar")
      lines[[i]] <- c(lines[[i]], ar = as.numeric(unname(lines[[i]][id])))
      lines[[i]][id] <- names(lines[[i]])[id]
      names(lines[[i]])[id] <- "type"
    }
    
    id <- which(arg_names == "")
    if(length(id) > 1) {
      stop("undefined lines argument(s)") # todo: specify error
    }
    if(length(id) == 1) {
      names(lines[[i]])[id] <- "type"
    }
  }
  
  lines
  
}
