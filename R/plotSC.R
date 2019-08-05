#' Plot single-case data
#' 
#' The \code{plotSC} function provides a plot of a single-case or multiple
#' single-cases.
#' 
#' 
#' @aliases plotSC plot.scdf
#' @inheritParams .inheritParams
#' @param ylim Lower and upper limits of the y-axis (e.g., \code{ylim = c(0,
#' 20)} sets the y-axis to a scale from 0 to 20). With multiple single-cases
#' you can use \code{ylim = c(0, NA)} to scale the y-axis from 0 to the maximum
#' of each case. \code{ylim} is not set by default, which makes \code{scan} set
#' a proper scale based on the given data.
#' @param xlim Lower and upper limits of the x-axis (e.g., \code{xlim = c(0,
#' 20)} sets the x-axis to a scale from 0 to 20). With multiple single-cases
#' you can use \code{ylim = c(0, NA)} to scale the x-axis from 0 to the maximum
#' of each case. \code{xlim} is not set by default, which makes \code{scan} set
#' a proper scale based on the given data.
#' @param xinc An integer. Increment of the x-axis. 1 :each mt value will be printed, 2 : every other value, 3 : every third values etc.
#' @param style Either a character with the name of a pre-implemented style or a style object. See \code{\link{style.plotSC}} to learn about this format. 
#' @param lines A character or list defining one or more lines or curves to be
#' plotted. The argument is either passed as a character string (e.g.,
#' \code{lines = "median"}) or as a list (e.g., \code{list("median", "trend")}.
#' Some of the procedures can be refined with an additional argument (e.g.,
#' \code{lines = list("mean" = 0.20)} adds a 20\% trimmed mean line. By default
#' no additional lines are plotted. Possible lines are: \itemize{
#' \item\code{"median"} Separate lines for phase A and B medians.
#' \item\code{"mean"} Separate lines for phase A and B means. By default it is
#' 10\%-trimmed. Other trims can be set, using a second parameter (e.g.,
#' \code{lines = list(mean = 0.2)} draws a 20\%-trimmed mean line).
#' \item\code{"trend"} Separate lines for phase A and B trends.
#' \item\code{"trendA"} Trend line for phase A, extrapolated throughout phase
#' B.  \item\code{"maxA/minA"} Line at the level of the highest or lowest phase A score.
#' \item\code{"medianA"} Line at the phase A median score.  \item\code{"meanA"}
#' Line at the phase A 10\%-trimmed mean score. Apply a different trim, by
#' using the additional argument (e.g., \code{lines = list(meanA = 0.2)}).
#' \item\code{"plm"} Regression lines for piecewise linear regression model.
#' \item\code{"plm.ar"} Regression lines for piecewise autoregression model.
#' The lag is specified like this: \code{lines = list(plm.ar = 2)}. Default lag is set to 2.
#' \item\code{"movingMean"} Draws a moving mean curve, with a specified lag:
#' \code{lines = list(movingMean = 2)}. Default is a lag 1 curve.
#' \item\code{"movingMedian"} Draws a moving median curve, with a specified
#' lag: \code{lines = list(movingMedian = 3)}. Default is a lag 1 curve.
#' \item\code{"loreg"} Draws a non-parametric local regression line. The
#' proportion of data influencing each data point can be specified using
#' \code{lines = list("loreg" = 0.66)}. The default is 0.5.  \item\code{"lty"}
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
#' \code{marks = list(positions = c(1, 8, 15), col = "red", cex = 3)} to make
#' the MTs one, eight and 18 appear big and red.
#' @param phase.names By default phases are labeled based on the levels of the phase variable. 
#' Use this argument to specify different labels: \code{phase.names = c("Baseline",
#' "Intervention")}.
#' @param xlab The label of the x-axis. Default is \code{xlab = "Measurement
#' time"}.
#' @param ylab The labels of the y-axis. Default is \code{ylab = "Score"}.
#' @param main Main title of the plot.
#' @param case.names Case names. If not provided, names are taken from the scdf.
#' Set \code{case.names = ""} if you don't like to include case names.
#' @param ... Further arguments passed to the plot command.
#' @return Returns a plot of one or multiple single-cases.
#' @author Juergen Wilbert
#' @seealso \code{\link{style.plotSC}}, \code{\link{describeSC}}, \code{\link{overlapSC}}
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
#'        phase.names = c("Baseline", "Intervention"), 
#'        lines = list("loreg", lty = "solid", col = "black", lwd = 3))
#' 
#' ## Plot a random MBD over three cases and mark interesting MTs
#' dat <- rSC(design = design_rSC(3))
#' plot(dat, marks = list(positions = list(c(2,4,5),c(1,2,3),c(7,8,9)), col = "blue",
#'        cex = 1.4), style = c("grid", "annotate", "tiny"))
#' 
#' @export
plot.scdf <- function(...) {
  plotSC(...)
}

#' @rdname plot.scdf
#' @export
plotSC <- function(data, dvar, pvar, mvar, ylim = NULL, xlim = NULL, xinc = 1, lines = NULL, marks = NULL, phase.names = NULL, xlab = NULL, ylab = NULL, main = "", case.names = NULL, style = "grid", ...) {
  
  dots <- list(...)
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
  
  data.list <- .SCprepareData(data)
  
  N <- length(data.list)
  if (N > 1) par(mfrow = c(N, 1))
  
# define style ------------------------------------------------------------
  
  if (is.list(style)) {
    ref.style <- "default"
    if ("style" %in% names(style)) ref.style <- style$style
    style <- c(style, style_plotSC(ref.style))
    style <- style[unique(names(style))]
  }
  
  if (is.character(style)) style <- style_plotSC(style)
  
  #for pre style backwards compatibility
  sty.names <- c("fill", "fill.bg", "frame", "grid", "lwd", "pch", "text.ABlag", "type")
  if (any(names(dots) %in% sty.names)) {
    stop("Using style parameters like 'fill' directly as arguments is deprectated. ",
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
  
  if (style$frame %in% "")   style$frame <- NA
  if (style$grid %in% "")    style$grid  <- FALSE
  if (style$fill.bg %in% "") style$fill.bg  <- FALSE
  
  ### END: define style
  
  # Marks on the outliers from outlierSC
  if (identical(class(marks), c("sc","outlier"))) 
    marks <- list(positions = marks$dropped.mt)
  
  # name cases
  if (!is.null(case.names)) names(data.list) <- case.names
  case.names <- names(data.list)
  
  # set x ans y axis labels
  if (is.null(xlab)) xlab <- mvar
  if (is.null(ylab)) ylab <- dvar
  if (is.null(xlab)) xlab <- "Measurement time"
  if (is.null(ylab)) ylab <- "Score"
  if (xlab == "mt") xlab <- "Measurement time"
  
  # prepare lines definitions
  if (class(lines) != "list") lines <- lapply(lines, function(x) x)
  
  if (!is.null(names(lines))) {
    id <- which(names(lines) == "")
    names(lines)[id] <- lines[id]
    lines[id] <- NA
  } else {
    tmp <- lines
    lines <- rep(NA, length(lines))
    names(lines) <- tmp
  }
  
  # set xlim and ylim
  values.tmp <- unlist(lapply(data.list, function(x) x[, dvar]))
  mt.tmp     <- unlist(lapply(data.list, function(x) x[, mvar]))
  
  if (is.null(ylim))
    ylim <- c(min(values.tmp, na.rm = TRUE), max(values.tmp, na.rm = TRUE))
  if (is.null(xlim))
    xlim <- c(min(mt.tmp, na.rm = TRUE), max(mt.tmp, na.rm = TRUE))
  
  
# Plotting cases ----------------------------------------------------------

  par(mgp = c(2,1,0))
  
  for(case in 1:N) {
    data <- data.list[[case]]

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
        seq(xlim[1], xlim[2], xinc),  par("usr")[3], cex = style$cex.axis, labels = seq(xlim[1], 
        xlim[2], xinc), srt = 0, pos = 1, offset = style$cex.axis, xpd = TRUE
      )
      
      yticks_pos <- axTicks(2, usr = c(y.lim[1], ylim[2]))
      axis(side = 2, at = yticks_pos, labels = NA)
      text(
        par("usr")[1], yticks_pos, labels = yticks_pos, offset = style$cex.axis, srt = 0, pos = 2, 
        cex = style$cex.axis, xpd = TRUE
      )
      
      if (style$ylab.orientation == 0) 
        mtext(ylab, side = 2, line = 2, las = 0, cex = style$cex.lab)

      if (style$ylab.orientation == 1)
        mtext(ylab, side = 2, line = 2, las = 1, at = max(y.lim), cex = style$cex.lab)

      mtext(xlab, side = 1, line = 2, las = 0, cex = style$cex.lab)
    } 
    
    # multple plots, first to secondlast
    if (N > 1 && case != N) {
      # first plot
      if (case == 1) {
        if (main != "") 
          par(mai = c(style$mai[1] * 2 / 6, style$mai[2], style$mai[3] * 3, style$mai[4]))
        if (main == "") 
          par(mai = c(style$mai[1] * 2 / 6, style$mai[2], style$mai[3] * 3, style$mai[4])) #par(mai = c(0.2, 0.82, 0.6, 0.42))
      }
      # middle plot
      else  
        par(mai = c(style$mai[1] * 4 / 6, style$mai[2], style$mai[3] * 2, style$mai[4])) #par(mai = c(0.4, 0.82, 0.4, 0.42))
      
      plot(
        data[[mvar]], data[[dvar]], xaxt = "n", yaxt = "n", type = "n", 
        xlim = xlim, ylim = y.lim, ann = FALSE, ...
      )
      
      yticks_pos <- axTicks(2, usr = c(y.lim[1], ylim[2]))
      axis(side = 2, at = yticks_pos, labels = NA)
      text(
        par("usr")[1], yticks_pos, labels = yticks_pos, offset = style$cex.axis, srt = 0, pos = 2, 
        cex = style$cex.axis, xpd = TRUE
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
        seq(xlim[1], xlim[2], xinc),  par("usr")[3], cex = style$cex.axis, labels = seq(xlim[1], 
        xlim[2], xinc), srt = 0, pos = 1, offset = style$cex.axis, xpd = TRUE
      )
      
      yticks_pos <- axTicks(2, usr = c(y.lim[1], ylim[2]))
      axis(side = 2, at = yticks_pos, labels = NA)
      text(
        par("usr")[1], yticks_pos, labels = yticks_pos, offset = style$cex.axis, srt = 0, pos = 2, 
        cex = style$cex.axis, xpd = TRUE
      )
      
      if (style$ylab.orientation == 0) 
        mtext(ylab, side = 2, line = 2, las = 0, cex = style$cex.lab)
      
      if (style$ylab.orientation == 1)
        mtext(ylab, side = 2, line = 2, las = 1, at = max(y.lim), cex = style$cex.lab)
      
      mtext(xlab, side = 1, line = 2, las = 0, cex = style$cex.lab)
      
    } 
    usr <- par("usr")

    # plot styling
    
    # fill bg
    if(class(style$fill.bg) == "character") {
      style$col.fill.bg <- style$fill.bg
      style$fill.bg <- TRUE
    }
    if (isTRUE(style$fill.bg))
      rect(usr[1], usr[3], usr[2], usr[4], col = style$col.fill.bg, border = NA)#, border = par("fg"))
    
    
    # grid
    if(class(style$grid) == "character") {
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

    if(class(style$fill) == "character") {
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
      if (style$col.lines != "")
        lines(x, y, type = "l", pch = style$pch, lty = style$lty, lwd = style$lwd, col = style$col.lines, ...)
      if (style$col.dots != "")
        lines(x, y, type = "p", pch = style$pch, lty = style$lty, lwd = style$lwd, col = style$col.dots, ...)
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
      
      if (class(marks.pos) == "numeric") {
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
      
      text(data[,mvar], data[,dvar], label = annotations.label, col = annotations.col, pos = annotations.pos, offset = annotations.offset, cex = annotations.cex, ...)
    }
    

    # lines -------------------------------------------------------------------
    
    if (!is.null(lines)) {
      #label <- ""
      #labelxy <- c(0,0)
      lty.line <- "dashed"
      lwd.line <- 2
      col.line <- "black"
      
      if (any(names(lines) == "lty")) {
        id <- which(names(lines) == "lty")
        lty.line <- lines[[id]]
      }
      if (any(names(lines) == "col")) {
        id <- which(names(lines) == "col")
        col.line <- lines[[id]]
      }
      if (any(names(lines) == "lwd")) {
        id <- which(names(lines) == "lwd")
        lwd.line <- lines[[id]]
      }
      if (any(names(lines) == "trend")) {
        for(i in 1:length(design$values)) {
          x <- data[design$start[i]:design$stop[i], mvar]
          y <- data[design$start[i]:design$stop[i], dvar]
          reg <- lm(y~x)
          lines(c(min(x), max(x)), c(reg$coefficients[1] + min(x) * reg$coefficients[2], reg$coefficients[1] + max(x) * reg$coefficients[2]), lty = lty.line, col = col.line, lwd = lwd.line)
        }
      }
      if (any(names(lines) == "median")) {
        for(i in 1:length(design$values)) {
          x <- data[design$start[i]:design$stop[i], mvar]
          y <- data[design$start[i]:design$stop[i], dvar]
          lines(c(min(x), max(x)), c(median(y, na.rm = TRUE), median(y, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)
        }      
        #labelxy <- c(max(Bx), median(B,na.rm = TRUE))
        #label <- "Median"
      }
      if (any(names(lines) == "mean")) {
        id <- which(names(lines) == "mean")
        lines.par <- lines[[id]]
        if (is.na(lines.par)) lines.par <- 0.1
        
        for(i in 1:length(design$values)) {
          x <- data[design$start[i]:design$stop[i],mvar]
          y <- data[design$start[i]:design$stop[i],dvar]
          lines(c(min(x), max(x)), c(mean(y, trim = lines.par, na.rm = TRUE), mean(y, trim = lines.par, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)
        }
      }
      if (any(names(lines) == "trendA")) {
        x <- data[design$start[1]:design$stop[1],mvar]
        y <- data[design$start[1]:design$stop[1],dvar]
        maxMT <- max(data[,mvar])
        reg <- lm(y~x)
        lines(c(min(x), maxMT), c(reg$coefficients[1]  + min(x) * reg$coefficients[2], reg$coefficients[1] + maxMT * reg$coefficients[2]), lty = lty.line, col = col.line, lwd = lwd.line)
      }
      if (any(names(lines) == "loreg")) {
        id <- which(names(lines) == "loreg")
        lines.par <- lines[[id]]
        if (is.na(lines.par)) lines.par <- 0.5
        reg <- lowess(data[,dvar]~data[,mvar], f = lines.par)
        lines(reg, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      
      if (any(names(lines) == "pnd") || any(names(lines) == "maxA")) {
        x <- data[design$start[1]:design$stop[1],mvar]
        y <- data[design$start[1]:design$stop[1],dvar]
        maxMT <- max(data[,mvar])
        lines(c(min(x), maxMT), c(max(y), max(y)), lty = lty.line, col = col.line, lwd = lwd.line)		
        #labelxy <- c(max(Bx), max(A))
        #label <- "Max A"
      }
      
      if (any(names(lines) == "minA")) {
        x <- data[design$start[1]:design$stop[1],mvar]
        y <- data[design$start[1]:design$stop[1],dvar]
        maxMT <- max(data[,mvar])
        lines(c(min(x), maxMT), c(min(y), min(y)), lty = lty.line, col = col.line, lwd = lwd.line)		
      }
      if (any(names(lines) == "medianA")) {
        x <- data[design$start[1]:design$stop[1],mvar]
        y <- data[design$start[1]:design$stop[1],dvar]
        maxMT <- max(data[,mvar])
        
        lines(c(min(x), maxMT), c(median(y, na.rm = TRUE), median(y, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)		
      }
      if (any(names(lines) == "meanA")) {
        id <- which(names(lines) == "meanA")
        lines.par <- lines[[id]]
        if (is.na(lines.par)) lines.par <- 0.1
        
        x <- data[design$start[1]:design$stop[1],mvar]
        y <- data[design$start[1]:design$stop[1],dvar]
        maxMT <- max(data[,mvar])
        lines(c(min(x), maxMT), c(mean(y, trim = lines.par, na.rm = TRUE), mean(y, trim = lines.par, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)		
        #labelxy <- c(max(Bx), mean(A, trim = lines.par, na.rm = TRUE))
        #label <- "Mean A"
      }
      if (any(names(lines) == "plm")) {
        pr <- plm(data)
        y <- pr$full.model$fitted.values
        lines(data[,mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      if (any(names(lines) == "plm.ar")) {
        id <- which(names(lines) == "plm.ar")
        lines.par <- as.numeric(lines[[id]])
        if (is.na(lines.par)) lines.par <- 2
        pr <- plm(data, AR = lines.par)
        y <- pr$full.model$fitted
        lines(data[,mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      
      if (any(names(lines) == "movingMean")) {
        id <- which(names(lines) == "movingMean")
        lines.par <- lines[[id]]
        if (is.na(lines.par)) lines.par <- 1
        y <- .SCmovingAverage(data[,dvar],lines.par, mean)
        lines(data[,mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      if (any(names(lines) == "movingMedian")) {
        id <- which(names(lines) == "movingMedian")
        lines.par <- lines[[id]]
        if (is.na(lines.par)) lines.par <- 1
        y <- .SCmovingAverage(data[,dvar],lines.par, median)
        lines(data[,mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      
    }#### END: Adding help-lines
    

    # phase names -------------------------------------------------------------
    if (is.null(phase.names)) phase.names <- design$values
    for(i in 1:length(design$values)) {
      mtext(
        phase.names[i], side = 3, 
        at = (data[design$stop[i], mvar] - data[design$start[i], mvar]) / 2 + data[design$start[i], mvar], 
        cex = style$cex.text, ...
      )
    }
    
    # line between phases -----------------------------------------------------
    if (is.null(style$text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        abline(v = data[design$stop[i] + 1, mvar] - 0.5, lty = style$lty.seperators, lwd = style$lwd.seperators, col = style$col.seperators)
      }
    }
    
    if (!is.null(style$text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        tex <- paste(unlist(strsplit(style$text.ABlag[i], "")), collapse = "\n")
        text(data[design$stop[i] + 1, mvar] - 0.5, (y.lim[2] - y.lim[1]) / 2 + y.lim[1], labels = tex, cex = 0.8, ...)
      }
      
    }

    # Adding case name --------------------------------------------------------
    if (length(case.names) ==  N)
      mtext(case.names[case], side = 3, line = -1, adj = 0, at = min(xlim), cex = style$cex.text, ...)	
  }
  
}
