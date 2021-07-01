#' @export
print.scplot <- function(object) {
  
  data_list <- object$scdf
  dvar <- object$dvar
  pvar <- object$pvar
  mvar <- object$mvar
  ylim <- object$yaxis$lim
  xlim <- object$xaxis$lim
  theme <- object$theme
  
  theme <- .check_theme(theme)
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  

  # set outer margin --------------------------------------------------------

  if (!is.null(object$title)) {
    theme$oma[3] <- theme$oma[3] + 1 + sum(unlist(strsplit(object$title, "")) == "\n") 
  }
  if (!is.null(object$caption)) {
    if (!is.null(theme$wrap.caption)) {
      object$caption <- paste(
        strwrap(object$caption, width = theme$wrap.caption),
        collapse = "\n"
      )
    }    
    if (theme$parse.caption) object$caption <- str2expression(object$caption)
    theme$oma[1] <- theme$oma[1] + 1 + sum(unlist(strsplit(object$caption, "")) == "\n")
  }
  #if (!is.null(object$xlab)) theme$oma[1] <- theme$oma[1] + 1.1
  
  
  
  N <- length(data_list)
  
  if (N > 1) par(mfrow = c(N, 1))
  
  par("oma" = theme$oma)
  
  if (!is.null(theme$col.bg)) par("bg" = theme$col.bg)
  par("family"   = theme$font)
  par("cex"      = 1) # or 1

  # prepare lines definitions
  object$lines <- .prepare_arg_lines(object$lines)
  
  # set global xlim and ylim ---------
  
  if (is.null(ylim)) {
    .dv <- unlist(lapply(data_list, function(x) x[, dvar]))
    ylim <- c(min(.dv, na.rm = TRUE), max(.dv, na.rm = TRUE))
  }
  if (is.null(xlim)) {
    .mt     <- unlist(lapply(data_list, function(x) x[, mvar]))
    xlim <- c(min(.mt, na.rm = TRUE), max(.mt, na.rm = TRUE))
  }
  
  # set left margin --------
  
  fac_i_to_l <- 5
  
  w_axis <- strwidth(
   as.character(ylim[2]), 
   units = "inches", 
   cex = theme$cex.yaxis
  ) * fac_i_to_l
 
  w_ticks <- strwidth(
    "W", units = "inches", cex = theme$cex.yaxis * 0.5 * fac_i_to_l
  )
  
  # Horizontal
  if (theme$ylab.orientation == 1) {
    w_label <- strwidth(
      object$ylabel, 
      units = "inches", 
      cex = theme$cex.ylab
    ) * fac_i_to_l
  }
  
  # Vertical
  if (theme$ylab.orientation == 0) {
    w_label <- theme$cex.ylab * (sum(unlist(strsplit(object$ylabel, "")) == "\n") + 1)
    #w_label <- theme$cex.ylab
  }
  
  #cat(w_label, w_axis, w_ticks, sep = ", ")
  theme$mar[2] <- theme$mar[2] + w_label + w_axis + w_ticks + 
                  0.5 * theme$cex.ylab
  

  # set lower margin ----------------------------------------------------

  h_axis <- theme$cex.xaxis
  h_ticks <- 0.5 * theme$cex.xaxis
  h_label <- (sum(unlist(strsplit(object$xlabel, "")) == "\n") + 1) 
  
  theme$mar[1] <- theme$mar[1] + h_axis + h_ticks + h_label
  
  # Plotting cases ----------------------------------------------------------
  
  for(case in 1:N) {
    data <- data_list[[case]]
    design <- .phasestructure(data, pvar)
    
    # plot ylim
    y_lim <- ylim
    if (is.na(ylim[2])) y_lim[2] <- max(data[, dvar])
    if (is.na(ylim[1])) y_lim[1] <- min(data[, dvar])
    
    #if (case == N) par(mar = theme$mar)
    #if (case != N) par(mar = c(0, theme$mar[2:4]))
    par(mar = theme$mar)

    plot(
      data[[mvar]], data[[dvar]], type = "n", 
      xlim = xlim, ylim = y_lim, ann = FALSE,
      xaxp = c(xlim[1], xlim[2], xlim[2] - xlim[1]),
      yaxt = "n", xaxt = "n", bty = "n"
    )
    
    # add xlab --------------------------------------------------------
    
    if (N == 1 || case == N) {
      
      mtext(
        object$xlabel,
        side = 1,
        line = theme$mar[1] - 1.1,
        padj = 0, # 
        las = 0,
        cex = theme$cex.xlab,
        col = theme$col.xlab
      )
    }
    
    # add ylab -------------------------------------------------------

    if (theme$ylab.orientation == 0) 
      mtext(
        text = object$ylabel, 
        side = 2, 
        line = theme$mar[2] - 0.4,
        las = 0, 
        adj = 0.5,
        padj = 1,
        cex = theme$cex.ylab, 
        col = theme$col.ylab
      )
    
    if (theme$ylab.orientation == 1) {
      mtext(
        text = object$ylabel, 
        side = 2, # left
        line = theme$mar[2] - 0.2, # start 0.2 right of the margin
        las = 1, # horizontal print
        adj = 0, # left aligned
        padj = 1, # text below the "at" position
        at = par("usr")[4], 
        cex = theme$cex.ylab, 
        col = theme$col.ylab
      )
    }
    
    
    usr <- par("usr")
    
    # add inner background -------------------------------------------------------
    
    if (!is.null(theme$col.fill.bg)){

      type_phases <- unique(design$values)
      col <- rep(theme$col.fill.bg, length = length(type_phases))
      
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
    
    # add grid --------------------------------------------

    if (!is.null(theme$col.grid)) 
      grid(
        NULL, NULL, 
        col = theme$col.grid, 
        lty = theme$lty.grid, 
        lwd = theme$lwd.grid
      )
    
    # add fill array below lines --------------------------------
    
    if (!is.null(theme$col.ridge)) {
      for(i in 1:length(design$values)) {
        x <- data[design$start[i]:design$stop[i], mvar]
        y <- data[design$start[i]:design$stop[i], dvar]
        
        for(i in 1:length(x)) {
          x_values <- c(x[i], x[i + 1], x[i + 1], x[i])
          #y_values <- c(y_lim[1], y_lim[1], y[i + 1], y[i])
          y_values <- c(par("usr")[3], par("usr")[3], y[i + 1], y[i])
          polygon(x_values, y_values, col = theme$col.ridge, border = NA)      
        }
      }
    }
    
    # add frame ---------------------------------
    
    if (!is.null(theme$col.frame)){
      rect(
        usr[1],usr[3],usr[2],usr[4], 
        col = NA, 
        border = theme$col.frame,
        lwd = theme$lwd.frame,
        lty = theme$lty.frame
      )
    }
    # add xaxis --------------------------------------------------------
    
    if (N == 1 || case == N) { #only when one plot or last plot
      xticks_pos <- seq(xlim[1], xlim[2], 1)
      axis(
        side = 1, 
        at = xticks_pos, 
        labels = FALSE, 
        col = theme$col.line.xaxis, 
        col.ticks = theme$col.ticks.xaxis,
        tcl = theme$length.ticks.xaxis * theme$cex.xaxis
      )
      text(
        x = seq(xlim[1], xlim[2], object$xaxis$inc),  
        y = par("usr")[3], 
        cex = theme$cex.xaxis, 
        col = theme$col.xaxis,
        labels = seq(xlim[1], xlim[2], object$xaxis$inc), 
        srt = 0, 
        pos = 1, 
        offset = theme$cex.xaxis * 0.75, 
        xpd = TRUE
      )
    }
    
    # add yaxis -------------------------------------------------------
    
    yticks_pos <- axTicks(2, usr = c(y_lim[1], ylim[2]))
    axis(
      side = 2, 
      at = yticks_pos, 
      labels = NA, 
      col = theme$col.line.yaxis, 
      col.ticks = theme$col.ticks.yaxis,
      tcl = theme$length.ticks.yaxis * theme$cex.xaxis
    )
    text(
      x = par("usr")[1],
      y = yticks_pos, 
      labels = yticks_pos, 
      offset = theme$cex.yaxis * 0.75,
      col = theme$col.yaxis,
      srt = 0, 
      pos = 2, 
      cex = theme$cex.yaxis, 
      xpd = TRUE
    )
    
    # draw dv lines ---------------------------------------------
    for(i in 1:length(design$values)) {
      x <- data[design$start[i]:design$stop[i], mvar]
      y <- data[design$start[i]:design$stop[i], dvar]
      if (!is.null(theme$col.line)) {
        lines(
          x, y, 
          type = "l", 
          lty = theme$lty.line, 
          lwd = theme$lwd.line,
          col = theme$col.line
        )
      }
      if (!is.null(theme$col.dots)) {
        points(
          x, y,
          pch = theme$pch, 
          cex = theme$cex.dots, 
          col = theme$col.dots
        )
      }
    }
  
    # add marks ---------------------------------------------------------------
    
    if (!is.null(object$marks)) {
      
      id_case <- which(sapply(object$marks, function(x) x$case) == case)
      id_case <- c(
        id_case, 
        which(sapply(object$marks, function(x) x$case) == "all")
      )
      
      if (length(id_case) > 0) {
        for (i_marks in id_case) { 
          marks_case <- object$marks[[i_marks]]
          
          if (is.character(marks_case$positions)) {
            marks_case$positions <- eval(
              str2expression(marks_case$positions), envir = data
            )
            marks_case$positions <- which(marks_case$positions)
          }
          
          marks.x <- data[data[, mvar] %in% marks_case$positions, mvar]
          marks.y <- data[data[, mvar] %in% marks_case$positions, dvar]
          points(
            x = marks.x, 
            y = marks.y,
            pch = marks_case$pch, 
            cex = marks_case$cex, 
            col = marks_case$col
          )
        }
      }
    }
    
    # add annotations ---------------------------------------------------------
    
    if (!is.null(theme$annotations)) {
      text(
        x = data[,mvar], 
        y = data[,dvar], 
        label = round(data[, dvar], theme$annotations$round), 
        col = theme$annotations$col, 
        pos = theme$annotations$pos, 
        offset = theme$annotations$offset, 
        cex = theme$annotations$cex
      )
    }
    
    # add lines ---------------------------------------------------------------
    
    if (!is.null(object$lines)) 
      .add_lines(data, mvar, dvar, pvar, design, object$lines)
  
    # add phase names ---------------------------------------------------------
    if (is.null(object$phase_names$labels)) {
      case_phase_names <- design$values
    } else {
      case_phase_names <- object$phase_names$labels
    }
    
    
    for(i in 1:length(design$values)) {
      mtext(
        text = case_phase_names[i], 
        side = 3, 
        at = (data[design$stop[i], mvar] - data[design$start[i], mvar]) / 2 + 
          data[design$start[i], mvar], 
        cex = theme$cex.phasenames,
        col = theme$col.phasenames
      )
    }
    
    # add line between phases -------------------------------------------
    if (is.null(object$label.seperators)) {
      for(i in 1:(length(design$values) - 1)) {
        x <- data[design$stop[i] + 1, mvar] - 0.5
        x <- c(x, x)
        
        if (theme$extent.seperators == "full") y <- par("usr")[3:4]
        if (theme$extent.seperators == "scale") y <- ylim
        if (is.numeric(theme$extent.seperators)) {
          .range <- ylim[2] - ylim[1]
          .cut <- .range * (1 - theme$extent.seperators) / 2
          y <- c(ylim[1] + .cut, ylim[2] - .cut)
        }
        
        lines(
          x, y, 
          lty = theme$lty.seperators,
          lwd =  theme$lwd.seperators, 
          col = theme$col.seperators,
          xpd = TRUE
        )
        
      }
    }
    
    if (!is.null(object$label.seperators)) {
      for(i in 1:(length(design$values) - 1)) {
        tex <- paste(unlist(strsplit(object$label.seperators[i], "")), collapse = "\n")
        text(
          x = data[design$stop[i] + 1, mvar] - 0.5, 
          y = (y_lim[2] - y_lim[1]) / 2 + y_lim[1], 
          labels = tex, 
          col = them$col.seperators,
          cex = theme$size.seperators
        )
      }
      
    }
    
    # add case name -----------------------------------------------------
    if (!is.null(object$case_names$labels)) {
      args <- c(
        list(text = object$case_names$labels[case]), 
        cex = theme$cex.casenames,
        col = theme$col.casenames,
        theme$names, 
        at = min(xlim)
      )
      args <- args[!duplicated(names(args))]
      do.call(mtext, args)
    }
    
    
    # add text ----------------------------------------------------------
    if (length(object$texts) > 0) {
      id_case <- which(sapply(object$texts, function(x) x$case) == case)
      if (length(id_case) > 0) {
        for(i in id_case) {
          #args <- object$texts[[i]]
          #args <- args[-which(names(args) == "case")]
          #do.call(text, args)
          text(
            object$texts[[i]]$x, 
            object$texts[[i]]$y, 
            object$texts[[i]]$label, 
            srt = object$texts[[i]]$angle, 
            col = object$texts[[i]]$col, 
            cex = object$texts[[i]]$cex
          )
        }
        
      }
    }
    
    # add arrows --------------------------------------------------------
    if (length(object$arrows) > 0) {
      id <- which(sapply(object$arrows, function(x) x$case) == case)
      if (length(id) > 0) {
        for(i in id) {
          args <- object$arrows[[i]]
          args <- args[-which(names(args) == "case")]
          do.call(arrows, args)
        }
        
      }
    }
    
  }
  
  # add title -----------------------------------------------------------
  
  if (!is.null(object$title)) {
    
    if (!is.null(theme$wrap.title)) {
      object$title <- paste(
        strwrap(object$title, width = theme$wrap.title),
        collapse = "\n"
      )
    }    
    
    adj <- NA
    
    if (theme$align.main == "center") adj <- NA
    if (theme$align.main == "left") adj <- 0 + theme$margin.main
    if (theme$align.main == "right") adj <- 1 - theme$margin.main
    
    if (theme$parse.main) object$title <- str2expression(object$title)
    
    mtext(
      object$title, 
      side = 3, 
      outer = TRUE, 
      line = 0,
      padj = 0,
      cex = theme$cex.main, 
      font = theme$font.main, 
      col = theme$col.main, 
      adj = adj 
    )
  }

  # add caption ---------------------------------------------------
  
  if (!is.null(object$caption)) {
    adj <- NA
    
    if (theme$align.caption == "center") adj <- NA
    if (theme$align.caption == "left") adj <- 0 + theme$margin.caption
    if (theme$align.caption == "right") adj <- 1 - theme$margin.caption
    
    mtext(
      object$caption, 
      side = 1, 
      line = -1, 
      outer = TRUE, 
      cex = theme$cex.caption, 
      font = theme$font.caption, 
      col = theme$col.caption, 
      adj = adj,
      padj = 1
    )
  }  
  
  # add box ouround figure -----------------------------------------------

  par(op)
  
  if (!is.null(theme$col.box)) {
    box(
      which = "figure", 
      lwd = theme$lwd.box, 
      lty = theme$lty.box, 
      col = theme$col.box
    )
  }
  

  
}

.add_lines <- function(data, mvar, dvar, pvar, design, lines) {

    for(i_lines in seq_along(lines)) {
      
      line <- lines[[i_lines]]
      
      if (is.null(line[["lty"]])) line[["lty"]] <- "dashed"
      if (is.null(line[["lwd"]])) line[["lwd"]] <- 2
      if (is.null(line[["col"]])) line[["col"]] <- "black"
      
      lty.line <- line[["lty"]]
      lwd.line <- line[["lwd"]]
      col.line <- line[["col"]]
      
      if (line[["stat"]] == "trend") {
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
      if (line[["stat"]] == "median") {
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
      if (line[["stat"]] == "mean") {
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
      if (line[["stat"]] == "trendA") {
        x <- data[design$start[1]:design$stop[1], mvar]
        y <- data[design$start[1]:design$stop[1], dvar]
        maxMT <- max(data[, mvar])
        reg <- lm(y~x)
        lines(
          x = c(min(x), maxMT), 
          y = c(
            reg$coefficients[1] + min(x) * reg$coefficients[2], 
            reg$coefficients[1] + maxMT * reg$coefficients[2]
          ), 
          lty = lty.line, 
          col = col.line, 
          lwd = lwd.line
        )
      }
      if (line[["stat"]] == "trendA_bisplit") {
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
            reg$coefficients[1] + min(x) * reg$coefficients[2], 
            reg$coefficients[1] + maxMT * reg$coefficients[2]
          ), 
          lty = lty.line, 
          col = col.line, 
          lwd = lwd.line
        )
      }
      if (line[["stat"]] == "trendA_trisplit") {
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
      if (line[["stat"]] == "loreg") {
        if (is.null(line[["f"]])) line[["f"]] <- 0.5
        lines.par <- line[["f"]]
        reg <- lowess(data[,dvar] ~ data[,mvar], f = lines.par)
        lines(reg, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      
      if (line[["stat"]] %in% c("maxA", "pnd")) {
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
      
      if (line[["stat"]] == "minA") {
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
      if (line[["stat"]] == "medianA") {
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
      if (line[["stat"]] == "meanA") {
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
      
      if (line[["stat"]] == "plm") {
        pr <- plm(data_list[case])
        y <- fitted(pr$full.model)
        lines(data[[mvar]], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      if (line[["stat"]] == "plm.ar") {
        if (is.null(line[["ar"]])) line[["ar"]] <-2
        lines.par <- line[["ar"]]
        pr <- plm(data_list[case], AR = lines.par)
        y <- fitted(pr$full.model)
        lines(data[[mvar]], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      
      if (line[["stat"]] == "movingMean") {
        if (is.null(line[["lag"]])) line[["lag"]] <- 1
        lines.par <- line[["lag"]]
        y <- .moving_average(data[, dvar],lines.par, mean)
        lines(data[, mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      if (line[["stat"]] == "movingMedian") {
        if (is.null(line[["lag"]])) line[["lag"]] <- 1
        lines.par <- line[["lag"]]
        y <- .moving_average(data[, dvar],lines.par, median)
        lines(data[, mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
    }
  
}

