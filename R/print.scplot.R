#' @export
print.scplot <- function(object) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  data_list <- object$scdf
  pvar <- object$pvar
  mvar <- object$mvar

  theme <- .check_theme(object$theme)
  N <- length(data_list)
  
  # set x/y label  ---------
  
  if (is.null(object$xlabel)) {
    object$xlabel <- mvar
    if (object$xlabel == "mt") object$xlabel <- "Measurement time"
    object$xlabel <- .upperfirst(object$xlabel)
  }
  if (is.null(object$ylabel)) {
    if (length(object$dvar) == 1) {
      object$ylabel <- object$dvar
      object$ylabel <- .upperfirst(object$ylabel)
    } else {
      object$ylabel <- "Values"
    }
  }

  # set outer margin --------------------------------------------------------

  if (!is.null(object$title)) {
    tmp_chars <- sum(unlist(strsplit(object$title, "")) == "\n") 
    theme$oma[3] <- theme$oma[3] + 1 + tmp_chars
  }
  if (!is.null(object$caption)) {
    if (!is.null(theme$caption.wrap)) {
      object$caption <- paste(
        strwrap(object$caption, width = theme$caption.wrap),
        collapse = "\n"
      )
    }    
    if (theme$caption.parse) object$caption <- str2expression(object$caption)
    tmp_chars <- sum(unlist(strsplit(object$caption, "")) == "\n")
    theme$oma[1] <- theme$oma[1] + 1 + tmp_chars
  }
  
  # set par --------
  
  if (N > 1) par(mfrow = c(N, 1))
  
  par("oma" = theme$oma)
  
  if (!is.null(theme$plot.background.fill)) par("bg" = theme$plot.background.fill)
  par("family" = theme$font)
  par("cex" = 1) # or 1
  
  # set dataline for first dvar
  
  if (is.null(object$datalines[[1]]$variable)) object$datalines[[1]]$variable <- object$dvar[1]
  if (is.null(object$datalines[[1]]$col)) object$datalines[[1]]$col <- theme$dataline.col
  if (is.null(object$datalines[[1]]$width)) object$datalines[[1]]$width <- theme$dataline.width
  if (is.null(object$datalines[[1]]$linetype)) object$datalines[[1]]$linetype <- theme$dataline.linetype
  if (is.null(object$datalines[[1]]$dots)) object$datalines[[1]]$dots <- theme$datadots.col
  if (is.null(object$datalines[[1]]$shape)) object$datalines[[1]]$shape <- theme$datadots.shape
  if (is.null(object$datalines[[1]]$size)) object$datalines[[1]]$size <- theme$datadots.size
  
  # set global xlim and ylim ---------
  
  ylim <- object$yaxis$lim
  xlim <- object$xaxis$lim
  
  if (is.null(ylim)) {
    .dv <- unlist(lapply(data_list, function(x) x[, object$dvar]))
    ylim <- c(min(.dv, na.rm = TRUE), max(.dv, na.rm = TRUE))
  }
  
  if (is.null(xlim)) {
    .mt     <- unlist(lapply(data_list, function(x) x[, mvar]))
    xlim <- c(min(.mt, na.rm = TRUE), max(.mt, na.rm = TRUE))
  }

  # set left margin --------
  
  fac_i_to_l <- 5
  
  # w_axis <- strwidth(
  #  as.character(ylim[2]), 
  #  units = "inches", 
  #  cex = theme$yaxis.text.size
  # ) * fac_i_to_l
  # 
  
  w_axis <- 1
  
  w_ticks <- strwidth(
    "W", units = "inches", cex = theme$yaxis.text.size * 0.5 * fac_i_to_l
  )
  
  # Horizontal
  if (theme$yaxis.title.angle == 1) {
    w_label <- strwidth(
      object$ylabel, 
      units = "inches", 
      cex = theme$yaxis.title.size
    ) * fac_i_to_l
  }
  
  # Vertical
  if (theme$yaxis.title.angle == 0) {
    w_label <- theme$yaxis.title.size * (sum(unlist(strsplit(object$ylabel, "")) == "\n") + 1)
  }
  
  #cat(w_label, w_axis, w_ticks, sep = ", ")
  theme$mar[2] <- theme$mar[2] + w_label + w_axis + w_ticks + 
                  0.5 * theme$yaxis.title.size
  

  # set lower margin ----------------------------------------------------

  h_axis <- theme$xaxis.text.size
  h_ticks <- 0.5 * theme$xaxis.text.size
  h_label <- (sum(unlist(strsplit(object$xlabel, "")) == "\n") + 1) 
  
  theme$mar[1] <- theme$mar[1] + h_axis + h_ticks + h_label
  
  # Plotting cases ----------------------------------------------------------
  
  for(case in 1:N) {
    data <- data_list[[case]]
    design <- .phasestructure(data, pvar)
    
    # plot ylim
    y_lim <- ylim
    if (is.na(ylim[2])) y_lim[2] <- max(data[, object$dvar])
    if (is.na(ylim[1])) y_lim[1] <- min(data[, object$dvar])
    
    #if (case == N) par(mar = theme$mar)
    #if (case != N) par(mar = c(0, theme$mar[2:4]))
    par(mar = theme$mar)

    plot(
      data[[mvar]], data[[object$dvar[1]]], 
      type = "n", 
      xlim = xlim, ylim = y_lim, 
      ann = FALSE,
      xaxp = c(xlim[1], xlim[2], xlim[2] - xlim[1]),
      yaxt = "n", xaxt = "n", 
      bty = "n"
    )
    
    # add xlab --------------------------------------------------------
    
    if (N == 1 || case == N) {
      
      x <- (par("usr")[2] - par("usr")[1]) / 2 + par("usr")[1]
      y <- .line_to_xy(theme$mar[1] - 1.1 + theme$xaxis.title.vjust, side = 1)
     
      text(
        x = x, 
        y = y,
        col = theme$xaxis.title.col, 
        cex = theme$xaxis.title.size,
        labels = object$xlabel,
        adj = c(0.5,0),
        xpd = NA
      )
      
      # mtext(
      #   object$xlabel,
      #   side = 1,
      #   line = theme$mar[1] - 1.1 + theme$xaxis.title.vjust,
      #   padj = 0, # 
      #   las = 0,
      #   cex = theme$xaxis.title.size,
      #   col = theme$xaxis.title.col
      # )
    }
    
    # add ylab -------------------------------------------------------

    if (theme$yaxis.title.angle == 0) 
      mtext(
        text = object$ylabel, 
        side = 2, 
        line = theme$mar[2] - 0.4 + theme$yaxis.title.hjust,
        las = 0, 
        adj = 0.5,
        padj = 1,
        cex = theme$yaxis.title.size, 
        col = theme$yaxis.title.col
      )
    
    if (theme$yaxis.title.angle == 1) {
      mtext(
        text = object$ylabel, 
        side = 2, # left
        line = theme$mar[2] - 0.2 + theme$yaxis.title.hjust, # start 0.2 right of the margin
        las = 1, # horizontal print
        adj = 0, # left aligned
        padj = 1, # text below the "at" position
        at = par("usr")[4], 
        cex = theme$yaxis.title.size, 
        col = theme$yaxis.title.col
      )
    }
    
    
    usr <- par("usr")
    
    # add inner background -------------------------------------------------------
    
    if (!is.null(theme$panel.col)){

      type_phases <- unique(design$values)
      col <- rep(theme$panel.col, length = length(type_phases))
      
      for(i in seq_along(design$values)) {
        x <- data[design$start[i]:design$stop[i], mvar]
        
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

    if (!is.null(theme$grid.col)) 
      grid(
        NULL, NULL, 
        col = theme$grid.col, 
        lty = theme$grid.linetype, 
        lwd = theme$grid.width
      )
    
    # add ridge --------------------------------
    
    if (!is.null(theme$ridge.col)) {
      for(i in 1:length(design$values)) {
        x <- data[design$start[i]:design$stop[i], mvar]
        y <- data[design$start[i]:design$stop[i], object$dvar[1]]
        
        for(i in 1:length(x)) {
          x_values <- c(x[i], x[i + 1], x[i + 1], x[i])
          #y_values <- c(y_lim[1], y_lim[1], y[i + 1], y[i])
          y_values <- c(par("usr")[3], par("usr")[3], y[i + 1], y[i])
          polygon(x_values, y_values, col = theme$ridge.col, border = NA)      
        }
      }
    }
    
    # add frame ---------------------------------
    
    if (!is.null(theme$panel.frame.col)){
      rect(
        usr[1],usr[3],usr[2],usr[4], 
        col = NA, 
        border = theme$panel.frame.col,
        lwd = theme$panel.frame.width,
        lty = theme$panel.frame.linetype
      )
    }
    # add xaxis --------------------------------------------------------
    
    if (case == N) { #only when one plot or last plot
      xticks_pos <- seq(xlim[1], xlim[2], 1)
      axis(
        side = 1, 
        at = xticks_pos, 
        labels = FALSE, 
        col = theme$xaxis.line.col, 
        col.ticks = theme$xaxis.ticks.col,
        tcl = theme$xaxis.ticks.length * theme$xaxis.text.size
      )
      
      if (!is.null(object$xaxis$inc_from)) {
        x <- seq(object$xaxis$inc_from, xlim[2], object$xaxis$inc)
        x <- x[x >= xlim[1]]
        x <- c(1, x)
      } else {
        x <- seq(xlim[1], xlim[2], object$xaxis$inc)
      }
      
      text(
        x = x,  
        y = par("usr")[3], 
        cex = theme$xaxis.text.size, 
        col = theme$xaxis.text.col,
        labels = x, 
        srt = theme$xaxis.text.angle, 
        pos = 1, 
        offset = theme$xaxis.text.size * 0.75, 
        xpd = TRUE
      )
    }
    
    # add yaxis -------------------------------------------------------
    
    yticks_pos <- axTicks(2, usr = c(y_lim[1], y_lim[2]))
    y <- axis(
      side = 2, 
      at = yticks_pos, 
      labels = NA, 
      col = theme$yaxis.line.col, 
      col.ticks = theme$yaxis.ticks.col,
      tcl = theme$xaxis.ticks.length * theme$xaxis.text.size
    )
    
    if (!is.null(object$yaxis$inc)) {
      if (!is.null(object$yaxis$inc_from)) {
        y <- seq(object$yaxis$inc_from, y_lim[2], object$yaxis$inc)
        y <- y[y >= y_lim[1]]
      } else {
        y <- seq(y_lim[1], y_lim[2], object$yaxis$inc)
      }
    }
    
    text(
      x = par("usr")[1],
      y = y, 
      labels = y, 
      offset = theme$yaxis.text.size * 0.75,
      col = theme$yaxis.text.col,
      srt = theme$yaxis.text.angle, 
      pos = 2, 
      cex = theme$yaxis.text.size, 
      xpd = TRUE
    )
    
    # draw dv lines ---------------------------------------------
    
    for(i in seq_along(object$datalines)) {
      .draw_data(data, mvar, design, object$datalines[[i]])
    }
    
    # add marks ---------------------------------------------------------------
    
    if (!is.null(object$marks)) {
      
      id_case <- c(
        which(sapply(object$marks, function(x) x$case) == case), 
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
      
          if (marks_case$variable == ".dvar") marks_case$variable <- object$dvar[1]
          
          marks.x <- data[data[, mvar] %in% marks_case$positions, mvar]
          marks.y <- data[data[, mvar] %in% marks_case$positions, marks_case$variable] #object$dvar[1]]
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
    
    # add annotations /value labels ------------------------------------------
    
    if (!is.null(theme$labels.col)) {
      
      for(i in 1:length(object$dvar)) {
        
        x <- data[, mvar] + 
             theme$labels.nudge_x * strwidth("M", cex = theme$labels.size)
        y <- data[, object$dvar[i]] + 
             theme$labels.nudge_y * strheight("M", cex = theme$labels.size)
        
        label <- round(data[, object$dvar[i]], theme$labels.round)
        adj <- c(theme$labels.hjust, theme$labels.vjust)
        
        if (!is.null(theme$labels.box.fill) || !is.null(theme$labels.box.col)) {

          .add_text_box(
            text = label, 
            x = x, 
            y = y, 
            adj = adj,
            cex = theme$labels.size,
            fill = theme$labels.box.fill,
            col = theme$labels.box.col,
            lwd = theme$labels.box.width,
            lty = theme$labels.box.type,
            margin = theme$labels.box.margin
          ) 
        }
        
        text(
          x = x, 
          y = y, 
          label = label, 
          col = theme$labels.col, 
          adj = adj,
          cex = theme$labels.size,
          font = theme$labels.face,
          family = theme$labels.family,
          xpd = TRUE
        )
      }
    }
    
    # add statlines ------------------------------------------------------------
    
    if (!is.null(object$statlines)) {
      object$statlines <- .prepare_arg_lines(object$statlines)
      
      for(i in 1:length(object$dvar)) {
        for(j in 1:length(object$statlines)) {
          if (object$statlines[[j]]$variable == ".dvar") 
            object$statlines[[j]]$variable <- object$dvar[1]
          if (object$statlines[[j]]$variable == object$dvar[i])
            .add_lines(
              data, mvar, object$dvar[i], pvar, design, object$statlines[[j]]
            )
        }
      }
      
      
    }
    # add phasenames ---------------------------------------------------------

    if (identical(object$phasenames$labels, ".default")) {
      case_phase_names <- design$values
    } else {
      case_phase_names <- object$phasenames$labels
    }
    
    for(i in 1:length(design$values)) {
      
      adj <- c(0.5, 0)
      
      if (theme$phasenames.position.x == "centre") {
        x <- (data[design$stop[i], mvar] - data[design$start[i], mvar]) / 2 + 
              data[design$start[i], mvar]
        adj[1] <- 0.5
      }
      
      if (theme$phasenames.position.x == "left") {
        x <- data[design$start[i], mvar]
        adj[1] <- 0
      }
      
      y <- .yppt(theme$phasenames.position.y)
      
      if (identical(theme$phasenames.position.y, "above")) {
        y <- .yppt(1) + strheight("M", cex = theme$phasenames.size) * 0.2
        adj[2] <- 0
      }
      
      if (identical(theme$phasenames.position.y, "top")) {
        y <- .yppt(1) - strheight("M", cex = theme$phasenames.size) * 0.4
        adj[2] <- 1
      }      
      
      
      if (!is.null(theme$phasenames.box.fill) || !is.null(theme$phasenames.box.frame)) {
        .add_text_box(
          case_phase_names[i], x, y, adj,
          cex = theme$phasenames.size,
          fill = theme$phasenames.box.fill,
          col = theme$phasenames.box.col,
          lwd = theme$phasenames.box.width,
          lty = theme$phasenames.box.type,
          margin = theme$phasenames.box.margin
         ) 
      }
      
      text(
        x = x, 
        y = y, 
        label = case_phase_names[i], 
        col = theme$phasenames.col, 
        cex = theme$phasenames.size,
        adj = adj,
        xpd = NA
      )
      
    }
    
    # add line between phases -------------------------------------------
    if (is.null(object$seperators$label)) {
      for(i in 1:(length(design$values) - 1)) {
        x <- data[design$stop[i] + 1, mvar] - 0.5
        x <- c(x, x)
        
        if (theme$seperators.extent == "full") y <- par("usr")[3:4]
        if (theme$seperators.extent == "scale") y <- ylim
        if (is.numeric(theme$seperators.extent)) {
          .range <- ylim[2] - ylim[1]
          .cut <- .range * (1 - theme$seperators.extent) / 2
          y <- c(ylim[1] + .cut, ylim[2] - .cut)
        }
        
        lines(
          x, y, 
          lty = theme$seperators.linetype,
          lwd =  theme$seperators.width, 
          col = theme$seperators.col,
          xpd = TRUE
        )
        
      }
    }
    
    if (!is.null(object$seperators$label)) {
      for(i in 1:(length(design$values) - 1)) {
        tex <- paste(unlist(strsplit(object$seperators$label[i], "")), collapse = "\n")
        text(
          x = data[design$stop[i] + 1, mvar] - 0.5, 
          y = (y_lim[2] - y_lim[1]) / 2 + y_lim[1], 
          labels = tex, 
          col = them$seperators.col,
          cex = theme$seperators.size
        )
      }
      
    }
    
    # add casename -----------------------------------------------------
    if (!is.null(object$casenames$labels)) {
      
      if (!is.null(theme$casenames.position.x)) {
        x <- theme$casenames.position.x
      } else {
        x <- .xppt(0.02) #x <- xlim[1]
      }

      if (!is.null(theme$casenames.position.y)) {
        y <- theme$casenames.position.y
      } else {
        y <- .yppt(1) - strheight("M", cex = theme$casenames.size) * 0.5
      }
      
      
      text(
        x = x,
        y = y,
        labels = object$casenames$labels[case],
        col = theme$casenames.col,
        cex = theme$casenames.size,
        adj = c(0, 1),
        xpd = NA
      )
      
    }
    
    
    # add text ----------------------------------------------------------
    
    if (length(object$texts) > 0) {
      id_case <- which(sapply(object$texts, function(x) x$case) == case)
      if (length(id_case) > 0) {
        for(i in id_case) {
          text(
            object$texts[[i]]$x, 
            object$texts[[i]]$y, 
            object$texts[[i]]$label, 
            srt = object$texts[[i]]$angle, 
            col = object$texts[[i]]$col, 
            cex = object$texts[[i]]$cex,
            xpd = NA
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
    
    # add legend -----------
    
    if (!is.null(object$legend)) {
      
        if (theme$legend.position.case == case) {
        
        if (is.null(theme$legend.position.x)) {
          x <- .xppt(1.02)
        } else {
          x <- theme$legend.position.x
        }
        
        if (is.null(theme$legend.position.y)) {
          y <- .yppt(1)
        } else {
          y <- theme$legend.position.y
        }
          
        length <- length(object$datalines)
          
        if (!identical(object$legend$labels, ".default")) {
          legend_labels <- object$legend$labels
        } else {
          legend_labels <- .upperfirst(sapply(object$datalines, function(x) x$variable))
        }
          
          legend_pch <- sapply(object$datalines, function(x) x$shape)
          legend_col <- sapply(object$datalines, function(x) x$col)
          legend_pt_bg <- sapply(object$datalines, function(x) x$dots)
          legend_pt_cex <- sapply(object$datalines, function(x) x$size)
          legend_lwd <- sapply(object$datalines, function(x) x$width)
          legend_lty <- sapply(object$datalines, function(x) x$linetype)
          
          if (isTRUE(object$legend$statlines)){ 
            legend_labels <- c(
              legend_labels, 
              paste(
                .upperfirst(sapply(object$statlines, function(x) x$stat)),
                .upperfirst(sapply(object$statlines, function(x) x$variable))
              )
            )
            legend_col <- c(legend_col, sapply(object$statlines, function(x) x$col))
            legend_pt_cex <- c(legend_pt_cex, rep(0, length(object$statlines)))
            legend_pt_bg <- c(legend_pt_bg, rep("white", length(object$statlines)))
            legend_lwd <- c(legend_lwd, sapply(object$statlines, function(x) x$width))
            legend_lty <- c(legend_lty, sapply(object$statlines, function(x) x$linetype))
          }
          
          legend(
            x = x,
            y = y, 
            legend = legend_labels,
            xpd = NA, 
            pch = legend_pch,
            pt.bg = legend_pt_bg,
            col = legend_col,
            pt.cex = legend_pt_cex,
            cex = theme$legend.text.size,
            lty = legend_lty,
            lwd = legend_lwd,
            text.col = theme$legend.text.col,
            bg = theme$legend.bg.col,
            title = object$legend$title,
            seg.len = theme$legend.line.length
          )
      }  
    }
    
  } # end plot case loop
  
  # add title -----------------------------------------------------------
  
  if (!is.null(object$title)) {
    
    if (!is.null(theme$wrap.title)) {
      object$title <- paste(
        strwrap(object$title, width = theme$wrap.title),
        collapse = "\n"
      )
    }    
    
    adj <- NA
    
    if (theme$title.align == "center") adj <- NA
    if (theme$title.align == "left") adj <- 0 + theme$title.margin
    if (theme$title.align == "right") adj <- 1 - theme$title.margin
    
    if (theme$title.parse) object$title <- str2expression(object$title)
    
    mtext(
      object$title, 
      side = 3, 
      outer = TRUE, 
      line = 0,
      padj = 0,
      cex = theme$title.size, 
      font = theme$title.face, 
      col = theme$title.col, 
      adj = adj 
    )
  }

  # add caption ---------------------------------------------------
  
  if (!is.null(object$caption)) {
    
    adj <- NA
    
    if (theme$caption.align == "center") adj <- NA
    if (theme$caption.align == "left") adj <- 0 + theme$caption.margin
    if (theme$caption.align == "right") adj <- 1 - theme$caption.margin
    
    mtext(
      object$caption, 
      side = 1, 
      line = -1, 
      outer = TRUE, 
      cex = theme$caption.size, 
      font = theme$caption.face, 
      col = theme$caption.col, 
      adj = adj,
      padj = 1
    )
  }  
  
  # add box ouround figure -----------------------------------------------

  par(op)
  
  if (!is.null(theme$plot.background.col)) {
    box(
      which = "figure", 
      lwd = theme$plot.background.width, 
      lty = theme$plot.background.linetype,
      col = theme$plot.background.col
    )
  }
  

  
}

.add_lines <- function(data, mvar, dvar, pvar, design, line) {

    if (is.null(line[["linetype"]])) line[["linetype"]] <- "dashed"
    if (is.null(line[["width"]])) line[["width"]] <- 2
    if (is.null(line[["col"]])) line[["col"]] <- "black"
    
    lty.line <- line[["linetype"]]
    lwd.line <- line[["width"]]
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

.draw_data <- function(data, mvar, design, line) {

  for(i in 1:length(design$values)) {
    x <- data[design$start[i]:design$stop[i], mvar]
    y <- data[design$start[i]:design$stop[i], line$variable]
    if (!is.null(line$col)) {
      lines(
        x, y, 
        type = "l", 
        lty = line$linetype, 
        lwd = line$width,
        col = line$col
      )
    }
    if (!is.null(line$dots)) {
      points(
        x, y,
        pch = line$shape, 
        cex = line$size, 
        col = line$dots
      )
    }
  }  
}

.upperfirst <- function(x) {
  
  unlist(
    lapply(x, function(x) 
      paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
    )
  )
  
}

.yppt <- function(p, max = par("usr")[4], min = par("usr")[3]) {
  
  if (identical(p, "top")) return(par("usr")[4])
  if (identical(p, "above")) return(par("usr")[4])
  if (identical(p, "bottom")) return(par("usr")[3])
  
  min + (max - min) * p
}

.xppt <- function(p, max = par("usr")[2], min = par("usr")[1]) {
  
  if (identical(p, "left")) return(par("usr")[1])
  if (identical(p, "right")) return(par("usr")[2])
  min + (max - min) * p
}

.add_text_box <- function(text, cex  = 1, x, y, adj, margin = 0.5, 
                          lwd = "1", 
                          lty = "solid", 
                          fill = "white", 
                          col = "black") {
  
  margin <- rep_len(margin, 4)
  mar <- margin
  
  for(i in seq_along(text)) {
    
    width   <- strwidth(text[i], cex = cex)
    height  <- strheight(text[i], cex = cex)
    
    margin[c(1, 3)] <- height * mar[c(1, 3)]
    margin[c(2, 4)] <- strwidth("M", cex = cex) * mar[c(2, 4)]
    
    if (adj[1] == 0.5) {
      x1 <- x[i] - width / 2 - margin[2]
      x2 <- x[i] + width / 2 + margin[4]    
    }
    
    if (adj[1] == 0) {
      x1 <- x[i] - margin[2]
      x2 <- x[i] + width + margin[4]    
    }
    
    if (adj[1] == 1) {
      x1 <- x[i] - margin[2] - width
      x2 <- x[i] + margin[4]    
    }
    
    if (adj[2] == 0.5) {
      y1 <- y[i] - height / 2 - margin[3]
      y2 <- y[i] + height / 2 + margin[1] 
    }
    
    if (adj[2] == 0) {
      y1 <- y[i] - margin[1]
      y2 <- y[i] + height + margin[3] 
    }
    
    if (adj[2] == 1) {
      y1 <- y[i] - height - margin[1]
      y2 <- y[i] + margin[3] 
    }
    
    if (is.null(col)) col <- NA
    
    rect(
      x1, y1, x2, y2,
      col = fill,
      border = col,
      lwd = lwd,
      lty = lty,
      xpd = NA
    )
    
  }
}

.line_to_xy <- function(line, side = side) {
  if (side == 1) line <- par('usr')[3] - (par('cxy')[2] * (line + 1))
  if (side == 3) line <- par('usr')[4] + (par('cxy')[2] * (line + 1))
  if (side == 2) line <- par('usr')[1] - (par('cxy')[1] * (line + 1))
  if (side == 4) line <- par('usr')[2] + (par('cxy')[1] * (line + 1))
  line
}
