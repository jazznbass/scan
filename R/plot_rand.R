#' Plot random distribution
#' 
#' This function takes the return of the rand_test function and creates a 
#' histogram with the distribution of the rand sample statistics.
#'
#' @param object Object returned from the rand_test() function
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param title Plot title.
#' @param text_observed Text for marking the number of observed statistic.
#' @param color Bar color.
#' @param ... Further arguments passed to the plot function.
#' @export
plot_rand <- function(object, 
                      xlab = NA, 
                      ylab = "Frequency", 
                      title = "Random distribution", 
                      text_observed = "observed", 
                      color = "lightgrey", ...) {
  
  h <- hist(object$distribution, plot = FALSE)
  lab <- paste0(round(h$counts / length(object$distribution) * 100, 0), "%")
  xlim <- c(min(h$breaks, na.rm = TRUE), max(h$breaks, na.rm = TRUE))
  ylim <- round(max(h$counts * 1.2))
  if (object$observed.statistic < xlim[1]) xlim[1] <- object$observed.statistic
  if (object$observed.statistic > xlim[2]) xlim[2] <- object$observed.statistic
  
  if (is.na(xlab)) xlab <- object$statistic
  hist(
    object$distribution,
    xlab = xlab,
    labels = lab,
    xlim = xlim,
    ylim = c(0, ylim),
    ylab = ylab,
    main = title,
    col = color
  )
  abline(v = object$observed.statistic, lty = 2, lwd = 2, col = "grey")
  if (object$p.value < 0.5) pos <- 2 else pos <- 4
  text(object$observed.statistic, ylim, text_observed, pos = pos)
}
