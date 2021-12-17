#' Plot random distirubtion
#'
#' @param object Object returned from rand_test() function
#' @param ... 
#' @export
plot.sc_rand <- function(object, 
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
