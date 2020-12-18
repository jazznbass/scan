#' Aggregate multiple single-cases into one case
#' 
#' The makesingleSC function combines multiple single-case data frames into one
#' single-case data frame.
#' 
#' The algorithm works the following way: \enumerate{ \item All values of each
#' single-case are centred with respect to each case's phase A mean.  \item The
#' phase A values of all single-cases are combined in ascending order of their
#' measurement times.  \item The phase B values of all single-cases are combined
#' in ascending order of their measurement times.  \item Phase B values are
#' appended to phase A values. The measurement times of phase B are shifted to
#' start with the next MT after the end of phase A. }
#' 
#' @param data A vector with measurements, a data frame or a list of data
#' frames.
#' @param scale Unused
#' @param type By default values with the same measurement are added. If type
#' is set to "mean" or "median", values of the same measurement are replaced
#' with their mean or median. Default is "add".
#' @author Juergen Wilbert
#' @seealso \code{\link{scdf}}, \code{\link{longSCDF}},
#' \code{\link{writeSC}}
#' @keywords manip
#' @examples
#' 
#' ##Function deprecated
#' ## please do not use it!
#' @export
makesingleSC <- function(data, scale = FALSE, type = "add") {
  warning(.opt$function_deprecated_warning)
  data <- .SCprepareData(data, change.var.phase = TRUE, change.var.values = TRUE, change.var.mt = TRUE)
  N <- length(data)
  for(i in 1:N) {
    m  <- mean(data[[i]][data[[i]][,1] == "A",2], na.rm = TRUE)
    if (!scale) 
      sd <- 1
    if (scale) 
      sd <-  sd(data[[i]][data[[i]][,1] == "A",2], na.rm = TRUE)
    data[[i]][,2] <- (data[[i]][,2] - m) / sd 
    A <- data[[i]][data[[i]][,1] == "A",]	
    B <- data[[i]][data[[i]][,1] == "B",]	
    B[,3] <- B[,3] - min(B[,3], na.rm = TRUE) + 1
    if(i == 1) {
      new.data.A <- A
      new.data.B <- B	
    }
    if(i > 1) {
      new.data.A <- rbind(new.data.A, A)	
      new.data.B <- rbind(new.data.B, B)	
    }
  }
  new.data.A <- new.data.A[sort.list(new.data.A[[3]]),]
  if(type == "mean") {
    tmp <- aggregate(values~mt, data = new.data.A, mean, na.rm = TRUE)
    new.data.A <- data.frame(phase = rep("A", nrow(tmp)), values = tmp$values, mt = tmp$mt)
    tmp <- aggregate(values~mt, data = new.data.B, mean, na.rm = TRUE)
    new.data.B <- data.frame(phase = rep("B", nrow(tmp)), values = tmp$values, mt = tmp$mt)
  }
  if(type == "median") {
    tmp <- aggregate(values~mt, data = new.data.A, median, na.rm = TRUE)
    new.data.A <- data.frame(phase = rep("A", nrow(tmp)), values = tmp$values, mt = tmp$mt)
    tmp <- aggregate(values~mt, data = new.data.B, median, na.rm = TRUE)
    new.data.B <- data.frame(phase = rep("B", nrow(tmp)), values = tmp$values, mt = tmp$mt)
  }
  maxA <- max(new.data.A[,3], na.rm = TRUE)
  new.data.B[,3] <- new.data.B[,3] + maxA
  new.data.B <- new.data.B[sort.list(new.data.B[[3]]),]
  new.data <- rbind(new.data.A, new.data.B)
  
  return(list(new.data))
}
