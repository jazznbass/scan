#' Descriptive statistics for single-case data
#'
#' The `describe()` function provides common descriptive statistics for
#' single-case data.
#'
#'
#' @inheritParams .inheritParams
#' @details n = number of measurements; mis = number of missing vaues; m = mean;
#'   md = median; sd = standard deviation; mad = median average deviation; min =
#'   minimum; max = maximum; trend = weight of depended variable regressed on
#'   time (values ~ mt).
#' @return A list containing a data frame of descriptive statistics
#'   (descriptives); the cse design (design); the number of cases (N)
#' @author Juergen Wilbert
#' @seealso [overlap()], [plot.scdf()]
#' @examples
#'
#' ## Descriptive statistics for a study of three single-cases
#' describe(Grosche2011)
#'
#' ## Descriptives of a three phase design
#' describe(exampleABC)
#'
#' ## Write descriptive statistics to .csv-file
#' study <- describe(Waddell2011)
#' write.csv(study$descriptives, file = tempfile())
#' @export
describe <- function(data, dvar, pvar, mvar) {

  # set defaults attributes
  if (missing(dvar)) dvar <- dv(data) 
  if (missing(pvar)) pvar <- phase(data) 
  if (missing(mvar)) mvar <- mt(data) 
  
  dv(data) <- dvar
  phase(data) <- pvar
  mt(data) <- mvar
  
  data_list <- .prepare_scdf(data)
  N <- length(data_list)

  designs <- lapply(
    data_list, 
    function(x) rle(as.character(x[[pvar]]))$values
  )
  phase_designs <- sapply(designs, function(x) paste0(x, collapse = "-"))
  
  for(i in 1:N) {
    data_list[[i]][[pvar]] <- rename_phase_duplicates(data_list[[i]][[pvar]])
  }
  
  designs <- lapply(
    data_list, 
    function(x) rle(as.character(x[[pvar]]))$values
  )
  design <- unique(unlist(designs))
  
  vars <- c("n", "mis", "m", "md", "sd", "mad", "min", "max", "trend")
  vars <- paste0(rep(vars, each = length(design)), ".", design)
  
  desc <- as.data.frame(matrix(nrow = N, ncol = length(vars)))
  colnames(desc) <- vars
  desc <- data.frame(
    Case = names(data_list), 
    Design = phase_designs, 
    desc,
    check.names = FALSE
  )

  for (case in 1:N) {
    data <- data_list[[case]]
    phases <- .phasestructure(data, pvar = pvar)
    for (i in 1:length(phases$values)) {

      x <- data[[mvar]][phases$start[i]:phases$stop[i]]
      y <- data[[dvar]][phases$start[i]:phases$stop[i]]

      phase <- phases$values[i]
      desc[case, paste0("n.", phase)] <- length(y)
      desc[case, paste0("mis.", phase)] <- sum(is.na(y), na.rm = TRUE)
      desc[case, paste0("m.", phase)] <- mean(y, na.rm = TRUE)
      desc[case, paste0("md.", phase)] <- median(y, na.rm = TRUE)
      desc[case, paste0("sd.", phase)] <- sd(y, na.rm = TRUE)
      desc[case, paste0("mad.", phase)] <- mad(y, na.rm = TRUE)
      desc[case, paste0("min.", phase)] <- min(y, na.rm = TRUE)
      desc[case, paste0("max.", phase)] <- max(y, na.rm = TRUE)
      desc[case, paste0("trend.", phase)] <- coef(lm(y ~ I(x - x[1] + 1)))[2]
    }
  }

  out <- list(
    descriptives = desc,
    design = design,
    N = N
  )
  class(out) <- c("sc_desc")
  attributes(out)[opts("phase", "mt", "dv")] <- list(pvar, mvar, dvar)

  out
}
