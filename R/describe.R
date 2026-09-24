#' Descriptive statistics
#'
#' Number of measurements, missing values, mean, median, standard deviation,
#' median absolute deviation, minimum, maximum and trend, for each phase of
#' each case of a single-case data set.
#'
#' @details The statistics appear as columns named `<statistic>.<phase>`:
#'   `n`, `mis`, `m`, `md`, `sd`, `mad`, `min`, `max` and `trend`. `n` is the
#'   length of the phase and therefore includes the missing measurements that
#'   `mis` counts separately; all other statistics are computed from the
#'   observed measurements alone. `trend` is the regression weight of the
#'   dependent variable on the measurement time within that phase.
#'
#'   A phase name that occurs more than once in a case is numbered, so an ABAB
#'   design yields the phases `A(1)`, `B(1)`, `A(2)` and `B(2)` and hence the
#'   columns `m.A(1)`, `m.B(1)` and so on. The `Design` column, in contrast,
#'   shows the phase names as they are in the data. Cases with differing
#'   designs are put in the same table, so a case that lacks a phase of another
#'   case has `NA` in its columns.
#'
#'   A phase without any observed measurement leaves everything but `n` and
#'   `mis` at `NA`, a phase with a single observed measurement additionally
#'   leaves `sd` and `trend` at `NA`. Neither case is reported.
#' @inheritParams .inheritParams
#' @return An object of class `sc_desc` with the elements:
#'  |  |  |
#'  | --- | --- |
#'  | `descriptives` | Data frame with one row per case, the columns `Case` and `Design` and the statistics described above. |
#'  | `phase_names` | The phase names occurring across all cases, in the order in which the columns are arranged. |
#'  | `N` | Number of cases. |
#' @author Juergen Wilbert
#' @seealso [overlap()]
#' @examples
#' describe(exampleAB)
#'
#' # a design with more than two phases
#' describe(exampleABC)
#'
#' # write the statistics to a csv file
#' write.csv(describe(exampleAB)$descriptives, file = tempfile())
#' @order 1
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
  phase_names <- unique(unlist(designs))
  
  vars <- c("n", "mis", "m", "md", "sd", "mad", "min", "max", "trend")
  vars <- paste0(rep(vars, each = length(phase_names)), ".", phase_names)
  
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
      n_obs <- sum(!is.na(y))
      
      desc[case, paste0("n.", phase)] <- length(y)
      desc[case, paste0("mis.", phase)] <- sum(is.na(y))
      
      if (n_obs == 0) next
      
      desc[case, paste0("m.", phase)] <- mean(y, na.rm = TRUE)
      desc[case, paste0("md.", phase)] <- median(y, na.rm = TRUE)
      desc[case, paste0("sd.", phase)] <- sd(y, na.rm = TRUE)
      desc[case, paste0("mad.", phase)] <- mad(y, na.rm = TRUE)
      desc[case, paste0("min.", phase)] <- min(y, na.rm = TRUE)
      desc[case, paste0("max.", phase)] <- max(y, na.rm = TRUE)
      
      if (n_obs >= 2) {
        desc[case, paste0("trend.", phase)] <- 
          coef(lm(y ~ I(x - x[1] + 1), na.action = na.omit))[2]
      }
    }
  }

  out <- list(
    descriptives = desc,
    phase_names = phase_names,
    N = N
  )
  class(out) <- c("sc_desc")
  attributes(out)[opts("phase", "mt", "dv")] <- list(pvar, mvar, dvar)

  out
}
