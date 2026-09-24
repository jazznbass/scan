#' Percentage of all non-overlapping data
#'
#' Percentage of the measurements that do not have to be removed to separate
#' phase A from phase B (PAND; Parker, Hagan-Burke, & Vannest, 2007). Two
#' calculation methods are available, and they answer the question differently.
#'
#' @details `method = "sort"` follows the algorithm of Parker et al. (2007).
#'   The measurements of the series are sorted by their value, and the resulting
#'   order of the phases is compared with the original one in a two by two
#'   table. The original algorithm assigns ties randomly to one of the phases,
#'   which makes the result depend on chance; this implementation sorts without
#'   randomization instead. The table gives the effect size `phi` and allows a
#'   chi-squared and a Fisher exact test.
#'
#'   `method = "minimum"` applies the formulation Pustejovsky (2019) derived
#'   from the original definition of PAND: \deqn{PAND =
#'   \frac{1}{m+n}max\{(i+j)I(y^A_{i}<y^B_{n+1-j}\}} It reproduces the
#'   definition exactly, but its distribution under the null hypothesis is
#'   unknown, so no test is reported. Across several cases the non-overlapping
#'   measurements of each case are summed and divided by the total number of
#'   measurements. The code follows the `calc_PAND()` function of the
#'   `SingleCaseES` package.
#'
#'   The two methods do not give the same value for the same data: the sorting
#'   algorithm of the 2007 paper does not reproduce the definition given in that
#'   same paper.
#' @inheritParams .inheritParams
#' @param method Either `"sort"` or `"minimum"`, see the details.
#' @return An object of class `sc_pand` with the elements:
#'  |  |  |
#'  | --- | --- |
#'  | `pand` | Percentage of all non-overlapping data. |
#'  | `method` | Calculation method that was applied. |
#'  | `overlaps` | Number of overlapping measurements. |
#'  | `perc_overlap` | Percentage of overlapping measurements. |
#'  | `n` | Number of measurements. |
#'  | `n_a` | Number of measurements in phase A. |
#'  | `n_b` | Number of measurements in phase B. |
#'  | `N` | Number of cases. |
#'
#'   With `method = "sort"` additionally:
#'
#'  |  |  |
#'  | --- | --- |
#'  | `phi` | Effect size Phi based on expected and observed values. |
#'  | `matrix` | Two by two matrix of the phase proportions. |
#'  | `matrix_counts` | Two by two matrix of the phase counts. |
#'  | `chi_test` | Result of [chisq.test()]. |
#'  | `fisher_test` | Result of [fisher.test()]. |
#'
#'   With `method = "minimum"` additionally:
#'
#'  |  |  |
#'  | --- | --- |
#'  | `casewise` | PAND, non-overlapping measurements and phase lengths per case. |
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Parker, R. I., Hagan-Burke, S., & Vannest, K. (2007). Percentage
#'   of All Non-Overlapping Data (PAND): An Alternative to PND. *The Journal of
#'   Special Education, 40*, 194-204.
#'
#'   Pustejovsky, J. E. (2019). Procedural sensitivities of effect sizes for
#'   single-case designs with directly observed behavioral outcome measures.
#'   *Psychological Methods, 24*(2), 217-235.
#'   https://doi.org/10.1037/met0000179
#'
#'   Pustejovsky, J. E., Chen, M., & Swan, D. M. (2023). SingleCaseES: A
#'   Calculator for Single-Case Effect Sizes. R package version 0.7.1.9999,
#'   https://jepusto.github.io/SingleCaseES/
#' @examples
#' pand(exampleAB)
#'
#' # the example of Parker et al. (2007)
#' pand(Parker2007)
#'
#' # the definition of PAND instead of the sorting algorithm
#' pand(Parker2007, method = "minimum")
#'
#' # data that are expected to decrease in phase B
#' pand(exampleAB_decreasing, decreasing = TRUE)
#' @order 1
#' @export
pand <- function(data, dvar, pvar, 
                 decreasing = FALSE, 
                 phases = c(1, 2),
                 method = c("sort", "minimum")) {
  
  check_args(
    by_call(method)
  )
  
  # set default attributes
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data)
  
  dv(data) <- dvar
  phase(data) <- pvar
  
  data <- .prepare_scdf(data)
  data <- recombine_phases(data, phases = phases)$data
  
  for (i in seq_along(data)) {
    data[[i]] <- data[[i]][!is.na(data[[i]][[dvar]]), , drop = FALSE]
  }

  N_pre <- length(data)

  # removes cases with no data in either phase
  data <- data[sapply(data, function(x) {
    any(x[[pvar]] == "A") && any(x[[pvar]] == "B")
  })]

  N <- length(data)

  if (N == 0L) {
    abort("No cases with observations in both phases remain.")
  }

  if (N < N_pre) {
    warn(N_pre - N, " case(s) were removed from the analysis. ",
      "They had no data in either phase."
    )
  }

  values_a <- lapply(data, function(x) x[x[[pvar]] == "A", dvar])
  values_b <- lapply(data, function(x) x[x[[pvar]] == "B", dvar])
  n_all_a <- length(unlist(values_a))
  n_all_b <- length(unlist(values_b))
  n <- n_all_a + n_all_b
  
  if (method == "sort") {
    
    # phase order per case as found in data -----
    
    phases_data <- lapply(data, function(x) x[[pvar]]) |> unlist()
    
    # phase order when sorted by values within case ----
    phases_sorted <- lapply(data, function(x) {
      values <- if (decreasing) -x[[dvar]] else x[[dvar]]
      x[[pvar]][order(values, x[[pvar]])]
    }) |> unlist() 
    
    mat_counts <- table(phases_data, phases_sorted)
    mat_propotions <- prop.table(mat_counts)
    pand <- (mat_propotions[1,1] + mat_propotions[2,2]) * 100
    overlaps <- mat_counts[1,2] + mat_counts[2,1]
    perc_overlap <- overlaps / n * 100
    
    chi_test <- suppressWarnings(chisq.test(mat_counts, correct = FALSE))
    
    phi <- sqrt(chi_test$statistic / n)
    
    out <- list(
      pand = pand, 
      method = method,
      phi = unname(phi), 
      perc_overlap = perc_overlap, 
      overlaps = overlaps, 
      n = n, 
      N = N, 
      n_a = n_all_a, 
      n_b = n_all_b, 
      matrix = mat_propotions, 
      matrix_counts = mat_counts, 
      chi_test = chi_test,
      fisher_test = suppressWarnings(fisher.test(mat_counts)),
      decreasing = decreasing
    )
    
  }
  
  if (method == "minimum") {
    
    .pand_pustejowski <- function(values_a, values_b) {
      
      if (decreasing) {
        values_a <- -1 * values_a
        values_b <- -1 * values_b
      }
      n_a <- length(values_a)
      n_b <- length(values_b)
      x <- c(-Inf, sort(values_a))
      y <- c(sort(values_b), Inf)
      grid <- expand.grid(a = 1:(n_a + 1), b = 1:(n_b + 1))
      grid$no_overlap <- mapply(
        function(a, b) x[a] < y[b], 
        a = grid$a, 
        b = grid$b
      )
      grid$overlap <- grid$a + n_b - grid$b
      
      nonoverlaps <- max(grid$overlap * grid$no_overlap)
  
      list(
        pand = nonoverlaps/(n_a + n_b), 
        nonoverlaps = nonoverlaps, 
        length_a = n_a, 
        length_b = n_b
      )
    }
    
    casewise <- mapply(
      .pand_pustejowski, 
      values_a = values_a, 
      values_b = values_b,
      SIMPLIFY = FALSE, 
      USE.NAMES = TRUE
    )
    nonoverlaps <- lapply(casewise, function(x) x$nonoverlaps) |> 
      unlist() |> 
      sum()
    
    out <- list(
      pand = nonoverlaps / n * 100,
      overlaps = n - nonoverlaps,
      perc_overlap = 100 - (nonoverlaps / n * 100),
      n = n, 
      N = N, 
      n_a = n_all_a, 
      n_b = n_all_b, 
      casewise = casewise,
      method = method,
      decreasing = decreasing
    )
  }

  class(out) <- c("sc_pand")
  attributes(out)[opts("phase", "dv")] <- list(pvar, dvar)
  out
}



