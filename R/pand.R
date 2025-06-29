#'Percentage of all non-overlapping data
#'
#'The `pand()` function calculates the percentage of all non-overlapping data
#'(PAND; Parker, Hagan-Burke, & Vannest, 2007), an index to quantify a level
#'increase (or decrease) in performance after the onset of an intervention.
#'
#'PAND was proposed by Parker, Hagan-Burke, and Vannest in 2007. The authors
#'emphasize that PAND is designed for application in a multiple case design with
#'a substantial number of measurements, technically at least 20 to 25, but
#'preferably 60 or more. PAND is defined as 100% minus the percentage of data
#'points that need to be removed from either phase in order to ensure nonoverlap
#'between the phases. Several approaches have been suggested to calculate PAND,
#'leading to potentially different outcomes. In their 2007 paper, Parker and
#'colleagues present an algorithm for computing PAND. The algorithm involves
#'sorting the scores of a time series, including the associated phases, and
#'comparing the resulting phase order with the original phase order using a
#'contingency table. To account for ties, the algorithm includes a randomization
#'process where ties are randomly assigned to one of the two phases.
#'Consequently, executing the algorithm multiple times could yield different
#'results. It is important to note that this algorithm does not produce the same
#'results as the PAND definition provided earlier in the same paper. However, it
#'offers the advantage of allowing the calculation of an effect size measure
#'`phi`, and the application of statistical tests for frequency distributions.
#'Pustejovsky (2019) presented a mathematical formulation of Parker's original
#'definition for comparing two phases of a single case: \deqn{PAND =
#'\frac{1}{m+n}max\{(i+j)I(y^A_{i}<y^B_{n+1-j}\}} This formulation provides
#'accurate results for PAND, but the original definition has the drawback of an
#'unknown distribution under the null hypothesis, making a statistical test
#'difficult. The `pand()` function enables the calculation of PAND using both
#'methods. The first approach (`method = "sort"`) follows the algorithm
#'described above, with the exclusion of randomization before sorting to avoid
#'ambiguity. It calculates a phi measure and provides the results of a
#'chi-squared test and a Fisher exact test. The second approach (`method =
#'"minimum"`) applies the aforementioned formula. The code of this function is
#'based on the code of the `SingleCaseES` package (function `calc_PAND`). For a
#'multiple case design, overlaps are calculated for each case, summed, and then
#'divided by the total number of measurements. No statistical test is conducted
#'for this method.
#'
#'@inheritParams .inheritParams
#'@param method Either `"sort"`" or `"minimum"`. See details.
#'@order 1
#'@return 
#'  |  |  |
#'  | --- | --- |
#'  | `pand` | Percentage of all non-overlapping data. |
#'  | `method` | Calculation method. |
#'  | `phi` | Effect size Phi based on expected and observed values. | 
#'  | `perc_overlap` | Percentage of overlapping data points. | 
#'  | `overlaps` | Number of overlapping data points. |
#'  | `n` | Number of data points. |
#'  | `N` | Number of cases. | 
#'  | `n_a` | Number of data points in phase A. | 
#'  | `n_b` | Number of data points in phase B. | 
#'  | `matrix` | 2x2 frequency matrix of phase A and B comparisons. |
#'  | `matrix_counts` | 2x2 counts matrix of phase A and B comparisons. |
#'  | `chi_test` | A Chi-squared analysis of expected and observed data (chisq.test()). | 
#'  | `fisher_test` | A Fisher exact test analysis of expected and observed data (fisher.test()). | 
#'@author Juergen Wilbert
#'@family overlap functions
#'@references Parker, R. I., Hagan-Burke, S., & Vannest, K. (2007). Percentage
#'  of All Non-Overlapping Data (PAND): An Alternative to PND. *The Journal of
#'  Special Education, 40*, 194-204.
#'
#'  Parker, R. I., & Vannest, K. (2009). An Improved Effect Size for Single-Case
#'  Research: Nonoverlap of All Pairs. *Behavior Therapy, 40*, 357-367.
#'
#'  Pustejovsky, J. E. (2019). Procedural sensitivities of effect sizes for
#'  single-case designs with directly observed behavioral outcome measures.
#'  *Psychological Methods*, *24(2)*, 217-235.
#'  https://doi.org/10.1037/met0000179
#'
#'  Pustejovsky JE, Chen M, Swan DM (2023). SingleCaseES: A Calculator for
#'  Single-Case Effect Sizes. R package version 0.7.1.9999,
#'  https://jepusto.github.io/SingleCaseES/.
#' @examples
#' ## REplication of the Parker et al. 2007 example
#' pand(Parker2007)
#'
#' ## Calculate the PAND with an expected decrease of phase B scores
#' cubs <- scdf(c(20,22,24,17,21,13,10,9,20,9,18), B_start = 5)
#' pand(cubs, decreasing = TRUE)
#'
#'@export
pand <- function(data, dvar, pvar, 
                 decreasing = FALSE, 
                 phases = c(1, 2),
                 method = c("sort", "minimum")) {
  
  
  check_args(
    
    by_call(method)
  )
  method <- method[1]

  # set default attributes
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data)
  
  dv(data) <- dvar
  phase(data) <- pvar
  
  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data
  
  N <- length(data)
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
      x <- x[sample(1:nrow(x)),]
      x[[pvar]][order(x[[dvar]], x[[pvar]], decreasing = decreasing)]
    }) |> unlist() 
    
    #phases_sorted <- lapply(data, function(x) {
    #  x <- x[sample(1:nrow(x)),]
    #  x[[pvar]][sort.list(x[[dvar]],decreasing = decreasing)]
    #}) |> unlist()
    
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
      fisher_test = suppressWarnings(fisher.test(mat_counts))
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
      perc_overlaps = 100 - (nonoverlaps / n * 100),
      n = n, 
      N = N, 
      n_a = n_all_a, 
      n_b = n_all_b, 
      casewise = casewise,
      method = method
    )
  }

  class(out) <- c("sc_pand")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  out
}


#' @describeIn pand Export results as html table (see [export()])
#' @inheritParams export
#' @order 3
#' @export
export.sc_pand <- function(object, 
                           caption = NA, 
                           footnote = NA, 
                           filename = NA,
                           round = 1,
                           ...) {
  
  if (is.na(caption)) {
    caption <- c("Percentage of all non-overlapping data (PAND)")
  }
  
  if (is.na(footnote)) {
    footnote <- c(
      paste0("PAND = ", round(object$pand, 1), "%"),
      paste0("\u03A6 = ", round(object$phi, 3)), 
      paste0("\u03A6\u00b2 = ", round(object$phi^2, 3)), 
      paste0("Number of cases: ", object$N), 
      sprintf("\u03C7\u00B2 = %.2f, df = 1, p = %.3f; ",
              object$chi_test$statistic, 
              object$chi_test$p.value
      ),
      sprintf(
        "Fisher exact test: Odds ratio = %.2f, p = %.3f",
        object$fisher_test$estimate, 
        object$fisher_test$p.value
      )
    )
  }
  
  object$matrix <- rbind(object$matrix, object$matrix[1,] + object$matrix[2,])
  object$matrix_counts <- rbind(
    object$matrix_counts, object$matrix_counts[1,] + object$matrix_counts[2,]
  )
  object$matrix <- cbind(object$matrix, object$matrix[,1] + object$matrix[,2])
  object$matrix_counts <- cbind(
    object$matrix_counts, object$matrix_counts[,1] + object$matrix_counts[,2]
  )  
  out <- as.data.frame(
    round(rbind(object$matrix * 100, object$matrix_counts), round)
  )
  out <- cbind(
    data.frame(
      " " = rep(c("Real", " ", " "), 2), Phase = rep(c("A", "B", "Total"), 2)
    ), 
    out
  )
  names(out) <- c(" ", "  ", "A", "B", "Total")

  opts <- options(knitr.kable.NA = "")
  options(scan.export.kable = c(
    list(align = c("l", "r", "c", "c", "c")), 
    getOption("scan.export.kable")
  ))
  
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    spanner = list("Expected" = 3:5),
    row_group = list("Percentage" = 1:3, "Counts" = 4:6)
  )
  
  if (getOption("scan.export.engine") == "kable") {
    style <- "border-bottom: 1px solid; text-align: center"
    table <- table  |> 
      add_header_above(c(" " = 2, "Expected" = 3))  |> 
      pack_rows(index = c("Percentage" = 3, "Counts" = 3), label_row_css = style)  |> 
      column_spec(1:2, bold = TRUE)
  }
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  options(opts)
  
  table
}


