# #' @param correction The default `correction = TRUE` makes `pand` use a
# #'   frequency matrix, which is corrected for ties. A tie is counted as the half
# #'   of a measurement in both phases. Set `correction = FALSE` to use the
# #'   uncorrected matrix, which is not recommended.
# #'   
# #'   
old_pand <- function(data, dvar, pvar, 
                     decreasing = FALSE, 
                     correction = FALSE,
                     phases = c(1, 2)) {
  
  # set default attirubtes
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data)
  
  dv(data) <- dvar
  phase(data) <- pvar
  
  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data
  
  N <- length(data)
  
  values_a <- lapply(data, function(x) x[x[[pvar]] == "A", dvar])
  values_b <- lapply(data, function(x) x[x[[pvar]] == "B", dvar])
  
  # phase order per case as found in data
  
  phases_data <- lapply(data, function(x) x[[pvar]]) |> unlist()
  
  # phase order when sorted by values within case
  phases_sorted <- lapply(data, function(x) {
    x <- x[sample(1:nrow(x)),]
    x[[pvar]][sort.list(x[[dvar]])]
  }) |> unlist()
  
  n_all_a <- length(unlist(values_a))
  n_all_b <- length(unlist(values_b))
  n <- n_all_a + n_all_b
  
  overlaps_cases <- rep(NA, N)
  overlaps_A <- 0
  overlaps_B <- 0
  
  for (i in 1:N) {
    values <- c(values_a[[i]], values_b[[i]])
    n_a <- length(values_a[[i]])
    n_b <- length(values_b[[i]])
    n_ab <- n_a + n_b
    
    rang <- sort.list(values, decreasing = decreasing)
    a_over_b <- sum(rang[1:n_a] > n_a)
    b_over_a <- sum(rang[(n_a + 1):n_ab] <= n_a)
    
    overlaps_cases[i] <- a_over_b + b_over_a
    overlaps_A <- overlaps_A + a_over_b
    overlaps_B <- overlaps_B + b_over_a
  }
  
  overlaps <- overlaps_A + overlaps_B
  perc_overlap <- overlaps / n * 100
  prop_a <- n_all_a / n
  prop_b <- n_all_b / n
  
  b <- overlaps_A / n # prop overlaps A
  c <- overlaps_B / n # prop overlaps B
  a <- prop_a - b
  d <- prop_b - c
  phi_check <- (a / (a + c)) - (b / (b + d))
  pand <- 100 - perc_overlap
  mat_percentage <- matrix(c(a, b, c, d), nrow = 2)
  mat_counts <- mat_percentage * n
  
  chi_test <- suppressWarnings(chisq.test(mat_counts, correct = FALSE))
  
  phi <- sqrt(chi_test$statistic / n)
  
  out <- list(
    pand = pand, 
    phi = phi, 
    perc_overlap = perc_overlap, 
    overlaps_cases = overlaps_cases, 
    overlaps = overlaps, 
    n = n_ab, 
    N = N, 
    n_a = n_all_a, 
    n_b = n_all_b, 
    matrix = mat_percentage, 
    matrix_counts = mat_counts, 
    chi_test = chi_test,
    fisher_test = suppressWarnings(fisher.test(mat_counts)),
    correction = correction
  )
  
  class(out) <- c("sc_pand")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  out
}