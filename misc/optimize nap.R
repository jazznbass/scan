library(dplyr)

nap_optim <- function(data, dvar, pvar, 
                decreasing = FALSE, 
                phases = c(1, 2)) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, scan:::opt("dv"))
  if (missing(pvar)) pvar <- scdf_attr(data, scan:::opt("phase"))
  scdf_attr(data, scan:::opt("dv")) <- dvar
  scdf_attr(data, scan:::opt("phase")) <- pvar
  
  data <- scan:::.prepare_scdf(data, na.rm = TRUE)
  data <- scan:::recombine_phases(data, phases = phases)$data
  
  nap_case <- function(case) {
    values <- split(case[[dvar]], case[[pvar]])
    pairs <- length(values$A) * length(values$B)
    
    if (!decreasing)
      pos <- pairs - sum(unlist(lapply(values$A, function(x) x >= values$B)))
    if (decreasing)
      pos <- pairs - sum(unlist(lapply(values$A, function(x) x <= values$B)))
    
    ties <- sum(unlist(lapply(values$A, function(x) x == values$B)))
    nap  <- (pos + (0.5 * ties)) / pairs
    
    test <- wilcox.test(
      values$A, values$B, 
      alternative = if (decreasing) "greater" else "less", 
      exact = FALSE
    )

    list(
      nap = nap * 100,
      nap_rescaled = 2 * (nap * 100) - 100,
      pairs = pairs,
      pos = pos,
      ties = ties,
      w = test$statistic,
      p = test$p.value
    )
  }
  
  nap <- lapply(data, nap_case)
  
  #nap <- list(
  #  Case = revise_names(data), 
  #  NAP = nap * 100, 
  #  Rescaled = 2 * (nap * 100) - 100, 
  #  Pairs = pairs, 
  #  Positives = pos, 
  #  Ties = ties, 
  #  W = w, 
  #  p = p
  #)
  
  out <- list(nap = nap)
  class(out) <- c("sc_nap")
  attr(out, scan:::opt("phase")) <- pvar
  attr(out, scan:::opt("dv")) <- dvar
  out
}

bench::mark(
  nap = nap(exampleAB),
  nap_optim = nap_optim(exampleAB),
  check = FALSE
)

res <- nap_optim(exampleAB)

out <- as.data.frame(res$nap[[1]])
for(i in 2:length(res$nap)) 
  out <- rbind(out, as.data.frame(res$nap[[i]]))

out  
lapply(res$nap, cbind)

