
# #' @order 1
#' @export
ird <- function(data, dvar, pvar,
                decreasing = FALSE,
                phases = c(1, 2)) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  data <- .prepare_scdf(data, na.rm = TRUE)
  keepphases <- recombine_phases(data, phases = phases)
  
  data <- keepphases$data
  
  casenames <- revise_names(data)
  
  .ird <- function(data) {
    values <- split(data[[dvar]], data[[pvar]])
    
    if (!decreasing) {
      nonoverlap_a <- lapply(values$A, function(x) all(x >= values$B)) |> unlist() |> sum()
      nonoverlap_b <- lapply(values$B, function(x) all(x > values$A)) |> unlist() |> sum()
    }
    if (decreasing) {
      nonoverlap_a <- lapply(values$A, function(x) all(x <= values$B)) |> unlist() |> sum()
      nonoverlap_b <- lapply(values$B, function(x) all(x < values$A)) |> unlist() |> sum()
    }
    
    ir_a <- nonoverlap_a / length(values$A)
    ir_b <- nonoverlap_b / length(values$B)
    
    ird <- ir_b - ir_a   
    
    list(
      ird = ird,
      improve_rates = c(A = ir_a, B = ir_b),
      nonoverlaps = c(A = nonoverlap_a, B = nonoverlap_b)
    )
  }

  out <- list(
    decreasing = decreasing,
    phases = keepphases$phases
  )
  out$ird <- lapply(data, .ird)
  class(out) <- "sc_ird"
  out
  
}


#' @describeIn ird Print results
#' @order 2
#' @export
#' 
print.sc_ird <- function(x, digits = 3, ...) {
  cat("Improvement rate difference\n\n")
  ird <- x$ird |> 
    lapply(function(x) c(ird = x$ird, x$improve_rates, x$nonoverlaps)) 
  ird <- do.call(rbind, ird) |> 
    round(digits) |> 
    as.data.frame()
  names(ird) <- c(
    "IRD", 
    paste0("Improve rate phase ", c("A", "B")), 
    paste0("Non-overlap phase ", c("A", "B"))
  )

  print(ird, ...)
  cat("\n")
  if (x$decreasing) {
    cat("Assumed decreasing values in the B-phase.\n\n")
  }
}

