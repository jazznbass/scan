# Simple base-R checksum (non-cryptographic, stable for comparisons)
object_checksum <- function(x) {
  h <- suppressWarnings(
    unlist(x, recursive = TRUE) |> 
    as.numeric() |> 
    sum(na.rm = TRUE) 
  )
  sprintf("%.4f", h)
}
