# Simple base-R checksum (non-cryptographic, stable for comparisons)
object_checksum <- function(x) {
  #bytes <- as.integer(serialize(x, NULL, version = 2))  # raw -> ints 0..255
  #h <- 0
  #mod <- 2147483629  # large 31-bit prime (< 2^31)
  #for (b in bytes) {
  #  h <- (h * 16777619 + b) %% mod
  #}
  h <- suppressWarnings(
    unlist(x, recursive = TRUE) |> 
    as.numeric() |> 
    sum(na.rm = TRUE) 
  )
  sprintf("%.4f", h)
}
