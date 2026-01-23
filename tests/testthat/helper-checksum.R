# Simple base-R checksum (non-cryptographic, stable for comparisons)
object_checksum <- function(x) {
  bytes <- as.integer(serialize(x, NULL, version = 2))  # raw -> ints 0..255
  h <- 0
  mod <- 2147483629  # large 31-bit prime (< 2^31)
  for (b in bytes) {
    h <- (h * 16777619 + b) %% mod
  }
  sprintf("%08x", as.integer(h))
}
