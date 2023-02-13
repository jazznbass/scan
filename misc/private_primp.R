
.primp <- function(x, cat = TRUE) {
  
  styles <- c(black = 30, red = 31, green = 32, yellow = 33, 
              blue = 34, magenta = 35, cyan = 36, white = 37)
  styles <- c(styles, setNames(90:97, paste0("light", names(styles))))
  styles <- c(styles, setNames(c(40:47, 100:107), paste0("bg-", names(styles))))
  styles <- c(styles, bold = 1, faint = 2, italic = 3, underline = 4, swap = 7,
              strike = 9)
  
  start <- unlist(gregexpr('\\{', x))
  stop <- unlist(gregexpr('\\}', x))
  n_styles <- length(start)
  
  args <- character(n_styles)
  for(i in seq_along(start)) {
    extract <- substr(x, start[i]+1, stop[i]-1)
    extract <- unlist(strsplit(extract, "[,; ]"))
    extract <- sapply(extract, function(x) {
      .cset(setNames(styles[which(trimws(x) == names(styles))], NULL))
    })
    args[i] <- paste0(extract, collapse = "")
  }
  
  
  out <- c(substr(x, 0, start[1]-1), args[1])
  if (n_styles > 1) {
    for(i in 2:n_styles) {
      out <- c(out, substr(x, stop[i-1]+1, start[i]-1), .cset(), args[i])
    }
  }
  
  out <- c(out, substr(x, stop[n_styles]+1, nchar(x)), .cset())
  out <- paste0(out, collapse = "")
  if (cat) cat(out) else out
}

.cset <- function(x) {
  if (missing(x)) x <- "0" 
  paste0("\033[", x, "m")
}
