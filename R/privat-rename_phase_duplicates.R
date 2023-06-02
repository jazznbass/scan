rename_phase_duplicates <- function(phase) {
  ph_str <- rle(as.character(phase))
  ts <- table(ph_str$values)
  new_phase <- ph_str$values
  for(i in 1:length(ts)) {
    if (ts[i] > 1) {
      new_phase[which(ph_str$values == names(ts[i]))] <- 
        paste0(names(ts[i]), "(", 1:ts[i], ")")
    }
  }
  new_phase <- mapply(
    function(x,y) rep(x, y), 
    new_phase, ph_str$lengths, 
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ) |> unlist()
  as.factor(new_phase)
}