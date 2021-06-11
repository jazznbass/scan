.keepphasesSC <- function(data, 
                          phases = c(1, 2), 
                          set.phases = TRUE, 
                          pvar = "phase") {
  
  #if (is.data.frame(data)) data <- list(data)
  source_attributes <- attributes(data)
  
  warning <- character(0)
  
  if (class(phases) %in% c("character", "numeric", "integer")) {
    if (!length(phases) == 2) {
      stop("Phases argument not set correctly.")
    }    
    phases_A <- phases[1]
    phases_B <- phases[2]
  }
  
  if (class(phases) == "list") {
    phases_A <- phases[[1]]
    phases_B <- phases[[2]]
  }
  
  phases_total <- c(phases_A, phases_B)
  
  N <- length(data)
  design_list <- list()
  dropped_cases <- numeric(0)
  
  for(case in 1:N) {
    
    design <- rle(as.character(data[[case]][, pvar]))
    
    if (class(phases_total) == "character") {
      select_A <- which(design$values %in% phases_A)
      select_B <- which(design$values %in% phases_B)
    } else {
      select_A <- phases_A
      select_B <- phases_B
    }
    
    
    if (class(phases_total) != "character") {
      if (any(phases_total > length(design$values))) {
        warning <- c(warning, paste0("Phase(s) not found. Case ", case, " dropped.\n"))
        #warning(
        #  paste0("Phase(s) not found. Case ", case, " dropped.\n")
        #)
        dropped_cases <- c(dropped_cases, case)
        next
      }
    }
    
    if (class(phases_total) == "character") {
      
      tmp <- sapply(phases_total, function(x) sum(x == design$values) > 1)
      if (any(tmp)) {
        stop(
          paste0(
            "Selected phase ", paste(names(tmp[tmp])), 
            " occure several times. Please give number of phases instead of characters."
          )
        )
      }
      
      tmp <- sapply(phases_total, function(x) any(x == design$values))
      if (!all(tmp)) {  
        warning <- c(
          warning, paste0("Phase(s) ",  names(tmp[!tmp]), " not found. Case ", case, " dropped.\n")
        )
        #warning(
        #  paste0("Phase(s) ",  names(tmp[!tmp]), " not found. Case ", case, " dropped.\n")
        #)
        dropped_cases <- c(dropped_cases, case)
        next
      }
      
    }
    
    design$start <- c(1, cumsum(design$lengths) + 1)[1:length(design$lengths)]
    design$stop <- cumsum(design$lengths)
    class(design) <- "list"
    
    A <- unlist(lapply(select_A, function(x) design$start[x]:design$stop[x]))
    B <- unlist(lapply(select_B, function(x) design$start[x]:design$stop[x]))
    
    data[[case]][,pvar] <- as.character(data[[case]][, pvar])
    
    if (set.phases) {
      data[[case]][A ,pvar] <- "A"
      data[[case]][B ,pvar] <- "B"
    }
    data[[case]] <- data[[case]][c(A, B),]
    design_list[[case]] <- design
  }
  
  if (length(warning) > 0) warning(paste0(warning, collapse = "  "))
  if (length(dropped_cases > 0)) {
    data <- data[-dropped_cases]
    design_list <- design_list[-dropped_cases]
    source_attributes$names <- source_attributes$names[-dropped_cases]
    if (length(data) == 0) stop("No case remained.")
  }
  
  attributes(data) <- source_attributes
  out <- list(
    data = data, 
    designs = design_list, 
    N = N, 
    phases_A = phases_A, 
    phases_B = phases_B
  )
  
  class(out) <- c("sc_keepphases")
  
  out
}