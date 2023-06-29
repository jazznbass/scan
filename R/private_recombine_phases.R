recombine_phases <- function(data, 
                             phases = c(1, 2), 
                             set_phases = TRUE, 
                             phase_names = c("A", "B"),
                             pvar) {
      
  if (missing(pvar)) pvar <- phase(data)
  
  source_attributes <- attributes(data)
  
  warning <- character(0)
  
  if (inherits(phases, c("character", "numeric", "integer"))) {
    if (!length(phases) == 2) {
      stop("phases argument not set correctly.")
    }    
    phases_A <- phases[1]
    phases_B <- phases[2]
  }
  
  if (inherits(phases, "list")) {
    phases_A <- phases[[1]]
    phases_B <- phases[[2]]
  }
  
  phases_total <- c(phases_A, phases_B)
  
  N <- length(data)
  design_list <- list()
  dropped_cases <- numeric(0)
  
  for(case in 1:N) {
    
    design <- rle(as.character(data[[case]][[pvar]]))
    
    if (inherits(phases_total, "character")) {
      select_A <- which(design$values %in% phases_A)
      select_B <- which(design$values %in% phases_B)
    } else {
      select_A <- phases_A
      select_B <- phases_B
    }
    
    
    if (!inherits(phases_total, "character")) {
      if (any(phases_total > length(design$values))) {
        warning <- c(
          warning, 
          paste0("Phase(s) not found. Case ", case, " dropped.\n")
        )
        dropped_cases <- c(dropped_cases, case)
        next
      }
    }
    
    if (inherits(phases_total, "character")) {
      
      tmp <- sapply(phases_total, function(x) sum(x == design$values) > 1)
      if (any(tmp)) {
        stop(paste0(
          "Selected phase ", paste(names(tmp[tmp])), " occure several times. ",
          "Please give number of phases instead of characters."
        ))
      }
      
      tmp <- sapply(phases_total, function(x) any(x == design$values))
      if (!all(tmp)) {  
        warning <- c(
          warning, 
          paste0("Phase(s) ",  names(tmp[!tmp]), " not found. Case ", case, 
            " dropped.\n")
        )
      
        dropped_cases <- c(dropped_cases, case)
        next
      }
      
    }
    
    design$start <- c(1, cumsum(design$lengths) + 1)[1:length(design$lengths)]
    design$stop <- cumsum(design$lengths)
    class(design) <- "list"
    
    A <- unlist(lapply(select_A, function(x) design$start[x]:design$stop[x]))
    B <- unlist(lapply(select_B, function(x) design$start[x]:design$stop[x]))
    
    data[[case]][[pvar]] <- as.character(data[[case]][[pvar]])
    
    if (set_phases) {
      if (identical(phase_names, "auto")) {
        phase_names <- sapply(phases, function(x) {
          if (is.numeric(x)) {
            paste0(design$values[x], collapse = "")
          } else {
            paste0(x, collapse = "")
          }
        })          
      }
      data[[case]][A ,pvar] <- phase_names[1]
      data[[case]][B ,pvar] <- phase_names[2]
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
  
  original_phases <- lapply(design_list, function(x) x$values)
  
  new_phases <- lapply(original_phases, function(x) {
    out <- if (is.numeric(phases_A)) {
      paste0(x[phases_A], collapse = "")
    } else {
      paste0(x[which(phases_A %in% x)], collapse = "")
    }
    out <- if (is.numeric(phases_B)) {
      c(out, paste0(x[phases_B], collapse = ""))
    } else {
      c(out, paste0(x[which(phases_B %in% x)], collapse = ""))
    }
    out
  })
  
  #browser()
  
  out <- list(
    data = data, 
    designs = design_list, 
    N = N, 
    phases_A = phases_A, 
    phases_B = phases_B,
    phases = list(original = original_phases, new = new_phases)
  )
  class(out) <- c("sc_keepphases")
  
  out
}
