#' Add level 2 
#' 
#' Add level 2 variables from a level 2 dataset to an scdf file
#'
#' @inheritParams .inheritParams
#' @param data_l2 A level 2 dataset.
#' @param cvar Character string with the name of the "case" variable in the 
#' L2 dataset (default is 'case').
#' @return An scdf
#' @examples
#' Leidig2018 %>% add_l2(Leidig2018_l2)
#' @export
add_l2 <- function(data, data_l2, cvar = "case") {
  
  for(i in seq_along(data)) {
    
    id <- which(data_l2[[cvar]] == names(data)[i])
    
    if (length(id) > 1) {
      stop("Multiple matches for a casename in the L1 dataset")
    }
    
    if (length(id) == 1) {
      data[[i]] <- cbind(
        data[[i]], 
        data_l2[id, -which(names(data_l2) == cvar)], 
        row.names = NULL
      )
    }
    
  }
  
  data
}