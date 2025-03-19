#' Fetches elements from scan objects
#' Getter function for scan objects
#' @param object Object returned from a scan function.
#' @param what Element/part to be extracted.
#' @param ... Further parameters passed to the function.
#' @return An object of the respective regression model class.
#' @export
fetch <- function (object, what, ...) {
  UseMethod("fetch")
}

#' @export
fetch.sc_plm <- function(object, what = "model", ...) {
  if (what == "model") {
    return(object$full.model)
  }
}


#' @export
fetch.sc_hplm <- function(object, what = "model", ...) {
  if (what == "model") {
    return(object$hplm)
  }
  
}

#' @export
fetch.sc_bplm <- function(object, what = "model", ...) {
  if (what == "model") {
    return(object$mcmcglmm)
  }
}

#' @export
fetch.sc_mplm <- function(object, what = "model", ...) {
  if (what == "model") {
    return(object$full.model)
  }
}




