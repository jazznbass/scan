#' Fetches elements from scan objects
#'
#' @description The fetch function is a getter function for scan objects
#' returned from regression functions such as `plm()`, `hplm()`, `bplm()`, and
#' `mplm()`. It allows users to extract specific elements from these objects,
#' such as the fitted model.
#'
#' @aliases fetch.sc_plm fetch.sc_hplm fetch.sc_bplm fetch.sc_mplm
#' @param object Object returned from a scan function.
#' @param what Element/part to be extracted. Currently, only "model" is
#'   supported to extract the fitted regression model.
#' @param ... Further parameters passed to the function.
#' @return An object of the respective regression model class.
#' @author Juergen Wilbert
#' @family regression functions
#' @keywords regression
#' @examples
#' # plm regression
#' model1 <- plm(example_A24)
#' fetch(model1, what = "model") |> summary()
#' # Multilevel plm regression
#' model2 <- hplm(exampleAB_50)
#' fetch(model2, what = "model") |> summary()
#' # Bayesian plm regression
#' model3 <- bplm(exampleAB_50, nitt = 5000)
#' fetch(model3, what = "model") |> summary()
#' @export
fetch <- function (object, what, ...) {
  
  UseMethod("fetch")
  
}

#' @exportS3Method fetch sc_plm
fetch.sc_plm <- function(object, what = "model", ...) {
  if (what == "model") {
    return(object$full.model)
  }
}


#' @exportS3Method fetch sc_hplm
fetch.sc_hplm <- function(object, what = "model", ...) {
  if (what == "model") {
    return(object$hplm)
  }
  
}


#' @exportS3Method fetch sc_bplm
fetch.sc_bplm <- function(object, what = "model", ...) {
  if (what == "model") {
    return(object$mcmcglmm)
  }
}

#' @exportS3Method fetch sc_mplm
fetch.sc_mplm <- function(object, what = "model", ...) {
  if (what == "model") {
    return(object$full.model)
  }
}




