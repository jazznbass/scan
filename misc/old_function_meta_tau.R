#'  Tau values are not converted to
#'   Pearson r values. The argument \code{"meta_method"} calculates a
#'   random-effect model ("random") or a fixed effect model ("fixed").
#'   
#res <- metacor(tau, n)

#no difference between random and fixed
#TE <- res$TE
#seTE <- res$seTE.common

#if (identical(TE, Inf)) {
#  warning("One of the Tau values is 1. Therefore, ",
#          "the ci = [1;1] and p = 0.")
#}
#if (identical(TE, -Inf)) {
#  warning("One of the Tau values is -1. Therefore, ",
#          "the ci = [-1;-1] and p = 0.")
#}


#ci_z <- qnorm((1 - ci) /2, lower.tail = FALSE)
#table_tau$`CI lower` <-  table_tau$Tau - ci_z * table_tau$SE_Tau
#table_tau$`CI upper` <-  table_tau$Tau + ci_z * table_tau$SE_Tau


#' @param meta_method (Deprecated) Character string. If set "random", a random-effect
#'   meta-analysis is calculated. If set "fixed", a fixed-effect meta-analysis
#'   is calculated. If set "none", no meta-analysis is conducted (may be helpful
#'   to speed up analyses).


# .meta_tau_u_old <- function(tau_matrix, method = NA, ci = 0.95) {
#   
#   ci_z <- qnorm((1 - ci) /2, lower.tail = FALSE)
#   
#   .random <- function(tau, se) {
#     res <- metagen(tau, se)
#     ret <- list()
#     ret$Tau_U <- res$TE.random
#     ret$se <- res$seTE.random
#     ret$'CI lower' <- ret$Tau_U - ci_z * ret$se
#     ret$'CI upper' <- ret$Tau_U + ci_z * ret$se
#     ret$z <- res$zval.random
#     ret$p <- res$pval.random
#     ret
#   }
#   
#   .fixed <- function(tau, se) {
#     res <- metagen(tau, se)
#     ret <- list()
#     ret$Tau_U <- res$TE.fixed
#     ret$se <- res$seTE.fixed
#     ret$'CI lower' <- ret$Tau_U - ci_z * ret$se
#     ret$'CI upper' <- ret$Tau_U + ci_z * ret$se
#     ret$z <- res$zval.fixed
#     ret$p <- res$pval.fixed
#     ret
#   }
#   
#   .ot <- function(model) {
#     tau <- sapply(tau_matrix, function(x) x[model, "Tau"])
#     se <- sapply(tau_matrix, function(x) x[model, "SE_Tau"])
#     
#     if (method == "random") return(data.frame(Model = model, .random(tau, se)))
#     if (method == "fixed") return(data.frame(Model = model, .fixed(tau, se)))
#   }
#   
#   out <- data.frame(
#     Model = character(4), 
#     Tau_U = numeric(4),
#     se = numeric(4),
#     'CI lower' = numeric(4),
#     'CI upper' = numeric(4),
#     z = numeric(4),
#     p = numeric(4),
#     check.names = FALSE
#   )
#   
#   out[1,] <- .ot("A vs. B") 
#   out[2,] <- .ot("A vs. B - Trend A") 
#   out[3,] <- .ot("A vs. B + Trend B")
#   out[4,] <- .ot("A vs. B + Trend B - Trend A") 
#   
#   out
# }
