# transform.scdf <- function(`_data`, ...) {
#   expressions <- substitute(list(...))
#   df_all <- as.data.frame(`_data`)
#   
#   for(i in seq_along(`_data`)) {
#     .list <- as.list(`_data`[[i]])
#     .list$all_cases <- .list$all <- function(x) {
#       x <- substitute(x)
#       eval(x, df_all)
#     }
#     
#     for(j in 2:length(expressions)) {
#       new <- eval(expressions[c(1,j)], .list, parent.frame())
#       `_data`[[i]][[names(new)]] <- new[[1]]
#       .list[[names(new)]] <- new[[1]]
#       
#     }
#   }
#   `_data`
# }
