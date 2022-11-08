

Tarlow2016a <- scdf(c(A = 9, 6, 11, 5, B = 4, 7, 8, 7, 3, 7, 1))
Tarlow2016b <- scdf(c(A = 1, 2, 3, 4, 5, B = 2, 1, 0, 0, 0))
Tarlow2016c <- scdf(c(A = 33, 25, 17, 25, 14, 13,14, B = 14, 15, 15, 4, 6, 9, 5 ,4 ,2 ,2 ,8, 11 ,7))


refer <- "Tarlow, K. R. (2017). An Improved Rank Correlation Effect Size Statistic for Single-Case Designs: Baseline Corrected Tau. Behavior Modification, 41(4), 427–467. https://doi.org/10.1177/0145445516676750"


scdf_attr(Tarlow2016c, "info") <- paste0("Example page 439 from ", refer)
summary(Tarlow2016c)

