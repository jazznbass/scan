library(scan)
get_model <- function(x) {
  x$full.model
}
plm(exampleABC$Marie)$full.model -> res



acf(x, lag.max = 4, plot = FALSE)

auto_corr <- function(x, lag, alpha = 0.05) {
  m_x <- mean(x)
  t <- length(x)
  
  ar <- numeric(lag)
  for(l in 1:lag) {
    ar[l] <- sum((x[(1 + l):t])  * (x[1:(t - l)] - m_x)) / sum((x - m_x)^2)    
  }

  se <- numeric(lag)
  
  # <https://de.wikipedia.org/wiki/Korrelogramm>
  # Bartletts Formel für MA(l)
  for(l in 1:lag) se[l] <- sqrt((1 + 2 * sum(ar[1 - l]^2)) / (t))
  z <- ar / se
  
  p <- pnorm(abs(z), lower.tail = FALSE) * 2
  p_t <- pt(abs(z), df = 2, lower.tail = FALSE) * 2
  
  # <http://sfb649.wiwi.hu-berlin.de/fedc_homepage/xplore/tutorials/xegbohtmlnode39.html>
  se_all <- qnorm(1-(alpha / 2)) / sqrt(t)

  # <https://en.wikipedia.org/wiki/Ljung%E2%80%93Box_test>
  q_lb <- t * (t + 2) *  sum((ar^2)/(t - (1:lag)))
  p_all <- pchisq(q_lb, df = lag, lower.tail = FALSE)
  
  # return
  data.frame(
    lag = 1:lag, ar = ar, se = se, statistic = z, p_z = p, p_t, se_all = se_all, 
    q_lb = q_lb, p_all = p_all)
}

x <- plm(exampleABC$Marie)$full.model |> residuals()

auto_corr(x, 4)
pchisq(4.102758, 4, lower.tail = FALSE)
Box.test(x, 4, type = "Ljung-Box")
acf(x, lag.max = 3)

for(i in 1:3) {
  cor(res[1:(length(res) - i)], res[(i + 1):length(res)]) |> print()
}


    