
easy_prior <- function(scdf,
                       intercept = NA,
                       trend = NA,
                       level = NA,
                       slope = NA,
                       stength = c("weak", "moderate", "strong")){
  
  dat <- as.data.frame(scdf)
  sd_criteria <- sd(dat[[dvar(scdf)]], na.rm = TRUE)
  if (is.character(strength)) {
    if(strength == "weak") strength <- 10
    if(strength == "strong") strength <- 1
    if(strength == "moderate") strength <- 3
  }
  
  mu <- c(intercept, trend, lebel, slope)
  v <- c(NA, NA, NA, NA)
  
  if (!is.na(intercept)) v[1] <- (strength * sd_criteria)^2
  if (!is.na(trend)) 
    v[2] <- (strength * (sd_criteria / sd(dat[[mvar(scdf)]], na.rm = TRUE)))^2

}


prior_strength <- function(sd_x, sd_criteria, strength) {
  
  # Compute expected standard deviation of the regression coefficient
  expected_sd_beta <- sd_criteria / sd_x
  if (is.character(strength)) {
    if(strength == "weak") strength <- 10
    if(strength == "strong") strength <- 1
    if(strength == "moderate") strength <- 3
  }
  
  # Compute weak and strong prior variances
  V_weak <- (strength * expected_sd_beta)^2  # Weak prior (very flexible)
  V_strong <- (strength * expected_sd_beta)^2  # Strong prior (some constraint)
  V_moderate <- (strength * expected_sd_beta)^2  # Moderate prior
  
  # Return a list with computed priors
  return(list(Weak = V_weak, Moderate = V_moderate, Strong = V_strong))
}