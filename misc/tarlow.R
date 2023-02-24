# TAU-U FOR SINGLE-CASE RESEARCH (2017, MARCH) 

# Citation: Tarlow, K. R. (2017, March). Tau-U for single- 
#           case research (R code). Retrieved from 
#           http://ktarlow.com/stats/

# R SYNTAX COPYRIGHT (C) 2017 KEVIN R. TARLOW
# url: http://www.ktarlow.com/stats
# email: krtarlow@gmail.com

# adapted from: Parker, R. I., Vannest, K. J., Davis, J. L., 
#               & Sauber, S. B. (2011). Combining nonoverlap
#               and trend for single-case research: Tau-U. 
#               Behavior Therapy.

# This work is licensed under the Creative Commons 
# Attribution-NonCommercial 3.0 Unported License. 
# To view a copy of this license, visit 
# http://creativecommons.org/licenses/by-nc/3.0/deed.en_US
# 
# You are free to copy, distribute, transmit, and adapt
# the work under the following conditions:
#
# Attribution - You must attribute the work in the manner 
# specified by the author, KEVIN R. TARLOW (but not in any way 
# that suggests that the author endorses you or your use 
# of the work).
#
# Noncommercial  You may not use this work for commercial 
# purposes.

tauu <- function(a,b) {
  
  # The tauu() function accepts two arguments, a and b, which
  # are vectors for each phase in an AB single-case design
  
  library(Kendall)
  
  apairs <- (length(a) * (length(a)-1) / 2)
  bpairs <- (length(b) * (length(b)-1) / 2)
  
  r <- list(trenda=as.numeric(), trendb=as.numeric(), ab=as.numeric(), ab.mina=as.numeric(), ab.plusb=as.numeric(), ab.plusb.mina=as.numeric())
  
  r$trenda    <- Kendall(a,1:length(a))
  r$trenda[1] <- as.numeric(r$trenda$S) / apairs	
  r$trenda[4] <- apairs
  
  r$trendb    <- Kendall(b,1:length(b))
  r$trendb[1] <- as.numeric(r$trendb$S) / bpairs	
  r$trendb[4] <- bpairs
  
  r$ab    <- Kendall(c(a,b), c(rep(0,length(a)), rep(1,length(b))))
  r$ab[1] <- as.numeric(r$ab$S) / (length(a)*length(b))  
  r$ab[4] <- (length(a)*length(b))
  
  r$ab.mina    <- Kendall(c(a,b), c(-(1:length(a)),rep(length(a)+1,length(b))))
  r$ab.mina[1] <- as.numeric(r$ab.mina$S) / (length(a)*length(b) + apairs)
  r$ab.mina[4] <- (length(a)*length(b) + apairs)
  
  r$ab.plusb    <- Kendall(c(a,b), c(rep(0,length(a)), (length(a)+1):length(c(a,b))))
  r$ab.plusb[1] <- as.numeric(r$ab.plusb$S) / (length(a)*length(b) + bpairs)
  r$ab.plusb[4] <- (length(a)*length(b) + bpairs)
  
  r$ab.plusb.mina    <- Kendall(c(a,b), c((length(a):1), (length(a)+1):(length(a)+length(b))))
  r$ab.plusb.mina[1] <- as.numeric(r$ab.plusb.mina$S) / (length(a)*length(b) + apairs + bpairs)
  r$ab.plusb.mina[4] <- (length(a)*length(b) + apairs + bpairs)
  
  return(r)
}
