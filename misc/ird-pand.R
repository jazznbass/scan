library(SingleCaseES)
library(scan)

A <- c(20, 20, 26, 25, 22, 23)
B <- c(28, 25, 24, 27, 30, 30, 29)

Tau_BC(A_data = A, B_data = B, Kendall= TRUE)
case <- scdf(c(A = 20, 20, 26, 25, 22, 23, B = 28, 25, 24, 27, 30, 30, 29))

PAND(A, B)
pand(case)
pand(case, method = "pustejowski")
scan:::old_pand(Parker2007)

dat <- Parker2009[[1]]
dat <- split(dat$values, dat$phase)

ird(Parker2009)
pand(Parker2009)
IRD(dat$A1, dat$B1)
PAND(dat$A1, dat$B1)
?IRD

exampleAB

A_data <- dat$A1
B_data <- dat$B1

A_data <- c(3,5,4,3,9,8)
B_data <- c(7,6,7,6,8,7,5,9,12,11)

PAND(A_data, B_data)


ol <- expand.grid(A_data, B_data)
ol$ol <- apply(ol, 1, function(x) x[1] >= x[2])
sum(ol$ol)
sum(sapply(A_data, function(x) x >= B_data)) / (length(A_data)*length(B_data))
dat <- scdf(c(A_data, B_data), phase_design = list(A = length(A_data), B = length(B_data)))
pand(dat)


PAND(c(2,3,4,5,3), c(4,7,6,8,7))

#####
A <- c(2,3,4,5,3)
B <- c(4,7,6,8,7)
grid <- cbind(expand.grid(1:length(A), 1:length(B)), expand.grid(sort(A), sort(B)))
grid$nonoverlap <- grid[,3] < grid[,4]
grid$new <- grid[,1] + length(B) - grid[,2]
max(grid$new*grid$nonoverlap)

######

overlap <-  min(sum(A >= min(B)), sum(B < max(A)))
(length(A) + length(B) - overlap) / (length(A) + length(B))

######

a <- c(-Inf, sort(A))
b <- c(sort(B), Inf)
grid <- expand.grid(mt_a = 1:(length(A) + 1), mt_b = 1:(length(B) + 1))
grid$no_overlap <- mapply(function(mt_a, mt_b) a[mt_a] < b[mt_b], mt_a = grid$mt_a, mt_b = grid$mt_b)
grid$overlap <- grid$mt_a + length(B) - grid$mt_b
nonoverlap <- max(grid$overlap * grid$no_overlap)
nonoverlap/(length(A) + length(B))
