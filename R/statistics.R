########################################################
# Standard errors and confidence intervals for many
# descriptive statistics.
# See Harding, Tremblay, & Cousineau (2014,2015), 
#   The Quantitative Methods for Psychology.
# for a review.
########################################################

########################################################
################## CENTRAL TENDENCIES ##################
########################################################

######################### MEAN #########################
SE.mean <- function(x){
  sdx <- sd(x)
  n   <- length(x)
  se  <- sdx / sqrt(n)
  se
}
CI.mean <- function(x, gamma = 0.95){
  se <- SE.mean(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- mean(x) + se * tc
  ci
}


######################## MEDIAN ########################
SE.median <- function(x){
  sdx <- sd(x)
  n   <- length(x)
  se  <- sqrt(pi/2) * sdx / sqrt(n)
  se
}
CI.median <- function(x, gamma = 0.95){
  se <- SE.median(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- median(x) + se * tc
  ci
}


#################### HARMONIC MEAN #####################
hmean <- function(x) { 1 / mean(1/x) }
SE.hmean <- function(x){
  hm2  <- hmean(x)^2
  sd1x <- sd(1/x)
  n    <- length(x)
  se   <- hm2 * sd1x / sqrt(n-1)
  se
}
CI.hmean <- function(x, gamma = 0.95){
  se <- SE.hmean(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- hmean(x) + se * tc
  ci
}


#################### GEOMETRIC MEAN ####################
gmean <- function(x) { (prod(x))^(1/length(x)) }
SE.gmean <- function(x){
  gm   <- gmean(x)
  sdlx <- sd(log(x))
  n    <- length(x)
  se   <- gm * sdlx / sqrt(n-1)
  se
}
CI.gmean <- function(x, gamma = 0.95) {
  se <- SE.gmean(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- gmean(x) + se * tc
  ci
}


########################################################
################## SPREAD DESCRIPTION ##################
########################################################


####################### VARIANCE #######################
SE.var <- function(x){
  varx <- var(x)
  n    <- length(x)
  se   <- varx * sqrt(2/(n-1))
  se
}
CI.var <- function(x, gamma = 0.95){
  varx <- var(x)
  n    <- length(x)
  c2c  <- qchisq( c(1/2 + gamma/2, 1/2 - gamma/2), n-1)
  ci   <- varx * (n-1) / c2c
  ci
}


################## STANDARD DEVIATION ##################
SE.sd <- function(x){
  sdx <- sd(x)
  n   <- length(x)
  se  <- sdx / sqrt(2*(n-1))
  se
}
CI.sd <- function(x, gamma = 0.95){
  sdx <- sd(x)
  n   <- length(x)
  c2c <- sqrt(qchisq(c(1/2+gamma/2, 1/2-gamma/2), n-1))
  ci  <- sdx * sqrt(n-1) / c2c
  ci
}


################### MEDIAN DEVIATION ###################
# note that mad is already part of R with a slightly different definition
MAD <- function(x) {
  median(abs(x-median(x)))
}
SE.MAD <- function(x){
  sdx  <- sd(x)
  n    <- length(x)
  se   <- sqrt(2/pi) * sdx / sqrt(n)
  se
}
CI.MAD <- function(x, gamma = 0.95){
  se <- SE.MAD(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- MAD(x) + se * tc
  ci
}


####################### QUANTILE #######################
# quantile function does not work...


################## INTERQUARTILE RANGE##################
SE.IQR <- function(x){
  sdx  <- sd(x)
  n    <- length(x)
  q    <- dnorm(qnorm(0.25))
  se   <- sdx / (2 * sqrt(n) * q)
  se
}
CI.IQR <- function(x, gamma = 0.95){
  se <- SE.IQR(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- IQR(x) + se * tc
  ci
}


########################################################
################### SHAPE DESCRIPTION ##################
########################################################

####################### fisherskew ######################
# this is Fisher skew for small sample
fisherskew <- function(x) {
  vrx <- var(x)
  n   <- length(x)
  skbias <- (1/n) * (sum((x - mean(x))^3)) / ((n-1)/n * vrx)^(3/2)
  sqrt(n * (n-1)) / (n-2) * skbias
}
SE.fisherskew <- function(x){
  n    <- length(x)
  se   <- sqrt( (6 * n * (n - 1)) / ((n - 2) * (n + 1)*(n + 3)) )
  se
}
CI.fisherskew <- function(x, gamma = 0.95){
  se <- SE.fisherskew(x)
  zc <- qnorm(c(1/2 - gamma/2, 1/2 + gamma/2))
  ci <- fisherskew(x) + se * zc
  ci
}


###################### pearsonskew #####################
# this is pearson skew
pearsonskew <- function(x) {
  sdx <- sd(x)
  (mean(x) - median(x)) / sdx
}
SE.pearsonskew <- function(x){
  n    <- length(x)
  se   <- sqrt( (pi/2 - 1) / n )
  se
}
CI.pearsonskew <- function(x, gamma = 0.95){
  se <- SE.pearsonskew(x)
  n  <- length(x)
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- pearsonskew(x) + se * tc
  ci
}


####################### fisherkurtosis ######################
# this is fisher kurtosis for small sample
fisherkurtosis <- function(x) {
  vrx <- var(x)
  n   <- length(x)
  kubias <- (1/n) * (sum((x - mean(x))^4)) / ((n - 1)/n * vrx)^(4/2)
  (n+1) / ((n - 2) * (n - 3)) * ((n + 1) * (kubias - 3) + 6)
}
SE.fisherkurtosis <- function(x){
  n    <- length(x)
  se   <- 2 * SE.fisherskew(x) * sqrt( (n^2 - 1) / ((n - 3) * (n + 5)) )
  se
}
CI.fisherkurtosis <- function(x, gamma = 0.95){
  n    <- length(x)
  minbx <- 2 * (n - 1) / (n - 3)
  se <- SE.fisherkurtosis(x)
  lnc <- qlnorm( c(1/2 - gamma/2, 1/2 + gamma/2) )
  ci <- fisherkurtosis(x) + 2 * lnc ^ (se / 2) - minbx
  ci
}
