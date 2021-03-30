######################################################################################
#' @name summaryStatistics
#'
#' @title Additional summary statistics
#'
#' @aliases hmean gmean MAD fisherskew pearsonskew fisherkurtosis
#'
#' @description superb adds a few summary statistics that can
#' be used to characterize a dataset. All comes with ``SE.fct()`` and ``CI.fct()``.
#' See \insertCite{htc14,htc15}{superb} for more.
#' "superbPlot-compatible" summary statistics functions must have one parameter:
#' 
#' @usage hmean(x)
#' @usage gmean(x)
#' @usage MAD(x)
#' @usage fisherskew(x)
#' @usage pearsonskew(x)
#' @usage fisherkurtosis(x)
#' 
#' @param x a vector of numbers, the sample data (mandatory);
#'
#' @return a summary statistic describing the sample.
#'
#' @examples
#' # the confidence interval of the mean for default 95% and 90% confidence level
#' gmean( c(1,2,3) )  # the geometric mean; also available in psych::geometric.mean 	
#' hmean( c(1,2,3) )  # the harmonic mean;  also available in psych::harmonic.mean 	
#' MAD( c(1,2,3) )    # the median absolute deviation to the median (not the same as mad)
#' fisherskew( c(1,2,3) )     # the Fisher skew corrected for sample size
#' fisherkurtosis( c(1,2,3) ) # the Fisher kurtosis corrected for sample size
#' pearsonskew( c(1,2,3) )    # the Pearson skew
#'
#' @references
#'      \insertAllCited{}
#'
#' @export hmean
#' @export gmean
#' @export MAD
#' @export fisherskew
#' @export pearsonskew
#' @export fisherkurtosis
#'
hmean <- function(x) { 1 / mean(1/x) }
gmean <- function(x) { (prod(x))^(1/length(x)) }
MAD <- function(x) { median(abs(x-median(x))) }
fisherskew <- function(x) {
  vrx <- var(x)
  n   <- length(x)
  skbiased <- (1/n) * (sum((x - mean(x))^3)) / ((n-1)/n * vrx)^(3/2)
  sqrt(n * (n-1)) / (n-2) * skbiased
}
pearsonskew <- function(x) {
  sdx <- sd(x)
  (mean(x) - median(x)) / sdx
}
fisherkurtosis <- function(x) {
  vrx <- var(x)
  n   <- length(x)
  kubias <- (1/n) * (sum((x - mean(x))^4)) / ((n - 1)/n * vrx)^(4/2)
  (n+1) / ((n - 2) * (n - 3)) * ((n + 1) * (kubias - 3) + 6)
}



######################################################################################
#' @name precisionMeasures
#'
#' @title Precision measures
#'
#' @aliases SE.mean CI.mean SE.median CI.median SE.hmean CI.hmean SE.gmean CI.gmean
#'      SE.var CI.var SE.sd CI.sd SE.MAD CI.MAD SE.IQR CI.IQR
#'      SE.fisherskew CI.fisherskew SE.pearsonskew CI.pearsonskew
#'      SE.fisherkurtosis CI.fisherkurtosis
#'
#' @description superb comes with a few built-in measures of 
#' precisions. All ``SE.fct()`` functions produces an interval width;
#' all ``CI.fct()`` produces the lower and upper limits of an interval.
#' See \insertCite{htc14,htc15}{superb} for more.
#' "superbPlot-compatible" precision measures must have these parameters:
#' 
#' @usage SE.mean(x)
#' @usage CI.mean(x, gamma)
#' @usage SE.median(x)
#' @usage CI.median(x, gamma)
#' @usage SE.hmean(x)
#' @usage CI.hmean(x, gamma)
#' @usage SE.gmean(x)
#' @usage CI.gmean(x, gamma)
#' @usage SE.var(x)
#' @usage CI.var(x, gamma)
#' @usage SE.sd(x)
#' @usage CI.sd(x, gamma)
#' @usage SE.MAD(x)
#' @usage CI.MAD(x, gamma)
#' @usage SE.IQR(x)
#' @usage CI.IQR(x, gamma)
#' @usage SE.fisherskew(x)
#' @usage CI.fisherskew(x, gamma)
#' @usage SE.pearsonskew(x)
#' @usage CI.pearsonskew(x, gamma)
#' @usage SE.fisherkurtosis(x)
#' @usage CI.fisherkurtosis(x, gamma)
#' 
#' @param x a vector of numbers, the sample data (mandatory);
#' @param gamma a confidence level for CI (default 0.95).
#'
#' @return a measure of precision (SE) or an interval of precision (CI).
#'
#' @examples
#' # the confidence interval of the mean for default 95% and 90% confidence level
#' CI.mean( c(1,2,3) )
#' CI.mean( c(1,2,3), gamma = 0.90)
#'
#' # Standard errors for standard deviation, for MAD and for fisher skew
#' SE.sd( c(1,2,3) )
#' SE.MAD( c(1,2,3) )
#' SE.fisherskew( c(1,2,3) )
#'
#' @references
#'      \insertAllCited{}
#'
#' @export SE.mean
#' @export CI.mean
#' @export SE.median
#' @export CI.median
#' @export SE.hmean
#' @export CI.hmean
#' @export SE.gmean
#' @export CI.gmean
#' @export SE.var
#' @export CI.var
#' @export SE.sd
#' @export CI.sd
#' @export SE.MAD
#' @export CI.MAD
#' @export SE.IQR
#' @export CI.IQR
#' @export SE.fisherskew
#' @export CI.fisherskew
#' @export SE.pearsonskew
#' @export CI.pearsonskew
#' @export SE.fisherkurtosis
#' @export CI.fisherkurtosis
#'
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
# note that mad (lowercase) is already part of R with a slightly different definition
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



######################################################################################
#' @name bootstrapPrecisionMeasures
#'
#' @title Bootstrapped measures of precision 
#'
#' @aliases bootstrapSE.mean bootstrapCI.mean bootstrapSE.median bootstrapCI.median 
#'      bootstrapSE.hmean bootstrapCI.hmean bootstrapSE.gmean bootstrapCI.gmean
#'      bootstrapSE.var bootstrapCI.var bootstrapSE.sd bootstrapCI.sd 
#'
#' @description superb also comes with a few built-in measures of 
#' precisions that uses bootstrap. More can be added based on users needs.
#' All ``bootstrapSE.fct()`` functions produces an interval width;
#' all ``bootstrapCI.fct()`` produces the lower and upper limits of an interval.
#' These estimates are based on 5,000 sub-samples by default. Change this 
#' default with``options("superb.bootstrapIter" = number )``.
#' See \insertCite{et94}{superb} for a comprehensive introduction.
#' All "superbPlot-compatible" precision measures must have these parameters:
#' 
#' @usage bootstrapSE.mean(x)
#' @usage bootstrapCI.mean(x, gamma)
#' @usage bootstrapSE.median(x)
#' @usage bootstrapCI.median(x, gamma)
#' @usage bootstrapSE.hmean(x)
#' @usage bootstrapCI.hmean(x, gamma)
#' @usage bootstrapSE.gmean(x)
#' @usage bootstrapCI.gmean(x, gamma)
#' @usage bootstrapSE.var(x)
#' @usage bootstrapCI.var(x, gamma)
#' @usage bootstrapSE.sd(x)
#' @usage bootstrapCI.sd(x, gamma)
#' 
#' @param x a vector of numbers, the sample data (mandatory);
#' @param gamma a confidence level for CI (default 0.95).
#'
#' @return a measure of precision (SE) or an interval of precision (CI).
#'
#' @examples
#' # the confidence interval of the mean for default 95% and 90% confidence level
#' bootstrapCI.mean( c(1,2,3) )
#' bootstrapCI.mean( c(1,2,3), gamma = 0.90)
#'
#' # Standard errors for standard deviation or variance
#' bootstrapSE.sd( c(1,2,3) )
#' bootstrapSE.var( c(1,2,3) )
#'
#' @references
#'      \insertAllCited{}
#'
#' @export bootstrapSE.mean
#' @export bootstrapCI.mean
#' @export bootstrapSE.median
#' @export bootstrapCI.median
#' @export bootstrapSE.hmean
#' @export bootstrapCI.hmean
#' @export bootstrapSE.gmean
#' @export bootstrapCI.gmean
#' @export bootstrapSE.var
#' @export bootstrapCI.var
#' @export bootstrapSE.sd
#' @export bootstrapCI.sd
#'
bootstrapSE.mean <- function(x){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- mean(sample(x, length(x), replace = TRUE))
    }
    se <- sd(res)
    se
}
bootstrapCI.mean <- function(x, gamma = 0.95){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- mean(sample(x, length(x), replace = TRUE))
    }
    ci <- stats::quantile(res, probs = c(1/2-gamma/2, 1/2+gamma/2) )
    ci
}


######################## MEDIAN ########################
bootstrapSE.median <- function(x){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- median(sample(x, length(x), replace = TRUE))
    }
    se <- sd(res)
    se
}
bootstrapCI.median <- function(x, gamma = 0.95){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- median(sample(x, length(x), replace = TRUE))
    }
    ci <- stats::quantile(res, probs = c(1/2-gamma/2, 1/2+gamma/2) )
    ci
}


#################### HARMONIC MEAN #####################
bootstrapSE.hmean <- function(x){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- hmean(sample(x, length(x), replace = TRUE))
    }
    se <- sd(res)
    se
}
bootstrapCI.hmean <- function(x, gamma = 0.95){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- hmean(sample(x, length(x), replace = TRUE))
    }
    ci <- stats::quantile(res, probs = c(1/2-gamma/2, 1/2+gamma/2) )
    ci
}


#################### GEOMETRIC MEAN ####################
bootstrapSE.gmean <- function(x){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- gmean(sample(x, length(x), replace = TRUE))
    }
    se <- sd(res)
    se
}
bootstrapCI.gmean <- function(x, gamma = 0.95) {
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- gmean(sample(x, length(x), replace = TRUE))
    }
    ci <- stats::quantile(res, probs = c(1/2-gamma/2, 1/2+gamma/2) )
    ci
}


########################################################
################## SPREAD DESCRIPTION ##################
########################################################


####################### VARIANCE #######################
bootstrapSE.var <- function(x){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- var(sample(x, length(x), replace = TRUE))
    }
    se <- sd(res)
    se
}
bootstrapCI.var <- function(x, gamma = 0.95){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- var(sample(x, length(x), replace = TRUE))
    }
    ci <- stats::quantile(res, probs = c(1/2-gamma/2, 1/2+gamma/2) )
    ci
}


################## STANDARD DEVIATION ##################
bootstrapSE.sd <- function(x){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- sd(sample(x, length(x), replace = TRUE))
    }
    se <- sd(res)
    se
}
bootstrapCI.sd <- function(x, gamma = 0.95){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- sd(sample(x, length(x), replace = TRUE))
    }
    ci <- stats::quantile(res, probs = c(1/2-gamma/2, 1/2+gamma/2) )
    ci
}





