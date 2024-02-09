######################################################################################
#' @name summaryStatistics
#'
#' @title Additional summary statistics
#'
#' @aliases hmean gmean MAD fisherskew pearsonskew fisherkurtosis
#'
#' @md
#'
#' @description superb adds a few summary statistics that can
#' be used to characterize a dataset. All comes with ``SE.fct()`` and ``CI.fct()``.
#' See \insertCite{htc14,htc15}{superb} for more.
#' *superbPlot-compatible* summary statistics functions must have one parameter:
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
#' \insertAllCited{}
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
#' @name precisionMeasureWithCustomDF
#'
#' @title Confidence intervals with custom degree of freedom
#'
#' @aliases CIwithDF.mean 
#'
#' @md
#'
#' @description The following function computes a confidence interval with
#' custom degree of freedom. The default is to use N-1 but this number is not
#' always appropriate. For example, when there are heterogeneous variances, 
#' the confidence interval of the mean should mirror a Welsh test where the
#' degrees of freedom are altered based on variances. The function `CIwithDF.mean()`
#' accept an arbitrary defined degree of freedom (df). 
#' The df must be combined to the argument `gamma` after the confidence level.
#' 
#' @usage CIwithDF.mean(x, gamma = 0.95 )
#' 
#' @param x a vector of numbers, the sample data (mandatory);
#' @param gamma a vector containing first a confidence level for CI (default 0.95) and
#'   a custom degree of freedom (when unspecified, it uses ``n-1`` where ``n`` is the number of observations in x).
#'
#' @return the confidence interval (CI) where the ``t`` value is based on the custom-set degree of freedom.
#'
#' @details See the vignette "Unequal variances, Welch test, Tryon adjustment, and superb"
#' for an example of use.
#'
#' @examples
#' # this will issue a warning as no custom degree of freedom is provided
#' CIwithDF.mean( c(1,2,3), gamma = 0.90)          
#' # the confidence interval of the mean for 90% confidence level
#' CIwithDF.mean( c(1,2,3), gamma = c(0.90, 1.5) ) # uses 1.5 as df instead of 2.
#'
#' @references
#' \insertAllCited{}
#'
#' @export CIwithDF.mean
#'
CIwithDF.mean <- function(x, gamma = 0.95) {
    # this function is useful to implement Welch' test by specifying a rectifed df
    # which overrides the defautl n-1
    se <- SE.mean(x)
    n  <- length(x)
    if (length(gamma) == 2) {
        wdf = gamma[2]
        g = gamma[1]
    } else {
        warning("superb::FYI: No degree of freedom provided in gamma[2]; revert to CI")
        wdf = n-1
        g = gamma[1]
    }
    tc <- qt(c(1/2 - g/2, 1/2 + g/2), df = wdf)
    ci <- mean(x) + se * tc
    ci
}


######################################################################################
#' @name measuresWithMissingData
#'
#' @title Measures with missing data
#'
#' @aliases meanNArm, SE.meanNArm CI.meanNArm 
#'
#' @md
#'
#' @description The following three functions can be used with missing data. 
#' They return the mean, the standard error of the mean and the confidence 
#' interval of the mean.Note that we hesitated to provide these functions: you 
#' should deal with missing data prior to making your plot.
#' Also note that for repeated-measure design, only CA adjustment is available.
#' 
#' @usage meanNArm(x)
#' @usage SE.meanNArm(x)
#' @usage CI.meanNArm(x, gamma)
#' 
#' @param x a vector of numbers, the sample data (mandatory);
#' @param gamma a confidence level for CI (default 0.95).
#'
#' @return the means, a measure of precision (SE) or an interval of precision 
#'   (CI) in the presence of missing data.
#'
#' @examples
#' # the confidence interval of the mean for default 95% and 90% confidence level
#' meanNArm( c(1,2,3, NA) )
#' SE.meanNArm( c(1,2,3, NA) )
#' CI.meanNArm( c(1,2,3, NA) )
#' CI.meanNArm( c(1,2,3, NA), gamma = 0.90)
#'
#' @references
#' \insertAllCited{}
#'
#' @export meanNArm
#' @export SE.meanNArm
#' @export CI.meanNArm
#'
# Removal of the missing data built-in the functions. Not recommended:
# you should deal with your missing data prior to making a plot
meanNArm <- function(x) mean(x, na.rm = TRUE)
SE.meanNArm <- function(x){
  sdx <- sd(x, na.rm = TRUE)
  n   <- length(x[!is.na(x)])
  se  <- sdx / sqrt(n)
  se
}
CI.meanNArm <- function(x, gamma = 0.95){
  se <- SE.meanNArm(x)
  n  <- length(x[!is.na(x)])
  tc <- qt(c(1/2 - gamma/2, 1/2 + gamma/2), n-1)
  ci <- mean(x, na.rm = TRUE) + se * tc
  ci
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
#' @md
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
#' \insertAllCited{}
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
#' @aliases bootstrapSE.mean bootstrapPI.mean bootstrapSE.median bootstrapPI.median 
#'      bootstrapSE.hmean bootstrapPI.hmean bootstrapSE.gmean bootstrapPI.gmean
#'      bootstrapSE.var bootstrapPI.var bootstrapSE.sd bootstrapPI.sd 
#'
#' @md
#'
#' @description superb also comes with a few built-in measures of 
#' precisions that uses bootstrap. More can be added based on users needs.
#' All ``bootstrapSE.fct()`` functions produces an interval width;
#' all ``bootstrapPI.fct()`` produces the lower and upper limits of an interval.
#' These estimates are based on 5,000 sub-samples by default. Change this 
#' default with``options("superb.bootstrapIter" = number )``.
#' See \insertCite{et94;textual}{superb} for a comprehensive introduction.
#' The bootstrap estimates are called PI which stands for Precision intervals.
#' This is to denote that they estimate the sampling distribution, not the 
#' predictive distribution on which all confidence intervals are based
#' \insertCite{rpw19,pl10,l99}{superb}.
#' 
#' @usage bootstrapSE.mean(x)
#' @usage bootstrapPI.mean(x, gamma)
#' @usage bootstrapSE.median(x)
#' @usage bootstrapPI.median(x, gamma)
#' @usage bootstrapSE.hmean(x)
#' @usage bootstrapPI.hmean(x, gamma)
#' @usage bootstrapSE.gmean(x)
#' @usage bootstrapPI.gmean(x, gamma)
#' @usage bootstrapSE.var(x)
#' @usage bootstrapPI.var(x, gamma)
#' @usage bootstrapSE.sd(x)
#' @usage bootstrapPI.sd(x, gamma)
#' 
#' @param x a vector of numbers, the sample data (mandatory);
#' @param gamma a confidence level for PI (default 0.95).
#'
#' @return a measure of precision (SE) or an interval of precision (PI).
#'
#' @examples
#' # the confidence interval of the mean for default 95% and 90% confidence level
#' bootstrapPI.mean( c(1,2,3) )
#' bootstrapPI.mean( c(1,2,3), gamma = 0.90)
#'
#' # Standard errors for standard deviation or variance
#' bootstrapSE.sd( c(1,2,3) )
#' bootstrapSE.var( c(1,2,3) )
#'
#' @references
#' \insertAllCited{}
#'
#' @export bootstrapSE.mean
#' @export bootstrapPI.mean
#' @export bootstrapSE.median
#' @export bootstrapPI.median
#' @export bootstrapSE.hmean
#' @export bootstrapPI.hmean
#' @export bootstrapSE.gmean
#' @export bootstrapPI.gmean
#' @export bootstrapSE.var
#' @export bootstrapPI.var
#' @export bootstrapSE.sd
#' @export bootstrapPI.sd
#'
bootstrapSE.mean <- function(x){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- mean(sample(x, length(x), replace = TRUE))
    }
    se <- sd(res)
    se
}
bootstrapPI.mean <- function(x, gamma = 0.95){
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
bootstrapPI.median <- function(x, gamma = 0.95){
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
bootstrapPI.hmean <- function(x, gamma = 0.95){
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
bootstrapPI.gmean <- function(x, gamma = 0.95) {
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
bootstrapPI.var <- function(x, gamma = 0.95){
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
bootstrapPI.sd <- function(x, gamma = 0.95){
    res = c()
    for (i in 1:getOption("superb.bootstrapIter")) {
        res[i] <- sd(sample(x, length(x), replace = TRUE))
    }
    ci <- stats::quantile(res, probs = c(1/2-gamma/2, 1/2+gamma/2) )
    ci
}





