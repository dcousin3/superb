% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functionsStatistical.R
\name{precisionMeasures}
\alias{precisionMeasures}
\alias{SE.mean}
\alias{CI.mean}
\alias{SE.median}
\alias{CI.median}
\alias{SE.hmean}
\alias{CI.hmean}
\alias{SE.gmean}
\alias{CI.gmean}
\alias{SE.var}
\alias{CI.var}
\alias{SE.sd}
\alias{CI.sd}
\alias{SE.MAD}
\alias{CI.MAD}
\alias{SE.IQR}
\alias{CI.IQR}
\alias{SE.fisherskew}
\alias{CI.fisherskew}
\alias{SE.pearsonskew}
\alias{CI.pearsonskew}
\alias{SE.fisherkurtosis}
\alias{CI.fisherkurtosis}
\title{Precision measures}
\usage{
SE.mean(x)

CI.mean(x, gamma)

SE.median(x)

CI.median(x, gamma)

SE.hmean(x)

CI.hmean(x, gamma)

SE.gmean(x)

CI.gmean(x, gamma)

SE.var(x)

CI.var(x, gamma)

SE.sd(x)

CI.sd(x, gamma)

SE.MAD(x)

CI.MAD(x, gamma)

SE.IQR(x)

CI.IQR(x, gamma)

SE.fisherskew(x)

CI.fisherskew(x, gamma)

SE.pearsonskew(x)

CI.pearsonskew(x, gamma)

SE.fisherkurtosis(x)

CI.fisherkurtosis(x, gamma)
}
\arguments{
\item{x}{a vector of numbers, the sample data (mandatory);}

\item{gamma}{a confidence level for CI (default 0.95).}
}
\value{
a measure of precision (SE) or an interval of precision (CI).
}
\description{
superb comes with a few built-in measures of
precisions. All \code{SE.fct()} functions produces an interval width;
all \code{CI.fct()} produces the lower and upper limits of an interval.
See \insertCite{htc14,htc15}{superb} for more.
"superbPlot-compatible" precision measures must have these parameters:
}
\examples{
# the confidence interval of the mean for default 95\% and 90\% confidence level
CI.mean( c(1,2,3) )
CI.mean( c(1,2,3), gamma = 0.90)

# Standard errors for standard deviation, for MAD and for fisher skew
SE.sd( c(1,2,3) )
SE.MAD( c(1,2,3) )
SE.fisherskew( c(1,2,3) )

}
\references{
\insertAllCited{}
}
