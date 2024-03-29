% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functionsStatistical.R
\name{bootstrapPrecisionMeasures}
\alias{bootstrapPrecisionMeasures}
\alias{bootstrapSE.mean}
\alias{bootstrapPI.mean}
\alias{bootstrapSE.median}
\alias{bootstrapPI.median}
\alias{bootstrapSE.hmean}
\alias{bootstrapPI.hmean}
\alias{bootstrapSE.gmean}
\alias{bootstrapPI.gmean}
\alias{bootstrapSE.var}
\alias{bootstrapPI.var}
\alias{bootstrapSE.sd}
\alias{bootstrapPI.sd}
\title{Bootstrapped measures of precision}
\usage{
bootstrapSE.mean(x)

bootstrapPI.mean(x, gamma)

bootstrapSE.median(x)

bootstrapPI.median(x, gamma)

bootstrapSE.hmean(x)

bootstrapPI.hmean(x, gamma)

bootstrapSE.gmean(x)

bootstrapPI.gmean(x, gamma)

bootstrapSE.var(x)

bootstrapPI.var(x, gamma)

bootstrapSE.sd(x)

bootstrapPI.sd(x, gamma)
}
\arguments{
\item{x}{a vector of numbers, the sample data (mandatory);}

\item{gamma}{a confidence level for PI (default 0.95).}
}
\value{
a measure of precision (SE) or an interval of precision (PI).
}
\description{
superb also comes with a few built-in measures of
precisions that uses bootstrap. More can be added based on users needs.
All \code{bootstrapSE.fct()} functions produces an interval width;
all \code{bootstrapPI.fct()} produces the lower and upper limits of an interval.
These estimates are based on 5,000 sub-samples by default. Change this
default with\code{options("superb.bootstrapIter" = number )}.
See \insertCite{et94;textual}{superb} for a comprehensive introduction.
The bootstrap estimates are called PI which stands for Precision intervals.
This is to denote that they estimate the sampling distribution, not the
predictive distribution on which all confidence intervals are based
\insertCite{rpw19,pl10,l99}{superb}.
}
\examples{
# the confidence interval of the mean for default 95\% and 90\% confidence level
bootstrapPI.mean( c(1,2,3) )
bootstrapPI.mean( c(1,2,3), gamma = 0.90)

# Standard errors for standard deviation or variance
bootstrapSE.sd( c(1,2,3) )
bootstrapSE.var( c(1,2,3) )

}
\references{
\insertAllCited{}
}
