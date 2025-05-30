% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functionsPlotting_ribbon.R
\name{superbPlot.lineBand}
\alias{superbPlot.lineBand}
\title{superbPlot 'lineBand' layout}
\usage{
superbPlot.lineBand(
  summarydata,
  xfactor,
  groupingfactor,
  addfactors,
  rawdata,
  pointParams = list(),
  lineParams = list(),
  errorbandParams = list(),
  facetParams = list(),
  xAsFactor = TRUE
)
}
\arguments{
\item{summarydata}{a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;}

\item{xfactor}{a string with the name of the column where the factor going on the horizontal axis is given;}

\item{groupingfactor}{a string with the name of the column for which the data will be grouped on the plot;}

\item{addfactors}{a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";}

\item{rawdata}{always contains "DV" for each participants and each level of the factors}

\item{pointParams}{(optional) list of graphic directives that are sent to the geom_point layer}

\item{lineParams}{(optional) list of graphic directives that are sent to the geom_jitter layer}

\item{errorbandParams}{(optional) list of graphic directives that are sent to the geom_ribbon layer}

\item{facetParams}{(optional) list of graphic directives that are sent to the facet_grid layer}

\item{xAsFactor}{(optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)}
}
\value{
a ggplot object
}
\description{
The lineBand layout displays an error band instead of individual error bars. This layout is
convenient when you have many points on your horizontal axis (so that the error bars are difficult to distinguish)
and when the results are fairly smooth.

The functions has these parameters:
}
\examples{
# this creates a fictious time series at 100 time points obtained in two conditions:
dta <- GRD( WSFactors = "timepoints (50) : condition(2)", 
    SubjectsPerGroup = 20,
    RenameDV = "activation",
    Effects = list("timepoints" = extent(5), "condition" = extent(3) ),
    Population=list(mean=50,stddev=10,rho=0.75)
)

# This will make a plot with error band
superb(
   crange(activation.1.1, activation.50.2) ~ ., 
   dta, 
   WSFactors   = c("timepoints(50)", "condition(2)"),
   adjustments = list(
        purpose       = "single",
        decorrelation = "CM"        ## or none for no decorrelation
   ),
   plotLayout  = "lineBand",        # note the uppercase B 
   pointParams = list(size= 1)      # making points smaller has better look
)

# if you extract the data with superbData, you can 
# run this layout directly
#processedData <- superb(
#   crange(activation.1.1, activation.50.2) ~ ., 
#   dta, 
#   WSFactors   = c("timepoints(50)", "condition(2)"), variables = colnames(dta)[2:101],
#   adjustments = list(
#        purpose       = "single",
#        decorrelation = "CM"        ## or none for no decorrelation
#   )
#)
#
#superbPlot.lineBand(processedData$summaryStatistic,
#   "timepoints",
#   "condition",
#   ".~.",
#   processedData$rawData)

}
\references{
\insertAllCited{}
}
