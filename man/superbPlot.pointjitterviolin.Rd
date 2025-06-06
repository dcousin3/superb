% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functionsPlotting_moderate.R
\name{superbPlot.pointjitterviolin}
\alias{superbPlot.pointjitterviolin}
\title{superbPlot point, jitter and violin plot layout}
\usage{
superbPlot.pointjitterviolin(
  summarydata,
  xfactor,
  groupingfactor,
  addfactors,
  rawdata,
  pointParams = list(),
  jitterParams = list(),
  violinParams = list(),
  errorbarParams = list(),
  facetParams = list()
)
}
\arguments{
\item{summarydata}{a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;}

\item{xfactor}{a string with the name of the column where the factor going on the horizontal axis is given;}

\item{groupingfactor}{a string with the name of the column for which the data will be grouped on the plot;}

\item{addfactors}{a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";}

\item{rawdata}{always contains "DV" for each participants and each level of the factors}

\item{pointParams}{(optional) list of graphic directives that are sent to the geom_bar layer}

\item{jitterParams}{(optional) list of graphic directives that are sent to the geom_bar layer}

\item{violinParams}{(optional) list of graphic directives that are sent to the geom_bar layer
this modified geom_violin has additional options "direction"/"antagonize" and "push".}

\item{errorbarParams}{(optional) list of graphic directives that are sent to the geom_superberrorbar layer}

\item{facetParams}{(optional) list of graphic directives that are sent to the facet_grid layer}
}
\value{
a ggplot object
}
\description{
superbPlot comes with a few built-in templates for making the final plots.
All produces ggplot objects that can be further customized. Additionally, it is
possible to add custom-make templates (see vignette 6). The functions, to be
"superbPlot-compatible", must have these parameters:
}
\examples{
# This will make a plot with jittered points and violins for the overall distribution
superb(
   len ~ dose + supp,
   ToothGrowth, 
   plotLayout = "pointjitterviolin" 
)

# if you extract the data with superbData, you can 
# run this layout directly
#processedData <- superb(
#   len ~ dose + supp, 
#   ToothGrowth, 
#   showPlot = FALSE
#)
#
#superbPlot.pointjitterviolin(processedData$summaryStatistic,
#   "dose",
#   "supp",
#   ".~.",
#   processedData$rawData)

}
