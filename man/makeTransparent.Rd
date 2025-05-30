% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeTransparent.R
\name{makeTransparent}
\alias{makeTransparent}
\title{makes ggplots with transparent elements}
\usage{
makeTransparent()
}
\value{
does not return anything; set the elements to transparent.
}
\description{
makeTransparent is an extension to ggplots
which makes all the elements of the plot transparent
except the data being displayed. This is useful to
superimpose multiple plots, e.g. to generate plots with
multiple error bars for example.
}
\examples{

# make a basic plot
superb(len ~ dose + supp, ToothGrowth ) 
# make a basic plot with transparent elements
superb(len ~ dose + supp, ToothGrowth,  
  ) + makeTransparent()

}
