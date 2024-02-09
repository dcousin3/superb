## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide', warning = FALSE----
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)
library(png)
options(superb.feedback = c('design','warnings') )

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  setwd("c:")
#  file <- "Demo_SPSS.sav"

## ---- echo=FALSE, results="hide"----------------------------------------------
file <- system.file("extdata", "SPSS_Demo.sav", package = "superb")

## -----------------------------------------------------------------------------
library(foreign)
data <- read.spss(file, to.data.frame = TRUE)

## ---- fig.width = 4, eval=FALSE, fig.cap="**Figure 1: A plot with SPSS data within R**"----
#  superbPlot( data,
#      WSFactors      = "Temps(2)",
#      variables      = c("time1","time2"),
#      plotStyle      = "line",
#      adjustments    = list(purpose = "single",
#                            decorrelation = "CA"),
#      errorbarParams = list(color = "purple"),
#      pointParams    = list( size = 2, color = "purple")
#  )

## ----fig2, echo=FALSE, fig.cap="**Figure 2: Syntax to generate a plot**", fig.width = 8, fig.height = 8----
library(png)
fle1 <- system.file("extdata", "Syntax1.png", package = "superb")
img1 <- readPNG(fle1)
plot.new() 
rasterImage(img1, 0, 0, 1, 1)

