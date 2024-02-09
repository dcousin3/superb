## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)
options(superb.feedback = c('design','warnings') )

## -----------------------------------------------------------------------------
Fulldta <- dta <- GRD( 
                WSFactors = c("Nletters(4)","Nuppercase(4)"),
                Effects = list("Nletters" = slope(25),
                               "Nuppercase" = slope(-25) ),
                Population = list(mean = 400, stddev = 50)
)

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 1**. Mean response times to say **Word** or **Non-word**."----
superbPlot(Fulldta,
    WSFactors = c("Nletters(4)","Nuppercase(4)"),
    variables = c("DV.1.1","DV.2.1","DV.3.1","DV.4.1",
                  "DV.1.2","DV.2.2","DV.3.2","DV.4.2",
                  "DV.1.3","DV.2.3","DV.3.3","DV.4.3",
                  "DV.1.4","DV.2.4","DV.3.4","DV.4.4"),
    plotStyle="line"
)

## -----------------------------------------------------------------------------
# destroying the six impossible columns
dta$DV.1.2 = NULL # e.g., the condition showing one letter with 2 upper-cases.
dta$DV.1.3 = NULL
dta$DV.1.4 = NULL
dta$DV.2.3 = NULL
dta$DV.2.4 = NULL
dta$DV.3.4 = NULL

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 2**. Mean response times to say **Word** or **Non-word**."----
superbPlot(dta,
    WSFactors = c("Nletters(4)","Nuppercase(4)"),
    variables = c("DV.1.1","DV.2.1","DV.3.1","DV.4.1","DV.2.2","DV.3.2",
                  "DV.4.2","DV.3.3","DV.4.3","DV.4.4"),
    WSDesign  = list(c(1,1), c(2,1), c(3,1),  c(4,1),  c(2,2),  c(3,2),
                     c(4,2), c(3,3), c(4,3),  c(4,4)),
    plotStyle="line"
)

