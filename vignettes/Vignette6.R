## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)

## -----------------------------------------------------------------------------
dta <- GRD()
head(dta)

## -----------------------------------------------------------------------------
dta <- GRD( RenameDV = "score" )

## -----------------------------------------------------------------------------
dta <- GRD( BSFactors = 'Group(3)')

## -----------------------------------------------------------------------------
dta <- GRD( BSFactors = c('Surgery(2)', 'Therapy(3)') )

## -----------------------------------------------------------------------------
dta <- GRD(
    BSFactors = c('Surgery(yes, no)', 'Therapy(CBT,Control,Exercise)')
)
unique(dta$Surgery)
unique(dta$Therapy)

## -----------------------------------------------------------------------------
dta <- GRD(
    BSFactors = c('Surgery(yes,no)', 'Therapy(CBT, Control,Exercise)'),
    WSFactors = 'Contrast(C1,C2,C3)',
)

## -----------------------------------------------------------------------------
dta <- GRD(
    BSFactors = "Therapy(3)",
    SubjectsPerGroup = c(2, 5, 1)
)
dta

## ---- dpi=72, fig.height=3, fig.width=4---------------------------------------
dta <- GRD(
    RenameDV = "IQ",
    Population=list(mean=100,stddev=15)
)
hist(dta$IQ)

## -----------------------------------------------------------------------------
dta <- GRD(
    BSFactors = "Group(2)",
    Population = list(
        mean   = 100,         # this set GM to 100
        stddev = 15,        # this set STDDEV to 15
        scores = "rnorm(1, mean = GM, sd = STDDEV )"
    )
)

## ---- dpi=72, fig.height=3, fig.width=4---------------------------------------
dta <- GRD(
    BSFactors = "Group(2)",
    Population = list(
        mean   = 100,       # this set GM to 100
        stddev = 15,        # this set STDDEV to 15
        scores = "rnorm(1, mean = GM, sd = Group * STDDEV )"
    )
)
superbPlot(dta,
    BSFactors = "Group",
    variables = "DV",
    plotStyle = "pointjitterviolin" )

## ---- dpi=72, fig.height=3, fig.width=4---------------------------------------
dta <- GRD(SubjectsPerGroup = 5000,
    RenameDV = "RT",
    Population=list(
        scores = "rweibull(1, shape=2, scale=40)+250"
    )
)
hist(dta$RT,breaks=seq(250,425,by=5))

## ---- message=FALSE, dpi=72, fig.height=3, fig.width=4------------------------
dta <- GRD(
    BSFactors = 'Therapy(CBT, Control, Exercise)',
    WSFactors = 'Contrast(3)',
    SubjectsPerGroup = 1000,
    Effects = list('Contrast' = slope(2))
)
superbPlot(dta,
    BSFactors = "Therapy", WSFactors = "Contrast(3)",
    variables = c("DV.1","DV.2","DV.3"),
    plotStyle = "line" )

## ---- message=FALSE, dpi=72, fig.height=3, fig.width=4------------------------
dta <- GRD(
    BSFactors = 'Therapy(CBT,Control,Exercise)',
    WSFactors = 'Contrast(3) ',
    SubjectsPerGroup = 1000,
    Effects = list(
        "code1"=Rexpression("if (Therapy =='CBT'){-1} else {0}"),
        "code2"=Rexpression("if (Contrast ==3) {+1} else {0}")
    )
)
superbPlot(dta,
    BSFactors = "Therapy", WSFactors = "Contrast(3)",
    variables = c("DV.1","DV.2","DV.3"),
    plotStyle = "line" )

## ---- dpi=72, fig.width=4, fig.height=4---------------------------------------
dta <- GRD(
    WSFactors = 'Difficulty(1, 2)',
    SubjectsPerGroup = 1000,
    Population=list(mean = 0,stddev = 20, rho = 0.5)
)
plot(dta$DV.1, dta$DV.2)

## ---- dpi=72, fig.width=4, fig.height=4---------------------------------------
dta <- GRD(
    WSFactors = 'Difficulty(1, 2)',
    SubjectsPerGroup = 1000,
    Population=list(mean = c(10,2),stddev= c(1,0.2),rho =-0.85)
)
plot(dta$DV.1, dta$DV.2)

## ---- dpi=72, fig.height=3, fig.width=4---------------------------------------
dta <- GRD(SubjectsPerGroup = 5000,
    Population= list( mean=100, stddev = 15 ),
    Contaminant=list( mean=200, stddev = 15, proportion = 0.10 )
)
hist(dta$DV,breaks=seq(-25,300,by=2.5))

## ---- dpi=72, fig.height=3, fig.width=4---------------------------------------
dta <- GRD(SubjectsPerGroup = 10000,
    Population=list( mean=100, stddev = 15 ),
    Contaminant=list( proportion = 0.10,
        scores="rweibull(1,shape=1.5, scale=30)+1.5*GM")
)
hist(dta$DV,breaks=seq(0,365,by=2.5))

## -----------------------------------------------------------------------------
dta <- GRD( BSFactors="grp(2)",
    WSFactors = "Moment (2)",
    SubjectsPerGroup = 1000,
    Effects = list("grp" = slope(100) ),
    Population=list(mean=0,stddev=20,rho= -0.85),
    Contaminant=list(scores = "NA", proportion=0.2)
)

