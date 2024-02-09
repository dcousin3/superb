## ---- message=FALSE, warning=FALSE, echo=TRUE, eval=TRUE----------------------
## Load relevant packages
library(superb)             # for superbPlot
library(ggplot2)            # for all the graphic directives
library(gridExtra)          # for grid.arrange

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
head(dataFigure1)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 1a**. Left panel of Figure 1."----
plt1a <- superbPlot(dataFigure1, 
            BSFactors   = "grp", 
            variables   = "score", 
            plotStyle   = "line" ) 
plt1a

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
ornateBS <- list(
    xlab("Group"), 
    ylab("Attitude towards class activities"),
    scale_x_discrete(labels = c("Collaborative\ngames", "Unstructured\nactivities")), #new!
    coord_cartesian( ylim = c(70,130) ),
    geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 10) +
    theme( plot.subtitle = element_text(size=12))
)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 1b**. Decorating left panel of Figure 1."----
plt1a <- plt1a + ornateBS + labs(subtitle="(stand-alone)\n95% CI")
plt1a

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 1c**. Making and decorating central panel of Figure 1."----
plt1b <- superbPlot(dataFigure1, 
            BSFactors    = "grp", 
            variables    = "score", 
            adjustments  = list(purpose = "difference"), #new!
            plotStyle    = "line" )
plt1b <- plt1b + ornateBS + labs(subtitle="Difference-adjusted\n95% CI") 
plt1b

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 1d**. Making and decorating right panel of Figure 1."----
plt1c <- superbPlot(dataFigure1, 
            BSFactors    = "grp", 
            variables    = "score", 
            adjustments  = list(purpose = "difference"),
            plotStyle    = "raincloud",                         # new layout!
            violinParams = list(fill = "green", alpha = 0.2) ) # changed color to the violin
plt1c <- plt1c + ornateBS + labs(subtitle="Difference-adjusted\n95% CI") 
plt1c

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=9, fig.height=4, fig.cap="**Figure 1**. The complete Figure 1."----
grid.arrange(plt1a, plt1b, plt1c, ncol=3)

## ---- message=FALSE, echo=TRUE, eval=FALSE------------------------------------
#  png(filename = "Figure1.png", width = 640, height = 320)
#  grid.arrange(plt1a, plt1b, plt1c, ncol=3)
#  dev.off()

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
t.test(dataFigure1$score[dataFigure1$grp==1],
       dataFigure1$score[dataFigure1$grp==2], 
       )

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
ornateWS <- list(
    xlab("Moment"),                                                #different!
    scale_x_discrete(labels=c("Pre\ntreatment", "Post\ntreatment")), 
    ylab("Statistics understanding"),
    coord_cartesian( ylim = c(75,125) ),
    geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 10) +
    theme( plot.subtitle = element_text(size=12))
)

## -----------------------------------------------------------------------------
head(dataFigure2)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 2a**. Making left panel of Figure 2."----
plt2a <- superbPlot(dataFigure2, 
            WSFactors    = "Moment(2)", 
            variables    = c("pre","post"), 
            adjustments  = list(purpose = "single"),
            plotStyle    = "line" ) 
plt2a <- plt2a + ornateWS + labs(subtitle="Stand-alone\n95% CI")
plt2a

## ---- message=TRUE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 2b**. Making central panel of Figure 2."----
plt2b <- superbPlot(dataFigure2, 
            WSFactors    = "Moment(2)", 
            variables    = c("pre","post"), 
            adjustments  = list(purpose = "difference", decorrelation = "CA"), #new
            plotStyle    = "line" ) 
plt2b <- plt2b + ornateWS + labs(subtitle="Correlation and difference-\nadjusted 95% CI") 
plt2b

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 2c**. Making third panel of Figure 2."----
plt2c <- superbPlot(dataFigure2, 
            WSFactors    = "Moment(2)", 
            variables    = c("pre","post"), 
            adjustments  = list(purpose = "difference", decorrelation = "CA"),
            plotStyle    = "pointindividualline" )   #new
plt2c <- plt2c + ornateWS + labs(subtitle="Correlation and difference-\nadjusted 95% CI")
plt2c 

## -----------------------------------------------------------------------------
ornateWS2 <- list(
    xlab("Difference"),                                      
    scale_x_discrete(labels=c("Post minus Pre\ntreatment")), 
    ylab("Statistics understanding"),
    coord_cartesian( ylim = c(-25,+25) ),
    geom_hline(yintercept = 0, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 10) +
    theme( plot.subtitle = element_text(size=12))
)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 2d**. Making right panel of Figure 2."----
dataFigure2$diff <- dataFigure2$post - dataFigure2$pre
plt2d <- superbPlot(dataFigure2, 
            WSFactor     = "Moment(1)", 
            variables    = c("diff"), 
            adjustments  = list(purpose = "single", decorrelation = "none"),
            plotStyle    = "raincloud",
            violinParams = list(fill = "green") )  #new
plt2d <- plt2d + ornateWS2 + labs(subtitle="95% CI \nof the difference")
plt2d

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=9, fig.height=4, fig.cap="**Figure 2**. The complete Figure 2."----
grid.arrange(plt2a, plt2b, plt2c, plt2d,  ncol=4)

## ---- message=FALSE, echo=TRUE, eval=FALSE------------------------------------
#  png(filename = "Figure2.png", width = 850, height = 320)
#  grid.arrange(plt2a, plt2b, plt2c, plt2d,  ncol=4)
#  dev.off()

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
t.test(dataFigure2$pre, dataFigure2$post, paired=TRUE)

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
ornateCRS <- list(
    xlab("Group"), 
    ylab("Quality of policies"),
    scale_x_discrete(labels=c("From various\nfields", "From the\nsame field")), #new!
    coord_cartesian( ylim = c(75,125) ),
    geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 10) +
    theme( plot.subtitle = element_text(size=12))
)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 3a**. The left panel of Figure 3."----
plt3a <- superbPlot(dataFigure3, 
    BSFactors     = "grp", 
    variables     = "VD", 
    adjustments   = list(purpose = "single", samplingDesign = "SRS"),
    plotStyle     = "line" )
plt3a <- plt3a + ornateCRS + labs(subtitle="Stand-alone\n95% CI") 
plt3a

## ---- message=TRUE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 3b**. The central panel of Figure 3."----
plt3b <- superbPlot(dataFigure3, 
    BSFactors     = "grp", 
    variables     = "VD", 
    adjustments   = list(purpose = "difference", samplingDesign = "CRS"), #new
    plotStyle     = "line", 
    clusterColumn = "cluster" )                                           #new
plt3b <- plt3b + ornateCRS + labs(subtitle="Cluster and difference-\nadjusted 95% CI")
plt3b

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=8, fig.height=4, fig.cap="**Figure 3c**. The right panel of Figure 3."----
plt3c <- superbPlot(dataFigure3, 
    BSFactors    = "grp", 
    variables   = "VD", 
    adjustments = list(purpose = "difference", samplingDesign = "CRS"),
    plotStyle   = "raincloud", 
    violinParams = list(fill = "green", alpha = 0.2),
    clusterColumn = "cluster" )
plt3c <- plt3c + ornateCRS + labs(subtitle="Cluster and difference-\nadjusted 95% CI")

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=8, fig.height=4, fig.cap="**Figure 3**. The complete Figure 3."----
grid.arrange(plt3a, plt3b, plt3c, ncol=3)

## ---- message=FALSE, echo=TRUE, eval=FALSE------------------------------------
#  png(filename = "Figure3.png", width = 640, height = 320)
#  grid.arrange(plt3a, plt3b, plt3c, ncol=3)
#  dev.off()

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
res    <- t.test( dataFigure3$VD[dataFigure3$grp==1], 
                  dataFigure3$VD[dataFigure3$grp==2], 
                )
# mean ICCs per group, as given by superbPlot
micc   <- mean(c(0.491335, 0.203857)) 
# lambda from five clusters of 5 participants each
lambda <- CousineauLaurencelleLambda(c(micc, 5, 5, 5, 5, 5, 5)) 
tcorrected  <- res$statistic / lambda
pcorrected  <- 1 - pt(tcorrected, 4)

cat(paste("t-test corrected for cluster-randomized sampling: t(",
    2*(dim(dataFigure3)[1]-2),") = ", round(tcorrected, 3),
    ", p = ", round(pcorrected, 3),"\n", sep= ""))

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
ornateBS <- list(
    xlab(""), 
    ylab("Metabolic score"),
    scale_x_discrete(labels=c("Response to treatment")), #new!
    coord_cartesian( ylim = c(75,125) ),
    geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2),
    theme_light(base_size = 10) +
    theme( plot.subtitle = element_text(size=12))
)

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 4a**. The left panel of Figure 4."----
plt4a <- superbPlot(dataFigure4, 
    BSFactors = "group", 
    variables = "score", 
    adjustments=list(purpose = "single", popSize = Inf),
    plotStyle="line" ) 
plt4a <- plt4a + ornateBS + labs(subtitle="Stand-alone\n95% CI") 

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 4b**. The central panel of Figure 3b."----
plt4b <- superbPlot(dataFigure4, 
    BSFactors = "group",
    variables = "score", 
    adjustments=list(purpose = "single", popSize = 50 ), # new!
    plotStyle="line" ) 
plt4b <- plt4b + ornateBS + labs(subtitle="Population size-\nadjusted 95% CI") 

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=3, fig.height=4, fig.cap="**Figure 4c**. The right panel of Figure 3b."----
plt4c <- superbPlot(dataFigure4, 
    BSFactors = "group",
    variables = "score", 
    adjustments=list(purpose = "single", popSize = 50 ), # new!
    plotStyle="pointjitterviolin",
    violinParams = list(fill = "green", alpha = 0.2)  ) 
plt4c <- plt4c + ornateBS + labs(subtitle="Population size-\nadjusted 95% CI") 

## ---- message=FALSE, echo=TRUE, eval=TRUE, fig.width=9, fig.height=4, fig.cap="**Figure 4**. The complete Figure 4."----
plt4 <- grid.arrange(plt4a, plt4b, plt4c, ncol=3)

## ---- message=FALSE, echo=TRUE, eval=FALSE------------------------------------
#  png(filename = "Figure4.png", width = 640, height = 320)
#  grid.arrange(plt4a, plt4b, plt4c, ncol=3)
#  dev.off()

## ---- message=FALSE, echo=TRUE, eval=TRUE-------------------------------------
res <- t.test(dataFigure4$score, mu=100)
tcorrected <- res$statistic /sqrt(1-nrow(dataFigure4) / 50)
pcorrected <- 1-pt(tcorrected, 24)

cat(paste("t-test corrected for finite-population size: t(",
    nrow(dataFigure4)-1,") = ", round(tcorrected, 3),
    ", p = ", round(pcorrected, 3),"\n", sep= ""))

