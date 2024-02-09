## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)
options(superb.feedback = 'none')

## -----------------------------------------------------------------------------
superb:::is.superbPlot.function("superbPlot.line")

## -----------------------------------------------------------------------------
testdata <- GRD(
    RenameDV   = "score", 
    SubjectsPerGroup = 25, 
    BSFactors  = "Difficulty(3)", 
    WSFactors  = "Day(day1, day2)",
    Population = list(mean = 65,stddev = 12,rho = 0.5),
    Effects    = list("Day" = slope(-5), "Difficulty" = slope(3) )
)
head(testdata)

## -----------------------------------------------------------------------------
mp <- function(data, style, ...) {
    superbPlot(data,
        WSFactors = "Day(2)",
        BSFactors = "Difficulty",
        variables = c("score.day1", "score.day2"),
        adjustments = list(purpose="difference", decorrelation="CA"),
        plotStyle = style,
        ...
    )+labs(title = paste("Layout is ''",style,"''",sep=""))
}

## ---- fig.width= 7, fig.height = 7, fig.cap = "**Figure 1a**. Look of the six built-in layouts on the same random dataset"----
p1 <- mp(testdata, "bar")
p2 <- mp(testdata, "point")
p3 <- mp(testdata, "line")
p4 <- mp(testdata, "pointjitter" )
p5 <- mp(testdata, "pointjitterviolin")
p6 <- mp(testdata, "pointindividualline")

library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2)

## ---- fig.width =7, fig.height = 3.5, fig.cap = "**Figure 1b**. The seventh layout, the raincloud"----
mp(testdata, "raincloud") + coord_flip()

## ---- fig.width= 5, fig.height = 3, fig.cap = "**Figure 1c**. Box plot of the data"----
mp(testdata, "boxplot", statistic = "median", pointParams = list(color="orange"))

## ---- fig.width= 7, fig.height = 7, fig.cap = "**Figure 2a**. The six built-in template with ornamental styling added."----
ornate = list( 
    scale_colour_manual( name = "Difference", 
        labels = c("Easy", "Hard", "Unthinkable"), 
        values = c("blue", "black", "purple")) ,
    scale_fill_manual( name = "Difference", 
        labels = c("Easy", "Hard", "Unthinkable"), 
        values = c("blue", "black", "purple")) ,
    scale_shape_manual( name = "Difference", 
        labels = c("Easy", "Hard", "Unthinkable") ,
        values = c(0, 10, 13)) ,
    theme_bw(base_size = 9) ,
    labs(x = "Days of test", y = "Score in points" ),
    scale_x_discrete(labels=c("1" = "Former day", "2" = "Latter day"))
)
library(gridExtra)
grid.arrange(
    p1+ornate, p2+ornate, p3+ornate,
    p4+ornate, p5+ornate, p6+ornate,
    ncol=2)

## -----------------------------------------------------------------------------
superbPlot.foo <- function(
    summarydata,
    xfactor,       
    groupingfactor,
    addfactors, 
    rawdata        
    # any optional argument you wish
) {
    plot <- ggplot() ## ggplot instructions...        
    return(plot)
}

## ---- message=FALSE, echo=FALSE, fig.height=3, fig.width=4, fig.cap="**Figure 3**. Mean score with 95% confidence interval using the ``simple`` plot layout."----
superbPlot.simple <- function( summarydata, xfactor, groupingfactor, addfactors, rawdata ) {
    plot <- ggplot(
        data = summarydata,
        mapping = aes( x = !!sym(xfactor), y = center, group= !!sym(groupingfactor))
    ) +
    geom_point( ) +
    geom_errorbar( mapping = aes(ymin = center + lowerwidth, ymax = center + upperwidth)  )+
    facet_grid( addfactors )
       
    return(plot)
}
superbPlot(TMB1964r,
    WSFactors = "T(7)",
    BSFactors = "Condition",
    variables = c("T1","T2","T3","T4","T5","T6","T7"),
    plotStyle = "simple"
)

## -----------------------------------------------------------------------------
superbPlot.simple <- function(
    summarydata, xfactor, groupingfactor, addfactors, rawdata
) {
    plot <- ggplot(
        data = summarydata, 
        mapping = aes( x = !!sym(xfactor), y = center, group = !!sym(groupingfactor) )
    ) +
    geom_point(  ) +
    geom_errorbar( mapping = aes(ymin = center + lowerwidth, 
                                 ymax = center + upperwidth)  )+ 
    facet_grid( addfactors )
        
    return(plot)
}

## -----------------------------------------------------------------------------
superb:::is.superbPlot.function("superbPlot.simple")

## ---- message=FALSE, echo=TRUE, results='hide', fig.show='hide'---------------
superbPlot(TMB1964r,
     WSFactors = "T(7)",      
     BSFactors = "Condition",
     variables = c("T1","T2","T3","T4","T5","T6","T7"),
     plotStyle = "simple"
)

## ---- eval = FALSE, message=FALSE, echo=TRUE, error=FALSE, results='hide'-----
#      do.call( geom_point, modifyList(
#         list( size= 3 ##etc., the default directives##
#         ), myownParams
#      ))

## -----------------------------------------------------------------------------
superbPlot.simpleWithOptions <- function(
    summarydata, xfactor, groupingfactor, addfactors, rawdata,
    myownParams = list()  ## will be used to add the optional arguments to the function
) {
    plot <- ggplot(
        data = summarydata, 
        mapping = aes( x = !!sym(xfactor), y = center, group=Condition)
    ) +
    do.call( geom_point, modifyList(
       list( color ="black" ),
        myownParams
    )) + 
    do.call( geom_errorbar, modifyList(
        list( mapping = aes(ymin = center + lowerwidth, 
                            ymax = center + upperwidth)  ),
        myownParams 
    )) + 
    facet_grid( addfactors )
        
    return(plot)
}
superb:::is.superbPlot.function("superbPlot.simpleWithOptions")

## ---- message=FALSE, eval=FALSE, echo=TRUE, results='hide', fig.show='show', fig.cap="**Figure 4**. A simple figure with optional arguments"----
#  superbPlot(TMB1964r,
#      WSFactors = "T(7)",
#      BSFactors = "Condition",
#      variables = c("T1","T2","T3","T4","T5","T6","T7"),
#      plotStyle = "simpleWithOptions",
#      ## here goes the optional arguments
#      myownParams = list(size=1, color="purple", position = position_dodge(width = 0.3) )
#  )

## ---- eval=TRUE, message=TRUE, echo=TRUE--------------------------------------
options(superb.feedback = 'all')
runDebug( 'where are we?', "Text to show when we get there",
  c("variable1", "variable2", "etc"),
  list( "var1InTheFct", "var2InTheFct", "varetcInTheFct")
)

## ---- eval=TRUE, message=TRUE, echo=TRUE, fig.show='hide'---------------------
superbPlot.empty <- function(
    summarydata, xfactor, groupingfactor, addfactors, rawdata 
) {
    runDebug( 'inempty', "Dumping the two dataframes",
        c("summary","raw"), list(summarydata, rawdata))

    plot <- ggplot() # an empty plot        
    return(plot)
}
options(superb.feedback = 'inempty')  ## turn on feedback when reaching 'inempty'
superbPlot(TMB1964r,
     WSFactors = "T(7)",      
     BSFactors = "Condition",
     variables = c("T1","T2","T3","T4","T5","T6","T7"),
     plotStyle = "empty" 
)

## ---- message=FALSE, echo=TRUE, results='hide', fig.show='hide'---------------
superbPlot.simple(summary, "T", "Condition", ".~.", raw)

## ---- message=FALSE, echo=TRUE, warning=FALSE---------------------------------
# install.packages("emojifont")
library(emojifont)

## -----------------------------------------------------------------------------
superbPlot.smiley <- function( summarydata, xfactor, groupingfactor, addfactors, rawdata ) {
    # the early part bears on summary data with variable "center"
    plot <- ggplot(
        data    = summarydata, 
        mapping = aes(
            x      = !!sym(xfactor), 
            y      = center, 
            fill   = !!sym(groupingfactor), 
            shape  = !!sym(groupingfactor), 
            colour = !!sym(groupingfactor) )
    ) +
    geom_point(position = position_dodge(width = .95)) +
    geom_errorbar( width = .6, position = position_dodge(.95), 
              mapping = aes(ymin = center + lowerwidth, ymax = center + upperwidth) 
    )+ 
    # this part bears on the rawdata only with variable "DV"
    geom_text(data = rawdata, 
              position = position_jitter(0.5),
              family   = "EmojiOne",
              label    = emoji("smile"), 
              size     = 6, 
              mapping  = aes(x = !!sym(xfactor), y = DV, group = !!sym(groupingfactor) )
    ) +
    facet_grid( addfactors )
        
    return(plot)
}

## -----------------------------------------------------------------------------
superb:::is.superbPlot.function("superbPlot.smiley")

## ---- fig.width= 4, fig.height = 3, fig.cap = "**Figure 5**. smile!"----------
superbPlot(TMB1964r,
     WSFactors = "T(7)",      
     BSFactors = "Condition", 
     variables = c("T1","T2","T3","T4","T5","T6","T7"),
     plotStyle = "smiley"
)

