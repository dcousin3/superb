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
        adjustments = list(purpose="difference", decorrelation="CM"),
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
        mapping = aes_string( x = xfactor, y = "center", group= groupingfactor)
    ) +
    geom_point( ) +
    geom_errorbar( mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth")  )+
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
        mapping = aes_string( x = xfactor, y = "center", group=groupingfactor)
    ) +
    geom_point(  ) +
    geom_errorbar( mapping = aes_string(ymin = "center + lowerwidth", 
                                        ymax = "center + upperwidth")  )+ 
    facet_grid( addfactors )
        
    return(plot)
}

## -----------------------------------------------------------------------------
superb:::is.superbPlot.function("superbPlot.simple")

