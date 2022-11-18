## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.")
library(superb)
library(ggplot2)

## ---- message=FALSE, echo=FALSE, fig.height=4, fig.width=3, fig.cap="**Figure 1**. Mean scores along with 95% confidence interval for two groups of students on the quality of learning behavior."----
superbPlot(dataFigure1, 
        BSFactors = "grp", 
        variables = "score", 
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="(stand-alone)\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative\ngames", "2" = "Unstructured\nactivity"))

## ---- message=FALSE, warning=FALSE, echo=TRUE---------------------------------
t.test(dataFigure1$score[dataFigure1$grp==1], 
        dataFigure1$score[dataFigure1$grp==2],
        var.equal=T)

## ---- message=FALSE, echo=TRUE------------------------------------------------
t.test(dataFigure1$score[dataFigure1$grp==1], mu=100)

## ---- message=FALSE, echo=TRUE------------------------------------------------
t.test(dataFigure1$score[dataFigure1$grp==2], mu=105)

## ---- message=FALSE, echo=TRUE, fig.height=4, fig.width=3, fig.cap="**Figure 2**. Mean scores along with difference-adjusted 95% confidence interval for two groups of students on the quality of learning behavior."----
superbPlot(dataFigure1, 
        BSFactors = "grp", 
        adjustments=list(purpose = "difference"),  # the only new thing here
        variables = "score", 
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="Difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) + 
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative\ngames", "2" = "Unstructured\nactivity"))

## ---- message=FALSE, echo=TRUE, fig.height=4, fig.cap="**Figure 3**. Two representation of the data with unadjusted (left) and adjusted (right) 95% confidence intervals"----
library(gridExtra)
plt1 <- superbPlot(dataFigure1, 
        BSFactors = "grp", 
        variables = "score", 
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="(stand-alone)\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative\ngames", "2" = "Unstructured\nactivity")) 

plt2 <- superbPlot(dataFigure1, 
        BSFactors = "grp", 
        adjustments=list(purpose = "difference"), 
        variables = "score", 
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="Difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) + 
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative\ngames", "2" = "Unstructured\nactivity")) 

plt <- grid.arrange(plt1, plt2, ncol=2)

## ---- message=FALSE, echo=TRUE, fig.height=4, fig.width=6, fig.cap="**Figure 4**. Two representations of the results with adjusted and unadjusted error bars on the same plot"----
# generate the two plots, nudging the error bars, using distinct colors, and 
# having the second plot's background transparent (with ``makeTransparent()`` )
plt1 <- superbPlot(dataFigure1, 
        BSFactors = "grp", 
        variables = "score", 
        errorbarParams = list(color="blue",position = position_nudge(-0.05) ),
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="(red) Difference-adjusted 95% confidence intervals\n(blue) (stand-alone) 95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative\ngames", "2" = "Unstructured\nactivity")) 


plt2 <- superbPlot(dataFigure1, 
        BSFactors = "grp", 
        adjustments=list(purpose = "difference"), 
        variables = "score", 
        errorbarParams = list(color="red",position = position_nudge(0.05) ),
        plotStyle="line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="(red) Difference-adjusted 95% confidence intervals\n(blue) (stand-alone) 95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) + 
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative\ngames", "2" = "Unstructured\nactivity"))

# transform the ggplots into "grob" so that they can be manipulated
plt1g <- ggplotGrob(plt1)
plt2g <- ggplotGrob(plt2 + makeTransparent() )

# put the two grob onto an empty ggplot (as the positions are the same, they will be overlayed)
ggplot() + 
    annotation_custom(grob=plt1g) + 
    annotation_custom(grob=plt2g)

