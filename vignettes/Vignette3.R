## ---- echo = FALSE, warning=FALSE, message = FALSE,  results = 'hide'---------
cat("this will be hidden; use for general initializations.")
library(superb)
library(ggplot2)

## -----------------------------------------------------------------------------
head(dataFigure2)

## -----------------------------------------------------------------------------
t.test(dataFigure2$pre, dataFigure2$post, var.equal=TRUE)

## -----------------------------------------------------------------------------
t.test(dataFigure2$pre, dataFigure2$post, var.equal=TRUE, paired = TRUE)

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 1**. Representation of the individual participants"----
library(reshape2)

# first transform the data in long format; the pre-post scores will go into column "variable"
dl <- melt(dataFigure2, id="id")

# add transparency when pre is smaller or equal to post
dl$trans = ifelse(dataFigure2$pre <= dataFigure2$post,0.9,1.0)

# make a plot, with transparent lines when the score increased
ggplot(data=dl, aes(x=variable, y=value, group=id, alpha = trans)) + 
    geom_line( ) +
    coord_cartesian( ylim = c(70,150) ) +
    geom_abline(intercept = 102.5, slope = 0, colour = "red", linetype=2)

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 2**. Representation of the *subject-centered* individual participants"----
# use subjectCenteringTransform function 
library(superb)
df2 <- subjectCenteringTransform(dataFigure2, c("pre","post"))

# tranform into long format
library(reshape2)
dl2 <- melt(df2, id="id")

# make the plot
ggplot(data=dl2, aes(x=variable, y=value, colour=id, group=id)) + geom_line()+
    coord_cartesian( ylim = c(70,150) ) +
    geom_abline(intercept = 102.5, slope = 0, colour = "red", size = 0.5, linetype=2)

## -----------------------------------------------------------------------------
t.test(dataFigure2$pre, dataFigure2$post, paired=TRUE)

## -----------------------------------------------------------------------------
cor(dataFigure2$pre, dataFigure2$post)

## ---- message=FALSE, warning=FALSE, echo=TRUE, fig.height = 3, fig.width = 4, fig.cap="**Figure 3a**. Means and difference and correlation-adjusted 95% confidence intervals"----
superbPlot(dataFigure2, 
	WSFactors    = "Moment(2)", 
	adjustments = list(
					purpose = "difference", 
					decorrelation = "CA"    ## NEW! use a decorrelation technique
	), 
	variables   = c("pre","post"), 
	plotStyle   = "line" )

## ---- message=FALSE, warning=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 3b**. Means and 95% confidence intervals on raw data (left) and on decorrelated data (right)"----
options(superb.feedback = 'none') # shut down 'warnings' and 'design' interpretation messages
library(gridExtra)

## realize the plot with unadjusted (left) and ajusted (right) 95\% confidence intervals
plt2a <- superbPlot(dataFigure2, 
        WSFactors    = "Moment(2)", 
        adjustments = list(purpose = "difference"), 
        variables   = c("pre","post"), 
        plotStyle   = "line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="Difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=10) + 
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))
plt2b <- superbPlot(dataFigure2, 
        WSFactors    = "Moment(2)", 
        adjustments = list(purpose = "difference", decorrelation = "CA"),  #only difference
        variables   = c("pre","post"), 
        plotStyle   = "line" ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="Correlation and difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) + 
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))
plt2  <- grid.arrange(plt2a,plt2b,ncol=2)

