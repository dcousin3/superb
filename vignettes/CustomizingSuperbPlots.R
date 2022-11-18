## ---- message=FALSE, warning=FALSE, echo=TRUE, eval=TRUE----------------------
## Load relevant packages
library(superb)             # for superbPlot
library(ggplot2)            # for all the graphic directives
library(gridExtra)          # for grid.arrange

## -----------------------------------------------------------------------------
Astats <- data.frame(
    MNs = c(6.75, 6.00, 5.50, 6.50, 8.00, 8.75),
    SDs = c(2.00, 3.00, 3.50, 3.50, 1.25, 1.25)
)
dtaA <- apply(Astats, 1, 
    function(stat) {rnorm(100, mean=stat[1], sd=stat[2])} 
)
dtaA <- data.frame(dtaA)
colnames(dtaA) <- c("Verbal", "Numerical", "Spatial", "Creativity", "Intrapersonal", "Interpersonal")

Bstats <- data.frame(
    MNs = c(3.33, 3.00, 2.50, 3.00, 2.75, 3.50),
    SDs = c(0.25, 0.50, 0.66, 0.50, 0.25, 0.25)
)
dtaB <- apply(Bstats, 1, 
    function(stat) {rnorm(100, mean=stat[1], sd=stat[2])} 
)
dtaB <- data.frame(dtaB)
colnames(dtaB) <- c("Verbal", "Numerical", "Spatial", "Creativity", "Intrapersonal", "Interpersonal")

## -----------------------------------------------------------------------------
mycolors <- c("seagreen","chocolate2","mediumpurple3","deeppink","chartreuse4", "darkgoldenrod1")
mylabels <- c("Verbal", "Numerical", "Spatial", "Creativity", "Intrapersonal", "Interpersonal")

## ---- message=FALSE, fig.width=6.7, fig.height=2.5, fig.cap="Figure 2, preliminary version"----
pltA <- superbPlot(dtaA,        # plot for the first data set...
    WSFactors = "Domain(6)",    # ...a within-subject design with 6 levels
    variables = mylabels,       # ...whose variables are contained in the above list
    adjustments = list(
        purpose = "difference", # we want to compare means
        decorrelation = "CM"    # and error bars are correlated-adjusted
    ),
    plotStyle="raincloud",

    # the following (optional) arguments are adjusting some of the visuals
    pointParams    = list(size = 0.75),
    jitterParams   = list(width =0.1, shape=21,size=0.05,alpha=1), # less dispersed jitter dots,
    violinParams   = list(trim=TRUE, alpha=1),                     # not transparent,
    errorbarParams = list(width = 0.1, size=0.5)                   # wider bars, thicker lines.
)
pltA

## ---- message=FALSE, fig.width=6.7, fig.height=2.5, fig.cap="Figure 2, version with colors"----
pltA + aes(fill = factor(Domain), colour = factor(Domain)) 

## -----------------------------------------------------------------------------
commonstyle <- list(
    theme_classic(),                             # It has no background, no bounding box.

    # We customize this theme further:
    theme(axis.line=element_line(size=0.50),     # We make the axes thicker...
        axis.text = element_text(size = 10),     # their text bigger...
        axis.title = element_text(size = 12),    # their labels bigger...
        plot.title = element_text(size = 10),    # and the title bigger as well.
        panel.grid = element_blank(),            # We remove the grid lines
        legend.position = "none"                 # ... and we hide the side legend.
    ),

    # Finally, we place tick marks on the units
    scale_y_continuous( breaks=1:10 ),

    # set the labels to be displayed 
    scale_x_discrete(name="Domain", labels = mylabels),

    # and set colours to both colour and fill layers
    scale_discrete_manual(aesthetic =c("fill","colour"), values = mycolors)
)

## ---- message=FALSE, fig.width=6.7, fig.height=2.5, fig.cap="Figure 2, final version"----
finalpltA <- pltA + aes(fill = factor(Domain), colour = factor(Domain)) + 
    commonstyle +                           # all the above directive are added;
    coord_cartesian( ylim = c(1,10) ) +     # the y-axis bounds are given ;
    labs(title="A") +                       # the plot is labeled "A"...
    ylab("Self-worth relevance")            # and the y-axis label given.
finalpltA

## ---- message=FALSE, fig.width=6.7, fig.height=2.5, fig.cap="Figure 2, bottom row"----
pltB <- superbPlot(dtaB,        # plot for the second data set...
    WSFactors = "Domain(6)",    # ...a within-subject design with 6 levels
    variables = mylabels,       # ...whose variables are contained in the above list
    adjustments = list(
        purpose = "difference", # we want to compare means
        decorrelation = "CM"    # and error bars are correlated-adjusted
    ),
    plotStyle="raincloud",
    # the following (optional) arguments are adjusting some of the visuals
    pointParams    = list(size = 0.75),
    jitterParams = list(width =0.1, shape=21,size=0.05,alpha=1), # less dispersed jitter dots,
    violinParams = list(trim=TRUE, alpha=1,adjust=3),            # not semi-transparent, smoother
    errorbarParams = list(width = 0.1, size=0.5)                 # wider bars, thicker lines.
)
finalpltB <- pltB + aes(fill = factor(Domain), colour = factor(Domain)) + 
    commonstyle +                           # the following three lines are the differences:
    coord_cartesian( ylim = c(1,5) ) +      # the limits, 1 to 5, are different 
    labs(title="B") +                       # the plot is differently-labeled
    ylab("Judgment certainty")              # and the y-axis label differns.
finalpltB

## ---- message=FALSE, fig.width=6.7, fig.height=5.0, fig.cap="Figure 2, final version"----
finalplt <- grid.arrange(finalpltA, finalpltB, ncol=1)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  ggsave( "Figure2.png",
#      plot=finalplt,
#      device = "png",
#      dpi = 320,          # pixels per inche
#      units = "cm",       # or "in" for dimensions in inches
#      width = 17,         # as found in the article
#      height = 13
#  )

