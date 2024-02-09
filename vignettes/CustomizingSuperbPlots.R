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
    errorbarParams = list(width = 0.1, linewidth=0.5)              # wider bars, thicker lines.
)
pltA

## ---- message=FALSE, fig.width=6.7, fig.height=2.5, fig.cap="Figure 2, version with colors"----
pltA + aes(fill = factor(Domain), colour = factor(Domain)) 

## -----------------------------------------------------------------------------
commonstyle <- list(
    theme_classic(),                             # It has no background, no bounding box.

    # We customize this theme further:
    theme(axis.line=element_line(linewidth=0.50), # We make the axes thicker...
        axis.text = element_text(size = 10),      # their text bigger...
        axis.title = element_text(size = 12),     # their labels bigger...
        plot.title = element_text(size = 10),     # and the title bigger as well.
        panel.grid = element_blank(),             # We remove the grid lines
        legend.position = "none"                  # ... and we hide the side legend.
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

## ---- message=FALSE, warning=FALSE, echo=FALSE, eval=TRUE---------------------
# load manually the data for the purpose of the vignette
cleandata <- data.frame(
  subject   = c(201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224),
  absentrt  = c(0.9069648,0.7501645,0.8143321,0.9850208,0.9279098,0.9620722,1.0160006,0.8921083,0.6041074,0.647717,0.6705584,0.9938026,0.8073152,1.079257,0.8648441,0.7923577,0.7683727,0.9004377,0.9590628,0.7619962,0.7245308,0.9070973,0.6244701,0.6991465),
  presentrt = c(0.8805836,0.7227798,0.7173632,0.9084251,0.8596929,0.8488763,0.9039185,0.867465,0.5874631,0.6320984,0.6598097,0.9046643,0.7659111,0.8824536,0.8235161,0.783525,0.6950923,0.8531382,0.8037397,0.674048,0.6987675,0.8272449,0.6298569,0.6853342),
  absentacc = c(0.984375,0.9375,0.953125,0.984375,0.875,0.859375,0.953125,0.953125,0.9375,0.921875,0.953125,0.875,0.96875,0.984375,0.84375,0.921875,0.921875,0.90625,0.953125,1,0.9375,0.984375,0.96875,0.9375),
  presentacc= c(0.984375,0.9921875,0.9765625,0.9921875,0.9375,0.9140625,0.9921875,0.9453125,0.96875,0.9609375,0.9765625,0.9375,0.984375,0.9765625,0.9765625,0.9140625,0.96875,0.9140625,0.9921875,0.9609375,0.9921875,0.9765625,0.9375,0.890625)
)

## -----------------------------------------------------------------------------
cleandata$absentrt = cleandata$absentrt*1000
cleandata$presentrt = cleandata$presentrt*1000

## -----------------------------------------------------------------------------
head(cleandata)

## -----------------------------------------------------------------------------
mycolors = c("black","lightgray")

## -----------------------------------------------------------------------------
library(scales)     # for a translated scale using trans_new()

shift_trans = function(d = 0) {
  scales::trans_new("shift", transform = function(x) x - d, inverse = function(y) y + d)
}

## ---- fig.width=3, fig.height=4, fig.cap="Figure 1, preliminary version"------
# defaults are means with 95% confidence intervals, so not specified
pltA <- superbPlot( cleandata,
    WSFactors = "target(2)",
    variables = c("absentrt", "presentrt"),
    adjustments = list(
        purpose = "difference", 
        decorrelation = "CM"),
    errorbarParams = list(colour = "gray35", width = 0.05)
)
pltA

## ---- fig.width=3, fig.height=4, fig.cap="Figure 2, version with adequate vertical scale"----
# attached the shifted scale to it
pltA <- pltA + scale_y_continuous(
    trans = shift_trans(720),      # use translated bars
    limits = c(720,899),           # limit the plot range
    breaks = seq(720,880,20),      # define major ticks
    expand = c(0,0) )              # no expansions over the plotting area
pltA

## ---- fig.width=3, fig.height=4, fig.cap="Figure 3, version  with theme and details adjusted"----
ornaments <- list(
    theme_classic(base_size = 14) + theme( legend.position = "none" ),
    aes(width = 0.5, fill = factor(target), colour = factor(target) ),
    scale_discrete_manual(aesthetic =c("fill","colour"), values = mycolors),
    scale_x_discrete(name="Color Singleton\nDistractor", labels = c("Absent","Present"))
)
pltA <- pltA + ornaments + ylab("Reaction time (ms)")
pltA

## ---- fig.width=3, fig.height=4, fig.cap="Figure 4, final version for RTs"----
pltA <- pltA + showSignificance( c(1,2), 870, -8, 
    "Singleton presence\nbenefit, p < .001",
    segmentParams = list(linewidth = 1))

# this is it! Check the result   
pltA

## ---- fig.width=3, fig.height=4, fig.cap="Figure 5, final version for mean accuracies"----
pltB <- superbPlot( cleandata,
    WSFactors = "target(2)",
    variables = c("absentacc", "presentacc"),
    adjustments = list(
        purpose = "difference", 
        decorrelation = "CM"),
    errorbarParams = list(colour = "gray35", width = 0.05)
) + 
scale_y_continuous(
    trans = shift_trans(0.9),           # use translated bars
    limits = c(0.9, 1.0),               # limit the plot range
    breaks = seq(0.90, 1.00, 0.01),     # define major ticks
    expand = c(0,0) ) +                 # remove empty space around plotting surface
ornaments + 
ylab("Accuracy (proportion correct)") +
showSignificance( c(1,2), 0.985, -0.005, 
    "Singleton presence\nbenefit, p = .010", 
    segmentParams = list(linewidth = 1) )

# this is it! Check the result   
pltB

## ---- fig.width=6, fig.height=4, fig.cap="Figure 6, final version"------------
finalplt <- grid.arrange(pltA, pltB, ncol=2)
#ggsave( "Figure2b.png",
#    plot=finalplt,
#    device = "png",
#    dpi = 320,          # pixels per inche
#    units = "cm",       # or "in" for dimensions in inches
#    width = 20,         # as found in the article
#    height = 15
#)

## -----------------------------------------------------------------------------
## superb::FYI: The HyunhFeldtEpsilon measure of sphericity per group are  1.000
## superb::FYI: All the groups' data are compound symmetric. Consider using CA.

