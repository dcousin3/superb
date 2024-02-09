## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)

## -----------------------------------------------------------------------------
grp1 <- c( 56,  54,  73,  46,  59,  62,  55,  53,  77,  60,  69,  66,  63, 
           62,  53,  82,  74,  70,  65,  70,  72,  65,  56,  58,  83)
grp2 <- c( 51,  99, 194, 123,  40,  83,  87, 117,  46,  89,  61,  81,  53, 
          141,  52,  53,  39,  96,  14,  81,  63,  66,  80, 113,  82)

## -----------------------------------------------------------------------------
dtaHetero <- data.frame( cbind(
    id    = 1:(length(grp1)+length(grp2)),
    group = c(rep(1,length(grp1)), rep(2, length(grp2)) ),
    score = c(grp1, grp2)
))
head(dtaHetero)

## -----------------------------------------------------------------------------
library(superb)             # to make the summary plot
library(ggplot2)            # for all the graphic directives
library(gridExtra)          # for grid.arrange

ornate = list(
    xlab("Difference"), 
    scale_x_discrete(labels=c("Pre-\nTreatment","Post-\nTreatment")),
    ylab("Score"),
    coord_cartesian( ylim = c(40,+110) ),
    theme_light(base_size = 10) +
    theme( plot.subtitle = element_text(size=16))
)

## ---- fig.height=4, fig.width=3, fig.cap = "**Figure 1**. Plot of dtaHerero showing heterogeneous error bars."----
pt <- superbPlot(dtaHetero, 
                 BSFactors    = "group", 
                 variables   = "score", 
                 adjustments = list(purpose = "difference"),
                 gamma       = 0.95,
                 statistic   = "mean", 
                 errorbar    = "CI",
                 plotStyle   = "line",
                 lineParams  = list(alpha = 0) #the line is made transparent
) + ornate
pt

## -----------------------------------------------------------------------------
# Welch's rectified degrees of freedom
wdf <- WelchDegreeOfFreedom(dtaHetero, "score","group")

## ---- fig.height=4, fig.width=3, fig.cap = "**Figure 2**. Plot of dtaHerero with rectified degree of freedom."----
pw <- superbPlot(dtaHetero, 
                 BSFactors   = "group", 
                 variables   = "score", 
                 adjustments = list(purpose = "difference"),
                 gamma       = c(0.95, wdf),         # new! 
                 statistic   = "mean", 
                 errorbar    = "CIwithDF",           # new!
                 plotStyle   = "halfwidthline",
                 lineParams  = list(alpha = 0)
) + ornate
pw

## ---- fig.height=4, fig.width=3, fig.cap = "**Figure 3**. Plot of dtaHerero with rectified degree of freedom and Tryon' difference-adjusted error bars."----
pwt <- superbPlot(dtaHetero, 
                  BSFactors   = "group", 
                  variables   = "score", 
                  adjustments = list(purpose = "tryon"), #new!
                  gamma       = c(0.95, wdf),
                  statistic   = "mean", 
                  errorbar    = "CIwithDF",
                  plotStyle   = "halfwidthline",
                  lineParams  = list(alpha = 0)
)+ ornate
pwt

## -----------------------------------------------------------------------------
# get the summary statistics with superbData
t <- superbData(dtaHetero, 
                BSFactors   = "group", 
                variables   = "score", 
                adjustments = list(purpose = "difference"),
                gamma       = 0.95,
                statistic   = "mean", errorbar = "CI"
)
# keep only the summary statistics:
t2 <- t$summaryStatistics

# the length is in column "upperwidth", for lines 1 and 2,
# so lets do the mean in the square sense:
tmean2 <- sqrt( (t2$upperwidth[1]^2 + t2$upperwidth[2]^2)/2 )

## -----------------------------------------------------------------------------
tmean    <- (t2$upperwidth[1] + t2$upperwidth[2] )/2

## -----------------------------------------------------------------------------
# get the summary statistics with superbData
wt <- superbData(dtaHetero, 
                BSFactors   = "group", 
                variables   = "score", 
                adjustments = list(purpose = "tryon"),
                gamma       = c(0.95, wdf),
                statistic   = "mean", errorbar = "CIwithDF"
)
wt2 <- wt$summaryStatistics
wmean <- (wt2$upperwidth[1]+wt2$upperwidth[2]) / 2

## ----fig.height=4, fig.width=9, fig.cap = "**Figure 4**. All three plots with relevant markers in red."----
# showing all three plots, with reference lines in red
grid.arrange(
    pt + labs(subtitle="Difference-adjusted 95% CI\n with default degree of freedom") + 
        geom_text( x = 1.15, y = mean(grp1)+(mean(grp2)-mean(grp1))/2, label = "power-2 mean", angle = 90) +
        geom_text( x = 1.55, y = mean(grp1)+(mean(grp2)-mean(grp1))/2, label = "regular mean", angle = 90) +
        geom_hline(yintercept = mean(grp1)+(mean(grp2)-mean(grp1))/2-tmean2/2, colour = "red", linewidth = 0.5, linetype=2) + 
        geom_hline(yintercept = mean(grp1)+(mean(grp2)-mean(grp1))/2+tmean2/2, colour = "red", linewidth = 0.5, linetype=2) + 
        # arrow for the power-2 mean
        geom_segment(arrow = arrow(length =unit(0.4,"cm")), x=1.33, y=mean(grp1)+(mean(grp2)-mean(grp1))/2, 
                xend=1.33, yend=mean(grp1)+(mean(grp2)-mean(grp1))/2+tmean2/2) +
        geom_segment(arrow = arrow(length =unit(0.4,"cm")), x=1.333, y=mean(grp1)+(mean(grp2)-mean(grp1))/2, 
                xend=1.33, yend=mean(grp1)+(mean(grp2)-mean(grp1))/2-tmean2/2) + 
        # arrow for the regular mean
        geom_segment(arrow = arrow(length =unit(0.4,"cm")), x=1.66, y=mean(grp1)+(mean(grp2)-mean(grp1))/2, 
                xend=1.66, yend=mean(grp1)+(mean(grp2)-mean(grp1))/2+tmean/2) +
        geom_segment(arrow = arrow(length =unit(0.4,"cm")), x=1.66, y=mean(grp1)+(mean(grp2)-mean(grp1))/2, 
                xend=1.66, yend=mean(grp1)+(mean(grp2)-mean(grp1))/2-tmean/2), 
    pw  + labs(subtitle="Difference-adjusted 95% CI\nwith df from Welch"),

    pwt + labs(subtitle="Tryon-adjusted 95% CI\nwith df from Welch") +
        geom_hline(yintercept = mean(grp1)+wt2$upperwidth[1]/2, colour = "red", linewidth = 0.5, linetype=2) + 
        geom_hline(yintercept = mean(grp2)+wt2$lowerwidth[2]/2, colour = "red", linewidth = 0.5, linetype=2),
 ncol=3)

## -----------------------------------------------------------------------------
t.test( grp1, grp2, 
        var.equal=FALSE)

