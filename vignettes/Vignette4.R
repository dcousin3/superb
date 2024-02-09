## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.")
library(superb)
library(ggplot2)

## ---- message=FALSE, echo=TRUE, fig.height=4, fig.width=6, fig.cap="**Figure 1**. Various statistics and various measures of precisions"----
# shut down 'warnings', 'design' and 'summary' messages
options(superb.feedback = 'none') 

# Generate a random dataset from a (3 x 2) design, entirely within subject.
# The sample size is very small (n=5) and the correlation between scores is high (rho = .8)
dta <- GRD( 
    WSFactors  = "Moment(3): Dose(2)", 
    Effects    = list("Dose*Moment"=custom(0,0,0,1,1,3)), 
    SubjectsPerGroup = 50, 
    Population = list( mean=10, stddev = 5, rho = .80)
)

# a quick function to call superbPlot
makeplot <- function(statfct, errorbarfct, gam, rg, subttl) {
    superbPlot(dta, 
        WSFactors  = c("Moment(3)","Dose(2)"), 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = statfct, 
        errorbar  = errorbarfct, 
        gamma     = gam, 
        plotStyle = "line",
        adjustments = list(purpose="difference", decorrelation="CM")
    ) + ylab(subttl) + coord_cartesian( ylim = rg )
} 

p1 <- makeplot("mean",      "CI", .95, c(6,14), "Mean +- 95% CI of the mean")
p2 <- makeplot("mean",      "SE", .00, c(6,14), "Mean +- SE of the mean")
p3 <- makeplot("median",    "CI", .95, c(6,14), "Median +- 95% CI of the median")
p4 <- makeplot("fisherskew","CI", .95, c(-2,+2), "Fisher skew +- 95% CI")

library(gridExtra)
p <- grid.arrange(p1,p2,p3,p4, ncol=2)

## -----------------------------------------------------------------------------
superb:::is.stat.function("mean")

## -----------------------------------------------------------------------------
superb:::is.errorbar.function("SE.mean")

## -----------------------------------------------------------------------------
superb:::is.gamma.required("SE.mean")

## ---- message=FALSE, echo=TRUE------------------------------------------------
    # create a descriptive statistics, the 20% trimmed mean
    trimmedmean    <- function(x) mean(x, trim = 0.2)

    # we can test it with the data from group 1...
    grp1 <- dataFigure1$score[dataFigure1$grp==1]
    grp2 <- dataFigure1$score[dataFigure1$grp==2]
    trimmedmean(grp1)

    # or check that it is a valid statistic function
    superb:::is.stat.function("trimmedmean")

## ---- message=FALSE, echo=TRUE, fig.height=4, fig.width=3, fig.cap="**Figure 2**. ``superbPlot`` with a custom-made descriptive statistic function "----
    superbPlot(dataFigure1, 
        BSFactors = "grp", 
        statistic = "trimmedmean", errorbar = "none", #HERE the statistic name is given
        plotStyle="line",
        adjustments = list(purpose = "difference"),
        variable = "score",
        errorbarParams = list(width=0) # so that the null-width error bar is invisible
    )+ ylab("20% trimmed mean") +
    theme_gray(base_size=10) +
    labs(title="20% trimmed mean with \nno error bars") +
    coord_cartesian( ylim = c(85,115) ) 

## ---- message=FALSE-----------------------------------------------------------
    library(psych)      # for winsor.sd

    CI.trimmedmean <- function(x, gamma = 0.95){
        trim <- 0.2
        g    <- floor(length(x) * 0.4)
        tc   <- qt(1/2+gamma/2, df=(length(x)-g-1) )
        lo   <- tc * winsor.sd(x, trim =0.2) / ((1-2*trim)*sqrt(length(x)))
        c(trimmedmean(x) -lo, trimmedmean(x)+lo)
    }

    # we test as an example the data from group 1
    CI.trimmedmean(grp1)  

    # or check that it is a valid interval function
    superb:::is.errorbar.function("CI.trimmedmean")

## ---- message=FALSE, echo=TRUE, fig.height=4, fig.width=3, fig.cap="**Figure 3**. `superbPlot` with a custom-made descriptive sttistic function "----
    superbPlot(dataFigure1, 
        BSFactors = "grp", 
        statistic = "trimmedmean", errorbar = "CI",
        plotStyle="line",
        adjustments = list(purpose = "difference"),
        variable = "score"
    )+ ylab("20% trimmed mean") +
    theme_gray(base_size=10) +
    labs(title="20% trimmed mean with \n95% confidence interval of 20% trimmed mean") +
    coord_cartesian( ylim = c(85,115) ) 

## -----------------------------------------------------------------------------
    # we define myBootstrapPI which subsample the whole sample, here called X
    myBootstrapPI.mean <- function(X, gamma = 0.95) {
      res = c()
      for (i in 1:10000) {
        res[i] <- mean(sample(X, length(X), replace = T))
      }
      quantile(res, c(1/2 - gamma/2, 1/2 + gamma/2))
    }

    # we check that it is a valid interval function
    superb:::is.errorbar.function("myBootstrapPI.mean")

## ---- message=FALSE, echo=TRUE,  fig.height=4, fig.width=6, fig.cap="**Figure 4**. `superbPlot` with a custom-made interval function."----
    plt1 <- superbPlot(dataFigure1, 
        BSFactors = "grp", 
        variable = c("score"), 
        plotStyle="line",
        statistic = "mean", errorbar = "CI",
        adjustments = list(purpose = "difference")
    ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="means and difference-adjusted\n95% confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))

    plt2 <- superbPlot(dataFigure1, 
        BSFactors = "grp", 
        variable = c("score"), 
        plotStyle="line",
        statistic = "mean", errorbar = "myBootstrapPI",
        adjustments = list(purpose = "difference")
    ) + 
    xlab("Group") + ylab("Score") + 
    labs(title="means and difference-adjusted\n95% bootstrap confidence intervals") +
    coord_cartesian( ylim = c(85,115) ) +
    theme_gray(base_size=10) +
    scale_x_discrete(labels=c("1" = "Collaborative games", "2" = "Unstructured activity"))

    library(gridExtra)
    plt <- grid.arrange(plt1, plt2, ncol=2)

