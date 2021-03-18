# superb: Summary statistics plotted with correct error bars

The library `superb`  offers two main functions, `superbPlot()` and `GRD()`. 
The purpose of
`superbPlot()` is to provide a plot with summary statistics and correct
error bars. With simple adjustments, the error bar are adjusted
to the design (within or between), to the purpose (single or pair-wise differences),
to the sampling method (simple randomized samples or cluster
randomized samples) and to the population size (infinite or of a specific
 size).

`GRD()` can easily generate random data from any design (within or between) using
any population distribution with any parameters, and with various 
effect sizes. GRD is useful to test statistical procedures such as 
`aov()` or plotting procedures such as `superbPlot()`.

# Installation

```{r, eval=FALSE}
install.packages("superb")
library(superb)
```

# Examples

```{r, fig.cap = "Example of a simple mean plot with 95% confidence interval"}
# This is a simple example illustrating the ToothGrowth of rats
# as a function of the dose of vitamin and the form of the vitamin (pills or juice)
superbPlot(ToothGrowth, 
    BSFactor = c("dose","supp"), 
    variables = "len" )
```


```{r, fig.cap ="Example of a median plot with 95% confidence interval of the median"}
# This explicitely indicates to display the 
# median instead of the default mean statistics
superbPlot(ToothGrowth, 
    BSFactor = c("dose","supp"), 
    variables = "len",
    statistic = "median")
```

```{r}
# This example generates scores for 3000 simulated participants in 
# a 3 x 2 design with repeated-measures on days.
# The factor day is belived to improve the scores (reducing it)
testdata <- GRD(
    RenameDV   = "score", 
    SubjectsPerGroup = 1000, 
    BSFactors  = "Difficulty(3)", 
    WSFactors  = "Day(2)",
    Population = list(mean = 75,stddev = 12,rho = 0.5),
    Effects    = list("Day" = slope(-3) )
)
head(testdata)

superbPlot(testdata, 
    BSFactor  = "Difficulty", 
    WSFactor  = "Day(2)",
    variables = c("score.1","score.2"),
    plotStyle = "line"
)
```

# For more

Consult the documentation, of the vignettes.

A general introduction to the `superb` framework is under consideration
at *Advances in Methods and Practices in Psychological Sciences*.


