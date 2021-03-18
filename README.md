
# superb: Summary statistics plotted with correct error bars

<img src="docs/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/superb)](https://cran.r-project.org/package=superb)
<!-- badges: end -->

The library `superb` offers two main functions, `superbPlot()` and
`GRD()`. The purpose of `superbPlot()` is to provide a plot with summary
statistics and correct error bars. With simple adjustments, the error
bar are adjusted to the design (within or between), to the purpose
(single or pair-wise differences), to the sampling method (simple
randomized samples or cluster randomized samples) and to the population
size (infinite or of a specific size).

`GRD()` can easily generate random data from any design (within or
between) using any population distribution with any parameters, and with
various effect sizes. GRD is useful to test statistical procedures such
as `aov()` or plotting procedures such as `superbPlot()`.

# Installation

``` r
install.packages("superb")
```

    ## Installing package into 'C:/Users/DenisCousineau/AppData/Local/Temp/RtmpkLpGCy/temp_libpath17f7c2f63449d'
    ## (as 'lib' is unspecified)

    ## Warning: package 'superb' is not available for this version of R
    ## 
    ## A version of this package for your version of R might be available elsewhere,
    ## see the ideas at
    ## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

``` r
library(superb)
```

# Examples

This is a simple example illustrating the ToothGrowth of rats as a
function of the dose of vitamin and the form of the vitamin (pills or
juice)

``` r
superbPlot(ToothGrowth, 
    BSFactor = c("dose","supp"), 
    variables = "len" )
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

This explicitely indicates to display the median instead of the default
mean statistics

``` r
superbPlot(ToothGrowth, 
    BSFactor = c("dose","supp"), 
    variables = "len",
    statistic = "median")
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This example generates scores for 3000 simulated participants in a 3 x 2
design with repeated-measures on days. The factor day is belived to
improve the scores (reducing it)

``` r
testdata <- GRD(
    RenameDV   = "score", 
    SubjectsPerGroup = 1000, 
    BSFactors  = "Difficulty(3)", 
    WSFactors  = "Day(2)",
    Population = list(mean = 75,stddev = 12,rho = 0.5),
    Effects    = list("Day" = slope(-3) )
)
head(testdata)
```

    ##   id Difficulty  score.1  score.2
    ## 1  1          1 44.90375 61.27902
    ## 2  2          1 70.66386 56.72789
    ## 3  3          1 92.24386 92.96150
    ## 4  4          1 73.87552 79.91657
    ## 5  5          1 75.95869 54.88111
    ## 6  6          1 88.30228 81.54658

``` r
superbPlot(testdata, 
    BSFactor  = "Difficulty", 
    WSFactor  = "Day(2)",
    variables = c("score.1","score.2"),
    plotStyle = "line"
)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# For more

Consult the documentation, of the vignettes.

A general introduction to the `superb` framework is under consideration
at *Advances in Methods and Practices in Psychological Sciences*.
