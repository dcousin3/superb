
# superb: Summary statistics plotted with correct error bars

<img src="logo.png" align="right" alt="" width="120" />

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

The `superbData()` function does not generate the plot but returns the
summary statistics and the interval boundaries. These can afterwards be
output to other plotting environment.

`GRD()` is used to easily generate random data from any design (within
or between) using any population distribution with any parameters, and
with various effect sizes. GRD is useful to test statistical procedures
such as `aov()` or plotting procedures such as `superbPlot()`.

# Installation

The official **CRAN** version can be installed with

``` r
install.packages("superb")
library(superb)
```

The development version can be accessed through GitHub:

``` r
devtools::install_github("dcousin3/superb")
library(superb)
```

# Examples

This is a simple example illustrating the ToothGrowth (dependent
variable is `len`) of rats as a function of the `dose` of vitamin and
the form of the vitamin `supp` (pills or juice)

``` r
superbPlot(ToothGrowth, 
    BSFactor = c("dose","supp"), 
    variables = "len" )
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

In the above, the defautl summary statistic, the mean, is used. The
error bars are, by default, the 95% confidence intervals. These two
choices can be changed with the `statistic` and the `errorbar`
arguments.

This second example explicitely indicates to display the `median`
instead of the default `mean` summary statistics

``` r
superbPlot(ToothGrowth, 
    BSFactor = c("dose","supp"), 
    variables = "len",
    statistic = "median")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

As a third example, we illustrate the harmonic means `hmedian` along
with 99.9% confidence intervals using lines:

``` r
superbPlot(ToothGrowth, 
    BSFactor = c("dose","supp"), 
    variables = "len",
    statistic = "hmean", 
    errorbar = "CI", gamma = 0.999,
    plotStyle = "line")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The second function, `GRD`, can be used to generate random data from
designs with various within- and between-subject factors. This example
generates scores for 300 simulated participants in a 3 x 2 design with
repeated-measures on `Day`s. Only the factor `Day` is simulated to
improve the scores by reducing it:

``` r
testdata <- GRD(
    RenameDV   = "score", 
    SubjectsPerGroup = 100, 
    BSFactors  = "Difficulty(3)", 
    WSFactors  = "Day(2)",
    Population = list(mean = 75,stddev = 12,rho = 0.5),
    Effects    = list("Day" = slope(-3) )
)
head(testdata)
```

    ##   id Difficulty  score.1  score.2
    ## 1  1          1 77.41706 83.21001
    ## 2  2          1 72.63189 82.72655
    ## 3  3          1 86.05087 71.07737
    ## 4  4          1 63.68760 71.25382
    ## 5  5          1 78.19190 87.32919
    ## 6  6          1 82.03752 84.70999

The simulated scores are illustrated using jitter dots as well as a
violin plot to show the distributions:

``` r
superbPlot(testdata, 
    BSFactor  = "Difficulty", 
    WSFactor  = "Day(2)",
    variables = c("score.1","score.2"),
    plotStyle = "pointjitterviolin"
)
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

As seen, `superb` can be used to illustrate summary statistics but also
some characteristics of the raw data.

# For more

The complete documentation is available on this
[site](https://dcousin3.github.io/superb).

A general introduction to the `superb` framework underlying this library
is under consideration at *Advances in Methods and Practices in
Psychological Sciences*.
