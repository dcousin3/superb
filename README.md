# superb: Summary statistics plotted with correct error bars

superb offers two functions, superbPlot and GRD. The purpose of
superbPlot is to provide a plot with summary statistics and correct
error bars. With simple adjustments, the error bar are adjusted
to the design (within or between), to the purpose (single or difference),
to the sampling method (simple randomized samples or cluster
randomized samples) and to the population size (infinite or a specific
total size).

GRD can easily generate random data from any design (within or between) using
any population distribution with any parameters, and with various 
effect sizes. GRD is useful to test statistical procedures such as 
aov or plotting procedures such as superbPlot.

# Example

`
# This simple example
superbPlot(ToothGrowth, 
    BSFactor = c("dose","supp"), 
    variables = "len" )
`


`# This explicitely indicates to display the 
# median instead of the default mean statistics
superbPlot(ToothGrowth, 
    BSFactor = c("dose","supp"), 
    variables = "len",
    statistic = "median")
`

# Installation

`install.packages("superb")`

# For more

Consult the documentation, of the vignettes.

A general introduction to the `superb` framework is under consideration
at *Advanced in Methods and Practices in Psychological Sciences*.


