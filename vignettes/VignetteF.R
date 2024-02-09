## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide', warning = FALSE----
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)
options(superb.feedback = 'none')

## ---- eval=TRUE---------------------------------------------------------------
dta <- data.frame(
    vocation = factor(unlist(lapply(c("Secondary","Vocational","Teacher","Gymnasium","University"), function(p) rep(p,2))), 
                    levels = c("Secondary","Vocational","Teacher","Gymnasium","University")),
    gender  = factor(rep(c("Boys","Girls"),5), levels=c("Boys","Girls")),
    obsfreq   = c(62,61,121,149,26,41,33,20,84,20)
)

## ---- eval=TRUE---------------------------------------------------------------
dta

## ---- eval=TRUE, fig.width = 6, eval=TRUE, fig.cap="**Figure 1: A quick-and-dirty plot**"----
library(superb)
library(ggplot2)

plt1 <- superbPlot(
    dta,
    BSFactors = c("vocation","gender"),
    variables = "obsfreq",                      # name of the column with the counts
    statistic = "identity",                     # the raw data as is
    errorbar  = "none",                         # no error bars
    # the following is for the look of the plot
    plotStyle      = "line",                    # style of the plot
    lineParams     = list( size = 1.0)          # thicker lines as well
)
plt1

## ---- eval=TRUE---------------------------------------------------------------
count <- function(x) x[1]

## ---- eval=TRUE---------------------------------------------------------------
init.count <- function(df) {
    totalcount <<- sum(df$DV)
}

## ---- eval=TRUE---------------------------------------------------------------
CI.count <- function(n, gamma=0.95) {
    N <- totalcount    

    # Clopper & Pearson CI from Leemis & Trivedi, 1996
    plow <- (1+(N-n+1)/((n+0)*qf(1/2-gamma/2,2*(n+0),2*(N-n+1))))^(-1)
    phig <- (1+(N-n+0)/((n+1)*qf(1/2+gamma/2,2*(n+1),2*(N-n+0))))^(-1)

    # convert to CI on counts
    nlow <- totalcount * plow
    nhig <- totalcount * phig

    # increase width for difference- and correlation-adjustments
    2 * c( nlow[1]-n[1], nhig[1]-n[1] ) + n[1]
}

## ---- eval=TRUE, fig.width = 6, eval=TRUE, fig.cap="**Figure 2: A complete plot**"----
plt2 <- superbPlot(
    dta,
    BSFactors = c("vocation","gender"),
    variables = "obsfreq",
    statistic = "count",                                 # the function defined above
    errorbar  = "CI",                                    # its CI define above
    # the following is for the look of the plot
    plotStyle      = "line",                             # style of the plot
    errorbarParams = list( width =0.5, linewidth =0.75 ),# have thicker error bars
    lineParams     = list( size = 1.0)                   # thicker lines as well
)
plt2

## ---- eval=TRUE, fig.width = 6, eval=TRUE, fig.cap="**Figure 3: An ornated plot**"----
ornate <- list(
    xlab("Educational vocation"),                      # label on the x-axis
    ylab("Observed frequency"),                        # label on the y-axis
    theme_bw(base_size = 16),                          # black and white theme
    scale_x_discrete(guide = guide_axis(n.dodge = 2))  # unalign labels
    # etc. anything accepted by ggplots can be added.
)
plt3 <- plt2 + ornate
plt3

