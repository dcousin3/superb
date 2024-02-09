## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)
options(superb.feedback = c('design','warnings') )

## -----------------------------------------------------------------------------
# enter the compiled data into a data frame:
compileddata <- data.frame(cbind(
    s = c(10, 18, 10),
    n = c(30, 28, 26)
))

## -----------------------------------------------------------------------------
group  <- c()
scores <- c()
for (i in 1: (dim(compileddata)[1])) {
        group  <- c( group, rep(i, compileddata$n[i] ) )
        scores <- c( scores, rep(1, compileddata$s[i]), 
                    rep(0, compileddata$n[i] - compileddata$s[i]) )
    }
dta  <- data.frame( cbind(group = group, scores = scores ) )

## -----------------------------------------------------------------------------
# the Anscombe transformation for a vector of binary data 0|1
A <-function(v) {
    x <- sum(v)
    n <- length(v)
    asin(sqrt( (x+3/8) / (n+3/4) ))
}   
SE.A <- function(v) {
    0.5 / sqrt(length(v+1/2))
}
CI.A <- function(v, gamma = 0.95){
    SE.A(v) * sqrt(qchisq(gamma, df=1))
}

## ---- echo = TRUE, message = FALSE--------------------------------------------
library(superb)
library(ggplot2)
library(scales)     # for asn_trans() non-linear scale

## ---- message=FALSE, echo=TRUE, fig.width = 3, fig.cap="**Figure 1**. Anscombe-transformed scores as a function of group."----
# ornate to decorate the plot a little bit...
ornate = list( 
    theme_bw(base_size = 10),
    labs(x = "Group" ),
    scale_x_discrete(labels=c("Group A", "Group B", "Group C"))
)
superbPlot(dta, 
    BSFactors = "group",
    variables = "scores",
    statistic = "A", 
    error     = "CI",
    adjustment = list( purpose = "difference"),
    plotStyle = "line",
    errorbarParams = list(color="blue") # just for the pleasure!
) + ornate + labs(y = "Anscombe-transformed scores" )

## -----------------------------------------------------------------------------
# the proportion of success for a vector of binary data 0|1
prop <- function(v){
    x <- sum(v)
    n <- length(v)
    x/n
}
# the de-transformed confidence intervals from Anscombe-transformed scores
CI.prop <- function(v, gamma = 0.95) {
    y     <- A(v)
    n     <- length(v)
    cilen <- CI.A(v, gamma)
    ylo   <- y - cilen
    yhi   <- y + cilen
    # reverse arc-sin transformation: naive approach
    cilenlo <- ( sin(ylo)^2 )
    cilenhi <- ( sin(yhi)^2 )

    c(cilenlo, cilenhi)
}

## ---- message=FALSE, echo=TRUE, fig.width = 3, fig.cap="**Figure 2**. Proportion as a function of group."----
superbPlot(dta, 
    BSFactors = "group",
    variables = "scores",
    statistic = "prop", 
    error     = "CI",
    adjustment = list( purpose = "difference"),
    plotStyle = "line",
    errorbarParams = list(color="blue")
) + ornate + labs(y = "Proportions" ) + 
    scale_y_continuous(trans=asn_trans())

