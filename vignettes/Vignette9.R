## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)

## -----------------------------------------------------------------------------
library(sadists) # for computing confidence intervals of Cohen's d
library(superb)

d1 <- function(X) {
        # the global variable GM.d1 is obtained from the initializer
        mean(X - GM.d1) / sd(X) 
    } 

CI.d1 <- function(X, gamma = .95) {
    n    <- length(X)
    dlow <- qlambdap(1/2-gamma/2, df = n-1, t = d1(X) * sqrt(n) ) 
    dhig <- qlambdap(1/2+gamma/2, df = n-1, t = d1(X) * sqrt(n) ) 
    c(dlow, dhig) / sqrt(n)
}

## -----------------------------------------------------------------------------
# let's generate random data with true Cohen's dp 
# of 0.12 (groups 1 and 2) and 0.24 (groups 1 and 3)
dta <- GRD( BSFactors = "Dose(3)", 
    RenameDV          = "score",
    Effects           = list("Dose" = custom(0, 1.2, 2.4)), 
    SubjectsPerGroup  = 500, 
    Population        = list( mean = 50, stddev = 10)
)

## -----------------------------------------------------------------------------
# the exact formulas for Cohen's d1 and dp. Only d1 is used in the plot
init.d1 <- function(df) { 
        GM.d1 <<- mean(df$DV) # will make d1 relative to the grand mean
}

## ---- message=TRUE, echo=TRUE, fig.width = 4, fig.cap="**Figure 1**. d_1 scores along with 95% confidence interval."----
# show a plot with Cohen's d1 and difference-adjusted confidence intervals of d1
superbPlot(dta, 
        BSFactors   = "Dose", variables = "score", 
        statistic   = "d1",  errorbar  = "CI", 
        gamma       = 0.95, 
        plotStyle   = "line",
        adjustments = list(purpose="difference")
) + theme_light(base_size = 10) + 
coord_cartesian( ylim = c(-0.3,+0.45) ) +
labs(title = "d_1 with difference-adjusted 95% confidence intervals of d_1",
     y     = "d_1 relative to grand mean") 

## -----------------------------------------------------------------------------
superb:::has.init.function("d1")

## -----------------------------------------------------------------------------
dp <- function(X, Y) {
        mean(X - Y) / sqrt((length(X)*var(X) + length(Y)*var(Y))/(length(X)+length(Y)-2))
    } 
CI.dp <- function(X, Y, gamma = .95) {
    n1 = length(X)
    n2 = length(Y)
    n = hmean(c(n1, n2))
    dlow <- qlambdap(1/2-gamma/2, df = n1+n2-2, t = dp(X, Y) * sqrt(n/2) ) 
    dhig <- qlambdap(1/2+gamma/2, df = n1+n2-2, t = dp(X, Y) * sqrt(n/2) ) 
    c(dlow, dhig) / sqrt(n/2)
}

## -----------------------------------------------------------------------------
grp1 <- dta$score[dta$Dose==1]
grp2 <- dta$score[dta$Dose==2]
grp3 <- dta$score[dta$Dose==3]

## -----------------------------------------------------------------------------
dp12 <- round(dp(grp2, grp1), 3)
dp13 <- round(dp(grp3, grp1), 3)
dp23 <- round(dp(grp3, grp2), 3)
c(dp12, dp13, dp23)

## -----------------------------------------------------------------------------
cidp12 = round(CI.dp(grp2, grp1, 0.95), 3)
cidp13 = round(CI.dp(grp3, grp1, 0.95 ), 3)
cidp23 = round(CI.dp(grp3, grp2, 0.95 ), 3)
c(cidp12,cidp13,cidp23)

## ---- message=FALSE, echo=TRUE, fig.width=4, fig.cap="**Figure 2**. d_1 scores along with 95% confidence interval."----
superbPlot(dta, BSFactors  = "Dose", variables = "score", 
        statistic = "d1",  errorbar  = "CI", gamma     = 0.95, 
        plotStyle = "line",
        adjustments = list(purpose="difference")
) + theme_light(base_size = 10) +
coord_cartesian( ylim = c(-0.3,+0.45) ) +
labs(title   = "d_1 with difference-adjusted 95% confidence intervals of d_1",
     caption = paste("Note: Cohen's d_p and its confidence interval computed with the \n",
                     "true formula (Cousineau & Goulet-Pelletier, 2020)"),      
     y       = "d_1 relative to grand mean") +  
showSignificance(c(1,2), 0.3, -0.01,
    paste("dp = ",dp12, ifelse(sign(cidp12[1])==sign(cidp12[2]),", p < .05",", p > .05")) ) + 
showSignificance(c(1,3), 0.4, -0.01,
    paste("dp = ",dp13, ifelse(sign(cidp13[1])==sign(cidp13[2]),", p < .05",", p > .05"))) + 
showSignificance(c(2,3), -0.25, +0.01,
    paste("dp = ",dp23, ifelse(sign(cidp23[1])==sign(cidp23[2]),", p < .05",", p > .05"))) 

## -----------------------------------------------------------------------------
compareCIlength <- function(g1, g2) {
    # compute the Cohen's dp confidence interval length
    cilength.dp = round(CI.dp(g1, g2)[2]-CI.dp(g1, g2)[1], 3)

    # compute two d1 CI length, difference-adjusted
    len1    = sqrt(2)*(CI.d1(grp1)[2] - CI.d1(grp1)[1])
    len2    = sqrt(2)*(CI.d1(grp2)[2] - CI.d1(grp2)[1])
    # average in the square sense the two d1 CI lengths
    cilength.d1 = round(sqrt((len1^2 + len2^2)/2), 3)

    data.frame( dp.length = cilength.dp, d1.average.length = cilength.d1)
}

compareCIlength(grp1,grp2)
compareCIlength(grp1,grp3)
compareCIlength(grp2,grp3)

