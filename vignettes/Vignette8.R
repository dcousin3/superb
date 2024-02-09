## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
options("superb.feedback" = c("warnings","design"))

library(ggplot2)
dta = GRD(WSFactors="Moments(3)", SubjectsPerGroup =20, 
    Population = list(mean = 12, stddev = 1, rho = 0.45),
    Effects = list("Moments" = custom(2,3,5) ),
    RenameDV = "Score"
)


## -----------------------------------------------------------------------------
pCM <- superbPlot(dta, WSFactors = "moment(3)",     
  variables = c("Score.1","Score.2","Score.3"), 
  adjustments=list(decorrelation="none"),
  preprocessfct = "subjectCenteringTransform",
  postprocessfct = "biasCorrectionTransform",
  plotStyle = "pointjitter",
  errorbarParams = list(color="red", width= 0.1, position = position_nudge(-0.05) )
)

## -----------------------------------------------------------------------------
pLM <- superbPlot(dta, WSFactors = "moment(3)", 
  variables = c("Score.1","Score.2","Score.3"), 
  adjustments=list(decorrelation="none"),
  preprocessfct = "subjectCenteringTransform",
  postprocessfct = c("biasCorrectionTransform","poolSDTransform"),
  plotStyle = "line",
  errorbarParams = list(color="orange", width= 0.1, position = position_nudge(-0.0) )
)

## -----------------------------------------------------------------------------
pNKM <- superbPlot(dta, WSFactors = "moment(3)", 
  variables = c("Score.1","Score.2","Score.3"), 
  adjustments=list(decorrelation="none"),
  preprocessfct = "subjectCenteringTransform",
  postprocessfct = c("poolSDTransform"),
  plotStyle = "line",
  errorbarParams = list(color="blue", width= 0.1, position = position_nudge(+0.05) )
)

## ---- fig.height=4, fig.width=7, fig.cap = "**Figure 1**. Plot of the tree decorrelation methods based on subject transformation."----
tlbl <- paste( "(red)    Subject centering & Bias correction == CM\n",
               "(orange) Subject centering, Bias correction & Pooling SDs == LM\n",
               "(blue)   Subject centering & Pooling SDs == NKM", sep="")

ornate <- list(
    xlab("Group"),
    ylab("Score"),
    labs(   title=tlbl),
    coord_cartesian( ylim = c(12,18) ),
    theme_light(base_size=10)
)

# the plots on top are made transparent
pCM2 <- ggplotGrob(pCM + ornate)
pLM2 <- ggplotGrob(pLM + ornate + makeTransparent() )
pNKM2 <- ggplotGrob(pNKM + ornate + makeTransparent() )

# put the grobs onto an empty ggplot 
ggplot() + 
    annotation_custom(grob=pCM2) + 
    annotation_custom(grob=pLM2) + 
    annotation_custom(grob=pNKM2)

