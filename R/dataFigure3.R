#' Data for Figure 3
#'
#' The data, inspired from \insertCite{cl16}{superb}, is an example where the
#' "stand-alone" 95% confidence interval of the means returns
#' a result in contradiction with the result of a statistical test.
#' The paradoxical result is resolved by using adjusted confidence intervals,
#' here the cluster- and different-adjusted confidence interval.
#'
#' @md
#'
#' @docType data
#'
#' @usage data(dataFigure3)
#'
#' @format An object of class data.frame.
#'
#' @keywords datasets
#'
#' @references 
#' \insertAllCited{}
#'
#' @source \doi{10.5709/acp-0214-z}
#'
#' @examples
#' library(ggplot2)
#' library(gridExtra)
#' data(dataFigure3)
#' 
#' options(superb.feedback = 'none') # shut down 'warnings' and 'design' interpretation messages
#' 
#' ## realize the plot with unadjusted (left) and ajusted (right) 95% confidence intervals
#' plt3a <- superbPlot(dataFigure3, BSFactors = "grp", 
#'     adjustments=list(purpose = "difference", samplingDesign = "SRS"), 
#'     variables = c("VD"), plotStyle="bar" ) + 
#'   xlab("Group") + ylab("Score") + labs(title="Difference-adjusted 95% CI\n") +
#'   coord_cartesian( ylim = c(85,115) ) +
#'   geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2)
#' plt3b <- superbPlot(dataFigure3, BSFactors = "grp", 
#'     adjustments=list(purpose = "difference", samplingDesign = "CRS"), 
#'     variables = c("VD"), plotStyle="bar", clusterColumn = "cluster" ) + 
#'   xlab("Group") + ylab("Score") + labs(title="Cluster and difference-adjusted\n95% CI") +
#'   coord_cartesian( ylim = c(85,115) ) + 
#'   geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2)
#' plt3  <- grid.arrange(plt3a,plt3b,ncol=2)
#' 
#' ## realise the correct t-test to see the discrepancy
#' res   <- t.test(dataFigure3$VD[dataFigure3$grp==1], 
#'                dataFigure3$VD[dataFigure3$grp==2],
#'                var.equal=TRUE)
#' micc  <- mean(c(0.491334683772226, 0.20385744842838)) # mean ICC given by superbPlot
#' lam   <- CousineauLaurencelleLambda(c(micc, 5,5,5,5,5,5))
#' tcorr <- res$statistic / lam
#' pcorr <- 1-pt(tcorr,4)
#' 
#' 
"dataFigure3"