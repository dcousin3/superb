#' Data for Figure 1
#'
#' The data, taken from Cousineau, 2017, is an example where the
#' "stand-alone" 95% confidence interval of the means returns
#' a result in contradiction with the result of a statistical test.
#' The paradoxical result is resovled by using adjusted confidence intervals,
#' here the different-adjusted confidence interval.
#'
#' @docType data
#'
#' @usage data(dataFigure1)
#'
#' @format An object of class data.frame.
#'
#' @keywords datasets
#'
#' @references Cousineau, D. (2017) Advances in cognitive psychology, 13:140-155
#' (\href{https://doi.org/10.5709#acp-0214-z}{doi})
#'
#' @source \href{https://doi.org/10.5709#acp-0214-z}{doi}
#'
#' @examples
#' library(gridExtra)
#' data(dataFigure1)
#' 
#' ## realize the plot with unadjusted (left) and ajusted (right) 95% confidence intervals
#' plt1a <- superbPlot(dataFigure1, BSFactor = "grp", 
#'     adjustments=list(purpose = "single"),
#'     variables = c("score"), plotStyle="bar" ) + 
#'   xlab("Group") + ylab("Score") + labs(title="95% CI\n") +
#'   coord_cartesian( ylim = c(85,115) ) +
#'   geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2)
#' plt1b <- superbPlot(dataFigure1, BSFactor = "grp", 
#'     adjustments=list(purpose = "difference"),
#'     variables = c("score"), plotStyle="bar" ) + 
#'   xlab("Group") + ylab("Score") + labs(title="Difference-adjusted 95% CI\n") +
#'   coord_cartesian( ylim = c(85,115) ) + 
#'   geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2)
#' plt1  <- grid.arrange(plt1a,plt1b,ncol=2)
#' 
#' ## realise the correct t-test to see the discrepancy
#' t.test(dataFigure1$score[dataFigure1$grp==1], 
#'        dataFigure1$score[dataFigure1$grp==2],
#'        var.equal=T)
#' 
"dataFigure1"