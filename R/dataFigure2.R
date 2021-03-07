#' Data for Figure 2
#'
#' The data, taken from Cousineau, 2017, is an example where the
#' "stand-alone" 95% confidence interval of the means returns
#' a result in contradiction with the result of a statistical test.
#' The paradoxical result is resovled by using adjusted confidence intervals,
#' here the correlation- and different-adjusted confidence interval.
#'
#' @docType data
#'
#' @usage data(dataFigure2)
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
#' data(dataFigure2)
#' 
#' ## realize the plot with unadjusted (left) and ajusted (right) 95% confidence intervals
#' plt2a <- superbPlot(dataFigure2, WSFactor = "Moment(2)", 
#'     adjustments=list(purpose = "difference"),
#'     variables = c("pre","post"), plotStyle="bar" ) + 
#'   xlab("Group") + ylab("Score") + labs(title="Difference-adjusted 95% CI\n") +
#'   coord_cartesian( ylim = c(85,115) ) +
#'   geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2)
#' plt2b <- superbPlot(dataFigure2, WSFactor = "Moment(2)", 
#'     adjustments=list(purpose = "difference", decorrelation = "CA"),
#'     variables = c("pre","post"), plotStyle="bar" ) + 
#'   xlab("Group") + ylab("Score") + labs(title="Correlation and difference-adjusted\n95% CI") +
#'   coord_cartesian( ylim = c(85,115) ) + 
#'   geom_hline(yintercept = 100, colour = "black", size = 0.5, linetype=2)
#' plt2  <- grid.arrange(plt2a,plt2b,ncol=2)
#' 
#' ## realise the correct t-test to see the discrepancy
#' t.test(dataFigure2$pre, dataFigure2$post, paired=T)
#' 
"dataFigure2"