######################################################################################
#' @title User Interface to get summary plot of any statistics with adjusted error bars.
#'
#' @md
#'
#' @description The function ``suberbShiny()`` provides a simple user interface 
#'      to plot standard error or confidence interval for various descriptive 
#'      statistics under various designs, population size and purposes,
#'      according to the ``suberb`` framework. See \insertCite{c17}{superb} for more.
#'      Limitations: it is not possible to use custom-made statistics with the 
#'      user interface, nor is it possible to request an adjustment for cluster-
#'      randomized sampling. These options are available with ``superbPlot()``.
#'
#' @return nothing.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # Launch the user interface: 
#' \donttest{
#' if (interactive())
#'    superbShiny() 
#' }
#'
#' @export superbShiny
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom foreign read.spss
#' @import ggplot2
#' @import shiny
#' @import shinyBS
#
######################################################################################



superbShiny <- function() {
    dir <- system.file("superbShiny", package="superb")
    options(shiny.launch.browser = TRUE) 
    shiny::runApp(dir)
}
