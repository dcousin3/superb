######################################################################################
#' @title User Interface to get summary plot of any statistics with adjusted error bars.
#'
#' @md
#'
#' @description The function ``suberbShiny()`` provides a simple user interface 
#'   to plot standard error or confidence interval for various descriptive 
#'   statistics under various designs, population size and purposes,
#'   according to the ``suberb`` framework. See \insertCite{cgh21}{superb} for more.
#'   Also see this \href{https://www.youtube.com/watch?v=rw_6ll5nVus/}{video} 
#'   from \insertCite{w21}{superb} for a demo using
#'   the shinyapps.io installation accessible at 
#'   \href{https://dcousin3.shinyapps.io/superbshiny/}{dcousin3.shinyapps.io/superbshiny}
#'   Limitations: it is not possible to use custom-made statistics with the 
#'   graphical user interface, nor is it possible to request an adjustment for cluster-
#'   randomized sampling. These options are available with ``superb()``.
#'
#' @param graphicDirectives (optional) used to set graphic directives from the command line. 
#'    This is useful for in-class demonstrations where the ylim() range should be set before
#'    reaching the last step of the GIU.
#'
#' @return A plot that can be cut-and-paste.
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



superbShiny <- function( graphicDirectives = NULL ) {
    dir <- system.file("superbShiny", package="superb")
    options(shiny.launch.browser = TRUE) 
    
    # send the additional graphic directives into options for easier retrieval by the app
    options( "superb.shiny.GR" = graphicDirectives )
    
    shiny::runApp(dir)
}
