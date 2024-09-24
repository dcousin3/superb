######################################################################################
#' @title makes ggplots with transparent elements
#'
#' @description makeTransparent is an extension to ggplots
#'      which makes all the elements of the plot transparent
#'      except the data being displayed. This is useful to 
#'      superimpose multiple plots, e.g. to generate plots with
#'      multiple error bars for example.
#'
#' @md
#'
#' @examples
#' 
#' # make a basic plot
#' superb(len ~ dose + supp, ToothGrowth ) 
#' # make a basic plot with transparent elements
#' superb(len ~ dose + supp, ToothGrowth,  
#'   ) + makeTransparent()
#'
#' @return does not return anything; set the elements to transparent.
#'
#' @export makeTransparent
######################################################################################

makeTransparent <- function() {
    # no argument; just graphic directives to hide elements
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
        )
    }

##################################################################   
# End of makeTransparent.
##################################################################   
