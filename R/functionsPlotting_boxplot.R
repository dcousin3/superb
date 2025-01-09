######################################################
######################################################
##  One advanced plot layouts: boxplot
######################################################
######################################################



######################################################################################
#' @name superbPlot.boxplot
#'
#' @title superbPlot 'boxplot' layout
#'
#' @md
#'
#' @description superbPlot comes with a few built-in templates for making the final plots.
#' All produces ggplot objects that can be further customized. Additionally, it is
#' possible to create custom-make templates (see vignette 5). The functions, to be
#' "superbPlot-compatible", must have these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors;
#' @param pointParams (optional) list of graphic directives that are sent to the geom_bar layer;
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer;
#' @param boxplotParams (optional) list f graphic directives that are sent to the geo_boxplot layer;
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer;
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete).
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with boxes for interquartile (box), median (line) and outliers (whiskers)
#' superb(
#'    len ~ dose + supp,
#'    ToothGrowth, 
#'    plotLayout = "boxplot" 
#' )
#'
#' # This layout of course is more meaningful if the statistic displayed is the median
#' superb(
#'    len ~ dose + supp,
#'    ToothGrowth, 
#'    statistic = "median",
#'    plotLayout = "boxplot" 
#' )
#'
#' # if you extracted the data with superbData, you can 
#' # run this layout directly
#' processedData <- superb(
#'    len ~ dose + supp,
#'    ToothGrowth, 
#'    statistic = "median",
#'    showPlot  = FALSE
#' )
#' 
#' superbPlot.boxplot(processedData$summaryStatistic,
#'    "dose", "supp", ".~.",
#'    processedData$rawData)
#'
#' # This will make a plot with customized boxplot parameters and black dots
#' superb(
#'    len ~ dose + supp,
#'    ToothGrowth, 
#'    statistic = "median",
#'    plotLayout = "boxplot",
#'    boxplotParams = list( outlier.shape=8, outlier.size=4 ),
#'    pointParams = list(color="black") 
#' )
#'
#' # You can customize the plot in various ways, e.g.
#' plt3 <- superb(
#'    len ~ dose + supp,
#'    ToothGrowth, 
#'    statistic = "median",
#'    plotLayout = "boxplot",
#'    pointParams = list(color="black")
#' )
#'
#' # ... by changing the colors of the fillings
#' library(ggplot2) # for scale_fill_manual, geom_jitter and geom_dotplot
#' plt3 + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
#'
#' # ... by overlaying jittered dots of the raw data
#' plt3 + geom_jitter(data = processedData$rawData, mapping=aes(x=dose, y=DV), 
#'    position= position_jitterdodge(jitter.width=0.5 , dodge.width=0.8 ) )
#'  
#' # ... by overlaying dots of the raw data, aligned along the center of the box
#' plt3 + geom_dotplot(data = processedData$rawData, mapping=aes(x=dose, y=DV), dotsize=0.5,
#'    binaxis='y', stackdir='center', position=position_dodge(0.7))  
#'  
#' 
#' @export superbPlot.boxplot
#'
######################################################################################

superbPlot.boxplot <- function(
    summarydata,               # a summary result data.frame
    xfactor,                   # the factor on the horizontal axis  
    groupingfactor,            # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    pointParams    = list(), 
    errorbarParams = list(),
    facetParams    = list(),
    boxplotParams  = list(),
    xAsFactor      = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("point", "Entering superbPlot.boxplot", c("xfactor2", "groupingfactor2", "addfactors2"), list(xfactor, groupingfactor, addfactors))
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] = unfactor(summarydata[[xfactor]])

# the x-axis var must be a factor
	rawdata[[xfactor]]     <- as.factor(rawdata[[xfactor]])
	summarydata[[xfactor]] <- as.factor(summarydata[[xfactor]])
	summarydata[[xfactor]] <- as.factor(summarydata[[xfactor]])

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes(
            x = !!mysym(xfactor), y = center, 
            shape = !!mysym(groupingfactor), 
            fill  = !!mysym(groupingfactor)
    )) + 
    # the boxes first so that they are underneath the bars and points
    do.call( geom_boxplot, modifyList(
        list( data = rawdata, width = 0.8-0.1, position = position_dodge2(width =0.8-0.1/2),
            mapping = aes( x = !!mysym(xfactor), y = DV ) ),
        boxplotParams
    )) +
    # the error bars
    do.call(geom_superberrorbar, modifyList(
         list(linewidth = 0.75, width=0.7, position = position_dodge2(width=0.15, padding=0.4),  #width = 0.2, 
            #mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
            mapping = aes(ymin = center + lowerwidth, ymax = center + upperwidth) ),
         errorbarParams
    )) +
    # the points 
    do.call(geom_point, modifyList(
        list(size = 3, position = position_dodge2(width =0.8-0.1/1), #(width+padding)/2
            #mapping = aes_string(group = groupingfactor) ),
            mapping = aes(color = !!mysym(groupingfactor)) ),
        pointParams
    )) +
    # the panels (rows or both rows and columns, NULL if no facet)
    do.call( facet_grid, modifyList(
        list( rows = addfactors ),
        facetParams
    ))
    
    return(plot)
}

