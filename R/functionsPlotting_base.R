##################################################################
##################################################################
##  Three basic plot layout: bar, point, line
##################################################################
##################################################################


######################################################################################
#' @name superbPlot.bar
#'
#' @title superbPlot 'bar' layout
#'
#' @md
#'
#' @description superbPlot comes with a few built-in templates for making the final plots.
#' All produces ggplot objects that can be further customized. Additionally, it is
#' possible to add custom-make templates (see vignette 6). The functions, to be
#' "superbPlot-compatible", must have these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param barParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with bars
#' superb(
#'    len ~ dose + supp, 
#'    ToothGrowth, 
#'    plotStyle="bar" 
#' )
#'
#' # if you extract the data with superbData, you can 
#' # run this layout directly
#' #processedData <- superb(
#' #   len ~ dose + supp,
#' #   ToothGrowth, 
#' #   showPlot = FALSE
#' #)
#' #
#' #superbPlot.bar(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @export superbPlot.bar
#'
######################################################################################

superbPlot.bar <- function(
    summarydata,               # a summary result data.frame
    xfactor,                   # the factor on the horizontal axis  
    groupingfactor,            # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) (1 or 2 only) to make multiple panels
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    barParams      = list(),   # merged into geom_bar
    errorbarParams = list(),   # merged into geom_superberrorbar
    facetParams    = list(),   # merged into facet_grid
    xAsFactor      = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("bar", "Entering superbPlot.bar", c("xfactor2", "groupingfactor2", "addfactors2"), list(xfactor, groupingfactor, addfactors))
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] <- unfactor(summarydata[[xfactor]])
        #summarydata[[xfactor]] = as.numeric(summarydata[[xfactor]])

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes(
            x      = !!mysym(xfactor), 
            y      = center, 
            fill   = !!mysym(groupingfactor), 
            shape  = !!mysym(groupingfactor), 
            colour = !!mysym(groupingfactor)
    )) +
    # the histograms; do.call so that pointParams can be integrated
    do.call( geom_bar, modifyList(
       list(position = position_dodge2(width = .50),
            stat = "identity" ),
        barParams
    )) +
    # the error bars; do.call so that errorbarParams can be integrated
    do.call( geom_superberrorbar, modifyList(
        list(position = position_dodge2(width = .50),  #removed width before position.
		mapping = 
            #aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
            aes(ymin = center + lowerwidth, ymax = center + upperwidth) ),
        errorbarParams
    )) +
    # the panels (rows or both rows and columns, NULL if no facet)
    do.call( facet_grid, modifyList(
        list( rows = addfactors ),
        facetParams
    ))
        
    return(plot)
}

 

######################################################################################
#' @name superbPlot.line
#'
#' @title superbPlot 'line' layout
#'
#' @md
#'
#' @description superbPlot comes with a few built-in templates for making the final plots.
#' All produces ggplot objects that can be further customized. Additionally, it is
#' possible to add custom-make templates (see vignette 6). The functions, to be
#' "superbPlot-compatible", must have these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param pointParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param lineParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with lines
#' superb(
#'    len ~ dose + supp, 
#'    ToothGrowth, 
#'    plotStyle="line" 
#' )
#'
#' # if you extract the data with superbData, you can 
#' # run this layout directly
#' #processedData <- superb(
#' #   len ~ dose + supp,
#' #   ToothGrowth, 
#' #   showPlot = FALSE
#' #)
#' #
#' #superbPlot.line(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @export superbPlot.line
#'
######################################################################################

superbPlot.line <- function(
    summarydata,               # a summary result data.frame
    xfactor,                   # the factor on the horizontal axis  
    groupingfactor,            # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    pointParams    = list(), 
    lineParams     = list(), 
    errorbarParams = list(),
    facetParams    = list(),
    xAsFactor = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("line", "Entering superbPlot.line", c("xfactor2", "groupingfactor2", "addfactors2", "params"), 
        list(xfactor, groupingfactor, addfactors, list(pointParams=pointParams, lineParams=lineParams, errorbarParams=errorbarParams))
    )
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] = unfactor(summarydata[[xfactor]])

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes(
            x = !!mysym(xfactor), y = center, ymin = center + lowerwidth, ymax = center + upperwidth, 
            colour = !!mysym(groupingfactor)
    )) +
    # the points ...
    do.call(geom_point, modifyList(
        list(size = 3, position = position_dodge(width = .15), 
            mapping = aes(group = !!mysym(groupingfactor) ) ),
        pointParams
    )) +
    # ... and the lines connecting the points
    do.call(geom_line, modifyList(
        list(position = position_dodge(width = .15), 
            mapping = aes(group = !!mysym(ifelse(is.null(groupingfactor),1,groupingfactor)) ) ),
        lineParams
    )) +
    # the error bars
    do.call(geom_superberrorbar, modifyList(
        list(width = 0.1, linewidth = 0.75, position = position_dodge(.15),
            mapping = aes(group = !!mysym(groupingfactor)) ),
        errorbarParams
    )) + 
    # the panels (rows or both rows and columns, NULL if no facet)
    do.call( facet_grid, modifyList(
        list( rows = addfactors ),
        facetParams
    ))
        
    return(plot)
}

 

######################################################################################
#' @name superbPlot.point
#'
#' @title superbPlot 'point' layout
#'
#' @md
#'
#' @description superbPlot comes with a few built-in templates for making the final plots.
#' All produces ggplot objects that can be further customized. Additionally, it is
#' possible to add custom-make templates (see vignette 6). The functions, to be
#' "superbPlot-compatible", must have these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param pointParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with points
#' superbPlot(ToothGrowth, 
#'    BSFactors = c("dose","supp"), variables = "len",
#'    plotStyle = "point" 
#' )
#'
#' # if you extract the data with superbData, you can 
#' # run this layout directly
#' #processedData <- superbData(ToothGrowth, 
#' # BSFactors = c("dose","supp"), variables = "len"
#' #)
#' #
#' #superbPlot.point(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @export superbPlot.point
#'
######################################################################################

superbPlot.point <- function(
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
    xAsFactor      = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("point", "Entering superbPlot.point", c("xfactor2", "groupingfactor2", "addfactors2"), list(xfactor, groupingfactor, addfactors))
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] = unfactor(summarydata[[xfactor]])

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes(
            x = !!mysym(xfactor), y = center, 
            shape = !!mysym(groupingfactor), 
            colour = !!mysym(groupingfactor)
    )) + 
    # the points 
    do.call(geom_point, modifyList(
        list(size = 3, position = position_dodge(width = .15), 
            #mapping = aes_string(group = groupingfactor) ),
            mapping = aes(group = !!mysym(groupingfactor)) ),
        pointParams
    )) +
    # the error bars
    do.call(geom_superberrorbar, modifyList(
         list(width = 0.2, linewidth = 0.5, position = position_dodge(.15), 
            #mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
            mapping = aes(ymin = center + lowerwidth, ymax = center + upperwidth) ),
         errorbarParams
    )) +
    # the panels (rows or both rows and columns, NULL if no facet)
    do.call( facet_grid, modifyList(
        list( rows = addfactors ),
        facetParams
    ))
    
    return(plot)
}



######################################################
######################################################
## end of the built-in templates
######################################################
######################################################
