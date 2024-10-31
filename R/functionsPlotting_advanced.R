######################################################
######################################################
##  Two advanced plot layouts: raincloud, halfwidthline
######################################################
######################################################



######################################################################################
#' @name superbPlot.raincloud
#'
#' @title superbPlot 'raincloud' layout
#'
#' @md
#'
#' @description The raincloud layout display jittered dots as well as a "cloud" (half of a violin) above them.
#' See @allen2019.
#' The functions has these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param pointParams (optional) list of graphic directives that are sent to the geom_point layer
#' @param jitterParams (optional) list of graphic directives that are sent to the geom_jitter layer
#' @param violinParams (optional) list of graphic directives that are sent to the geom_violin layer;
#'     this modified geom_violin has additional options "direction" and "push".
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with raincloud; they are better seen rotated: +coord_flip()
#' superb(
#'    len ~ dose + supp,
#'    ToothGrowth, 
#'    plotStyle="raincloud" 
#' )
#'
#' # if you extract the data with superbData, you can 
#' # run this layout directly
#' #processedData <- superb(ToothGrowth, 
#' #   len ~ dose + supp,
#' #   showPlot = FALSE
#' #)
#' #
#' #superbPlot.raincloud(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @references
#' \insertAllCited{}
#'
#' @export superbPlot.raincloud
#'
######################################################################################

superbPlot.raincloud <- function(
    summarydata,               # a summary result data.frame
    xfactor,                   # the factor on the horizontal axis  
    groupingfactor,            # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) (1 or 2 only) to make multiple panels
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    violinParams   = list(),   # merged into geom_flat_violin
    jitterParams   = list(),   # merged into geom_jitter
    pointParams    = list(),   # merged into geom_point
    errorbarParams = list(),   # merged into geom_errorbar
    facetParams    = list(),   # merged into facet_grid
    xAsFactor      = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("raincloud", "Entering superbPlot.raincloud", c("xfactor2", "groupingfactor2", "addfactors2"), list(xfactor, groupingfactor, addfactors))
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] = unfactor(summarydata[[xfactor]])

    # rename column "DV" as "center"
    rawdata$center <- rawdata$DV

    # determining the type of jitter based on the presence or not of a groupingfac
    if (is.null(groupingfactor)) {
        do_jitters = do.call(geom_jitter, modifyList(
                        list(data = rawdata, alpha = 0.5, size = 1.5, width = 0.2, height = 0.0,
                            mapping = aes(y = center ) ),
                        jitterParams
                    ) )
        do_violins = do.call( geom_flat_violin, modifyList(
                        list(data     = rawdata,
                            mapping   = aes( y = center ), 
                            scale     = "area", trim = FALSE, alpha = 0.25,
                            push      = 0.25, width = 0.66,
                            direction = 1),
                        violinParams
                    ) )
    } else {
        do_jitters = do.call( geom_point, modifyList(
                        list(data = rawdata, alpha = 0.5, size = 1.5,
                            position = position_jitterdodge(dodge.width = .15, jitter.width=0.15), 
                            mapping = aes(y = center) ),
                        jitterParams
                    ))
        do_violins = do.call( geom_flat_violin, modifyList(
                        list(data     = rawdata, 
                            position  = position_nudge(x=0.25, y = 0), #"dodge",
                            mapping   = aes( y = center, fill = !!mysym(groupingfactor) ), 
                            scale     = "area", trim = FALSE, alpha = 0.25,
                            push      = 0.25, width = 0.66,
                            direction = 1),
                        violinParams
                    ) )
    }

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        # Because aes_string is deprecated, we switch to the magical pair !!sym(string)...
        aes(
            x = !!mysym(xfactor), y = center, 
            fill = !!mysym(groupingfactor), 
            shape = !!mysym(groupingfactor), 
            colour = !!mysym(groupingfactor)
    )) +
    # violins in the back
    do_violins +
    # the jittered data
    do_jitters +
    # the summary data
    do.call( geom_point, modifyList(
        list(position = position_dodge(.25),  size = 3.5, color = "black" ),
        pointParams
    )) +
    # the error bars; do.call so that errorbarParams can be integrated
    do.call( geom_superberrorbar, modifyList(
        list(position = position_dodge(.25), width = 0.2, linewidth = 1., color = "black",
            #mapping = aes_string( ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
            mapping = aes( ymin = center + lowerwidth, ymax = center + upperwidth) ),
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
#' @name superbPlot.halfwidthline
#'
#' @title superbPlot 'halfwidthline' layout
#'
#' @md
#'
#' @description superbPlot comes with a few built-in templates for making the final plots.
#' All produces ggplot objects that can be further customized. The half-width confidence
#' interval line plot is EXPERIMENTAL. It divides the CI length by two, one thick section and one thin section.
#' The functions, to be "superbPlot-compatible", must have these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param pointParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param lineParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param errorbarlightParams (optional) graphic directives for the second half of the error bar;
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
#'    plotStyle="halfwidthline" 
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
#' #superbPlot.halfwidthline(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @export superbPlot.halfwidthline
#'
######################################################################################

superbPlot.halfwidthline <- function(
    summarydata,           		    # a summary result data.frame
    xfactor,                   		# the factor on the horizontal axis  
    groupingfactor,            		# the factor for multiple lines/bars within the plot
    addfactors,                		# the factor(s) to make multiple panels
    # what follows is unused and optional
    rawdata             = NULL,     # unused
    # what follows are optional
    pointParams         = list(), 
    lineParams          = list(), 
    errorbarParams      = list(),
    errorbarlightParams = list(),
    facetParams         = list(),
    xAsFactor           = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("halfwidthline", "Entering superbPlot.halfwidthline", c("xfactor2", "groupingfactor2", "addfactors2", "params"), list(xfactor, groupingfactor, addfactors, list(pointParams=pointParams, lineParams=lineParams, errorbarParams=errorbarParams)))
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # compute half-width limits
    summarydata$hwlowerwidth = summarydata$lowerwidth/2
    summarydata$hwupperwidth = summarydata$upperwidth/2

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
            #mapping = aes_string(group = groupingfactor) ),
            mapping = aes(group = !!mysym(groupingfactor)) ),
        pointParams
    )) +
    # ... and the lines connecting the points
    do.call(geom_line, modifyList(
        list(position = position_dodge(width = .15), 
            #mapping = aes_string(group = ifelse(is.null(groupingfactor),1,groupingfactor) ) ),
            mapping = aes(group = !!mysym(ifelse(is.null(groupingfactor),1,groupingfactor)) ) ),
        lineParams
    )) +
    # the error bars
    do.call(geom_superberrorbar, modifyList(
        list(width = 0.1, linewidth = 1.00, position = position_dodge(.15),
            aes(
                x = !!mysym(xfactor), y = center, ymin = center + lowerwidth, ymax = center + upperwidth, 
                colour = !!mysym(groupingfactor) ) ),
        errorbarParams
    )) + 
    # the lower small white points to cut the line in two
    do.call(geom_point, modifyList(
        list(color = "white", size = 1.5, position = position_dodge(.15),
            #mapping = aes_string(group = groupingfactor, y = "center + hwlowerwidth") ),
            mapping = aes(group = !!mysym(groupingfactor), y = center + hwlowerwidth) ),
        errorbarlightParams
    )) + 
    # the uppersmall white points to cut the line in two
    do.call(geom_point, modifyList(
        list(color = "white", size = 1.5, position = position_dodge(.15),
            #mapping = aes_string(group = groupingfactor, y = "center + hwupperwidth") ),
            mapping = aes(group = !!mysym(groupingfactor), y = center + hwupperwidth) ),
        errorbarlightParams
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
## end of advanced plot layouts
######################################################
######################################################
