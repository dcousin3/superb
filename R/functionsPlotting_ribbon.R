######################################################
######################################################
##  One advanced plot layout: lineBand
######################################################
######################################################



######################################################################################
#' @name superbPlot.lineBand
#'
#' @title superbPlot 'lineBand' layout
#'
#' @md
#'
#' @description The lineBand layout displays an error band instead of individual error bars. This layout is
#' convenient when you have many points on your horizontal axis (so that the error bars are difficult to distinguish)
#' and when the results are fairly smooth.
#'
#' The functions has these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param errorbandParams (optional) list of graphic directives that are sent to the geom_ribbon layer
#' @param pointParams (optional) list of graphic directives that are sent to the geom_point layer
#' @param lineParams (optional) list of graphic directives that are sent to the geom_jitter layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # this creates a fictious time series at 100 time points obtained in two conditions:
#' dta <- GRD( WSFactors = "timepoints (50) : condition(2)", 
#'     SubjectsPerGroup = 20,
#'     RenameDV = "activation",
#'     Effects = list("timepoints" = extent(5), "condition" = extent(3) ),
#'     Population=list(mean=50,stddev=10,rho=0.75)
#' )
#' 
#' # This will make a plot with error band
#' superb(
#'    crange(activation.1.1, activation.50.2) ~ ., 
#'    dta, 
#'    WSFactors   = c("timepoints(50)", "condition(2)"),
#'    adjustments = list(
#'         purpose       = "single",
#'         decorrelation = "CM"        ## or none for no decorrelation
#'    ),
#'    plotLayout  = "lineBand",        # note the uppercase B 
#'    pointParams = list(size= 1)      # making points smaller has better look
#' )
#'
#' # if you extract the data with superbData, you can 
#' # run this layout directly
#' #processedData <- superb(
#' #   crange(activation.1.1, activation.50.2) ~ ., 
#' #   dta, 
#' #   WSFactors   = c("timepoints(50)", "condition(2)"), variables = colnames(dta)[2:101],
#' #   adjustments = list(
#' #        purpose       = "single",
#' #        decorrelation = "CM"        ## or none for no decorrelation
#' #   )
#' #)
#' #
#' #superbPlot.lineBand(processedData$summaryStatistic,
#' #   "timepoints",
#' #   "condition",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @references
#' \insertAllCited{}
#'
#' @export superbPlot.lineBand
#'
######################################################################################

superbPlot.lineBand <- function(
    summarydata,
    xfactor,       
    groupingfactor,
    addfactors, 
    rawdata,
    pointParams         = list(),
    lineParams          = list(),
    errorbandParams     = list(),
    facetParams         = list(),
    xAsFactor = TRUE
) {
    runDebug("lineBand", "Entering superbPlot.lineBand", c("xfactor2", "groupingfactor2", "addfactors2", "params"), 
        list(xfactor, groupingfactor, addfactors, list(pointParams=pointParams, lineParams=lineParams, errorbarParams=errorbandParams))
    )
    mysym <- function(x) { if(is.character(x)) sym(x) else x }
#    if (is.null(groupingfactor)) groupingfactor = 1

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
    # the error band
    do.call(geom_ribbon, modifyList(
        list(alpha = 0.3, linewidth = 0.75, colour = "00000000", 
            position = position_dodge(.15),
            mapping = aes(fill = !!mysym(groupingfactor), group = !!mysym(ifelse(is.null(groupingfactor),1,groupingfactor)) ) ),
        errorbandParams
    )) + 
    # the points ...
    do.call(geom_point, modifyList(
        list(size = 3, position = position_dodge(width = .15), 
            mapping = aes(group = !!mysym(ifelse(is.null(groupingfactor),1,groupingfactor)) ) ),
        pointParams
    )) +
    # ... and the lines connecting the points
    do.call(geom_line, modifyList(
        list(position = position_dodge(width = .15), 
            mapping = aes(group = !!mysym(ifelse(is.null(groupingfactor),1,groupingfactor)) ) ),
        lineParams
    )) +
    # the panels (rows or both rows and columns, NULL if no facet)
    do.call( facet_grid, modifyList(
        list( rows = addfactors ),
        facetParams
    ))
        
    return(plot)
}
