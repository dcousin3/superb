######################################################################################
#' @title superbPlot templates
#'
#' @aliases superbPlot.bar superbPlot.line superbPlot.point superbPlot.pointjitter superbPlot.pointjitterviolin superbPlot.pointindividualline
#'
#' @description superbPlot comes with a few built-in templates for
#' for making the final plots. All produces ggplot objects that
#' can be further customized. Additionally, it is possible to add
#' custom-make templates (see vignette 6). The functions, to be 
#' "superbPlot-compatible", must have these parameters:
#' 
#' If you have useful styles, do contribute github.com/dcousin3/superb
#' by opening an issue.
#' Finally, any number of optional list of parameters with default value set to list()
#' can be provided depending on the template style. These includes errorbarParams,
#' pointParams, barParams, etc.
#'
#'
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xvar a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfac a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param Debug a boolean to display or not debugging information 
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param barParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_errorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#'
#' @return a ggplot object
#'
#'
#' @export superbPlot.bar
#' @export superbPlot.line
#' @export superbPlot.point
#' @export superbPlot.pointjitter
#' @export superbPlot.pointjitterviolin
#' @export superbPlot.pointindividualline
#' @importFrom utils modifyList
#'

######################################################
######################################################
##  the basic plot formats: bar, line, point
######################################################
######################################################


superbPlot.bar <- function(
    summarydata,               # a summary result data.frame
    xvar,                      # the factor on the horizontal axis  
    groupingfac,               # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) (1 or 2 only) to make multiple panels
    Debug          = FALSE,    # TRUE to dump variables into global env()
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    barParams      = list(),   # merged into geom_bar
    errorbarParams = list(),   # merged into geom_errorbar
    facetParams    = list()    # merged into facet_grid
) {
    runDebug(Debug, "Entering superbPlot.bar", c("xvar2", "groupingfac2", "addfactors2"), list(xvar, groupingfac, addfactors))

    plot <- ggplot(
        summarydata, 
        aes_string(
            x = xvar, y = "center", 
            fill = groupingfac, 
            shape = groupingfac, 
            colour = groupingfac
    )) +
    # the histograms; do.call so that pointParams can be integrated
    do.call( geom_bar, modifyList(
       list(position = position_dodge(width = .95),
            stat = "identity" ),
        barParams
    )) +
    # the error bars; do.call so that errorbarParams can be integrated
    do.call( geom_errorbar, modifyList(
        list(width = .6, position = position_dodge(.95), mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
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


superbPlot.line <- function(
    summarydata,               # a summary result data.frame
    xvar,                      # the factor on the horizontal axis  
    groupingfac,               # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    Debug          = FALSE,    # TRUE to dump variables into global env()
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    pointParams     = list(), 
    lineParams      = list(), 
    errorbarParams  = list(),
    facetParams     = list()    
) {
    runDebug(Debug, "Entering superbPlot.line", c("xvar2", "groupingfac2", "addfactors2"), list(xvar, groupingfac, addfactors))

    plot <- ggplot(
        summarydata, 
        aes_string(
            x = xvar, y = "center", ymin = "center + lowerwidth", ymax = "center + upperwidth", 
            shape = groupingfac, 
            colour = groupingfac
    )) +
    # the points ...
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .15), 
            stat = "identity", 
            mapping = aes_string(group = groupingfac) ),
        pointParams
    )) +
    # ... and the lines connecting the points
    do.call(geom_line, modifyList(
        list(position = position_dodge(width = .15), 
            stat = "identity", 
            mapping = aes_string(group = ifelse(is.null(groupingfac),1,groupingfac) ) ),
        lineParams
    )) +
    # the error bars
    do.call(geom_errorbar, modifyList(
        list(width = 0.2, size = 0.5, position = position_dodge(.15)),
        errorbarParams
        )
    ) + 
    # the panels (rows or both rows and columns, NULL if no facet)
    do.call( facet_grid, modifyList(
        list( rows = addfactors ),
        facetParams
    ))
        
    return(plot)
}

 
######################################################


superbPlot.point <- function(
    summarydata,               # a summary result data.frame
    xvar,                      # the factor on the horizontal axis  
    groupingfac,               # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    Debug          = FALSE,    # TRUE to dump variables into global env()
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    pointParams     = list(), 
    errorbarParams  = list(),
    facetParams     = list()    
) {
    runDebug(Debug, "Entering superbPlot.point", c("xvar2", "groupingfac2", "addfactors2"), list(xvar, groupingfac, addfactors))

    plot <- ggplot(
        summarydata, 
        aes_string(
            x = xvar, y = "center", 
            shape = groupingfac, 
            colour = groupingfac
    )) + 
    # the points 
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .15), 
            stat = "identity", 
            mapping = aes_string(group = groupingfac) ),
        pointParams
    )) +
    # the error bars
    do.call(geom_errorbar, modifyList(
         list(width = 0.2, size = 0.5, position = position_dodge(.15), mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
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
## the newer ones
######################################################
######################################################


superbPlot.pointjitter <- function(
    summarydata,               # a summary result data.frame
    xvar,                      # the factor on the horizontal axis  
    groupingfac,               # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    Debug          = FALSE,    # TRUE to dump variables into global env()
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    pointParams     = list(), 
    jitterParams    = list(),  
    errorbarParams  = list(),
    facetParams     = list()    
) {
    runDebug(Debug, "Entering superbPlot.pointjitter", 
        c("xvar2", "groupingfac2", "addfactors2","pointParams2","jitterParams2","errorbarParams2"), list(xvar, groupingfac, addfactors, pointParams, jitterParams, errorbarParams))

    plot <- ggplot(
        summarydata, 
        aes_string(
            x = xvar,   
            shape = groupingfac, 
            colour = groupingfac
    )) + 
    # the points 
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .5), 
            stat = "identity", size=3,
            mapping = aes_string(y = "center", group = groupingfac) ),
        pointParams
    )) + 
    # the error bars; define ymin, ymax only in errorbar
    do.call(geom_errorbar, modifyList(
        list(position = position_dodge(.5), 
            mapping = aes_string(group = groupingfac, ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
        errorbarParams
    )) + 
    # the jitters 
    do.call(geom_point, modifyList(
        list(data = rawdata, 
            position = position_jitterdodge(jitter.width=0.1, dodge.width=0.5),
            mapping = aes_string(y = "DV", group = groupingfac ) ),
        jitterParams
    )) +
    # the panels (rows or both rows and columns, NULL if no facet)
    do.call( facet_grid, modifyList(
        list( rows = addfactors ),
        facetParams
    ))

    return(plot)
}


######################################################


superbPlot.pointjitterviolin <- function(
    summarydata,               # a summary result data.frame
    xvar,                      # the factor on the horizontal axis  
    groupingfac,               # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    Debug          = FALSE,    # TRUE to dump variables into global env()
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    pointParams     = list(), 
    jitterParams    = list(), 
    violinParams    = list(), 
    errorbarParams  = list(),
    facetParams     = list()
) {
    runDebug(Debug, "Entering superbPlot.pointjitterviolin", 
        c("xvar2", "groupingfac2", "addfactors2","pointParams2","jitterParams2","violinParams2","errorbarParams2"), list(xvar, groupingfac, addfactors, pointParams, jitterParams, violinParams, errorbarParams))

    plot <- ggplot(data    = summarydata, 
                  mapping = aes_string(x = xvar )
        ) +
        do.call( geom_violin, modifyList(
            list(data   = rawdata, 
                mapping = aes_string(y = "DV", fill = groupingfac), 
                scale   = "area", trim = F, alpha = 0.7),
            violinParams) )+
        do.call( geom_point, modifyList(
            list(mapping = aes_string(colour = groupingfac, group = groupingfac, y = "center"), 
                size = 4, position = position_dodge(.9) ),
            pointParams) ) +
        do.call( geom_errorbar, modifyList(
            list(mapping = aes_string(group = groupingfac, colour= groupingfac, ymin = "center+lowerwidth", ymax = "center+upperwidth"), 
                position = position_dodge(.9), width = 0.4),
            errorbarParams) )+
        do.call( geom_point, modifyList(
            list(data = rawdata, 
                mapping = aes_string(y = "DV", colour = groupingfac),
                position = position_jitterdodge(jitter.width=0.15, dodge.width=0.9) ),
            jitterParams) ) +
        do.call( facet_grid, modifyList(
            list( rows = addfactors ),
            facetParams
        ))

    return(plot)
}



######################################################

 
superbPlot.pointindividualline <- function(
    summarydata,               # a summary result data.frame
    xvar,                      # the factor on the horizontal axis  
    groupingfac,               # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    Debug          = FALSE,    # TRUE to dump variables into global env()
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    pointParams    = list(), 
    lineParams     = list(),  
    errorbarParams = list(),
    facetParams    = list()  
) {
    runDebug(Debug, "Entering superbPlot.pointindividualline", 
        c("xvar2", "groupingfac2", "addfactors2","pointParams2","lineParams2","errorbarParams2"), list(xvar, groupingfac, addfactors, pointParams, lineParams, errorbarParams))

    plot <- ggplot(
        summarydata, 
        aes_string(
            x = xvar,   
            shape = groupingfac, 
            colour = groupingfac
    )) + 
    # the individual lines 
    do.call(geom_line, modifyList(
        list(data = rawdata,
            color="gray", size=0.2,
            mapping = aes_string(y = "DV", group = "id" ) ),
        lineParams
    )) +
    # the points 
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .5), 
            stat = "identity", size=3,
            mapping = aes_string(y = "center", group = groupingfac) ),
        pointParams
    )) + 
    # the error bars; define ymin, ymax only in errorbar
    do.call(geom_errorbar, modifyList(
        list(position = position_dodge(.5), width = 0.1,
            mapping = aes_string(group = groupingfac, ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
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
