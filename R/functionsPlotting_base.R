######################################################
######################################################
##  the basic plot formats: bar, line, point
######################################################
######################################################


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
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_errorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
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
    errorbarParams = list(),   # merged into geom_errorbar
    facetParams    = list(),   # merged into facet_grid
    xAsFactor      = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("bar", "Entering superbPlot.bar", c("xfactor2", "groupingfactor2", "addfactors2"), list(xfactor, groupingfactor, addfactors))

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] = as.numeric(summarydata[[xfactor]])

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes_string(
            x = xfactor, y = "center", 
            fill = groupingfactor, 
            shape = groupingfactor, 
            colour = groupingfactor
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
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_errorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
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
    runDebug("line", "Entering superbPlot.line", c("xfactor2", "groupingfactor2", "addfactors2"), list(xfactor, groupingfactor, addfactors))

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] = as.numeric(summarydata[[xfactor]])

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes_string(
            x = xfactor, y = "center", ymin = "center + lowerwidth", ymax = "center + upperwidth", 
            shape = groupingfactor, 
            colour = groupingfactor
    )) +
    # the points ...
    do.call(geom_point, modifyList(
        pointParams, 
        list(position = position_dodge(width = .15), 
            mapping = aes_string(group = groupingfactor) )
    )) +
    # ... and the lines connecting the points
    do.call(geom_line, modifyList(
        list(position = position_dodge(width = .15), 
            mapping = aes_string(group = ifelse(is.null(groupingfactor),1,groupingfactor) ) ),
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
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_errorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
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

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] = as.numeric(summarydata[[xfactor]])

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes_string(
            x = xfactor, y = "center", 
            shape = groupingfactor, 
            colour = groupingfactor
    )) + 
    # the points 
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .15), 
            mapping = aes_string(group = groupingfactor) ),
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


######################################################################################
#' @name superbPlot.pointjitter
#'
#' @title superbPlot point-and-jitter dots layout
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
#' @param jitterParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_errorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @export superbPlot.pointjitter
#'
######################################################################################

superbPlot.pointjitter <- function(
    summarydata,               # a summary result data.frame
    xfactor,                   # the factor on the horizontal axis  
    groupingfactor,            # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    rawdata,                   # the raw data in long format
    # what follows are optional
    pointParams    = list(), 
    jitterParams   = list(),  
    errorbarParams = list(),
    facetParams    = list(),   
    xAsFactor      = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("pointjitter", "Entering superbPlot.pointjitter", 
        c("xfactor2", "groupingfactor2", "addfactors2","pointParams2","jitterParams2","errorbarParams2"), list(xfactor, groupingfactor, addfactors, pointParams, jitterParams, errorbarParams))

    # depending on the scale of the x-axis.
    if (!xAsFactor) {
        summarydata[[xfactor]] = as.numeric(summarydata[[xfactor]])
        rawdata[[xfactor]] = as.numeric(rawdata[[xfactor]])
    }

    # determining the type of jitter based on the presence or not of a groupingfac
    if (is.null(groupingfactor)) {
        do_jitters = do.call(geom_jitter, modifyList(
                        list(data = rawdata, alpha = 0.2, width = 0.2, height = 0.0,
                             mapping = aes_string(y = "DV" ) ),
                        jitterParams
                    ) )
    } else {
        do_jitters = do.call(geom_point, modifyList(
                        list(data = rawdata , alpha = 0.2,
                            position = position_jitterdodge(jitter.width=0.1 , dodge.width=0.5 ),
                            mapping = aes_string(y = "DV", color = groupingfactor  ) ),
                        jitterParams
                    ) )
    }

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes_string( x = xfactor, color = groupingfactor )
    ) + 
    # the jitters 
    do_jitters +
    # the points 
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .5), 
            size=3,
            mapping = aes_string(group = groupingfactor, y = "center" ) ),
        pointParams
    )) + 
    # the error bars; define ymin, ymax only in errorbar
    do.call(geom_errorbar, modifyList(
        list(position = position_dodge(.5), width = 0.5, size = 0.5,
            mapping = aes_string(group = groupingfactor, ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
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
#' @name superbPlot.pointjitterviolin
#'
#' @title superbPlot point, jitter and violin plot layout
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
#' @param jitterParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param violinParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_errorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#'
#' @return a ggplot object
#'
#' @export superbPlot.pointjitterviolin
#'
######################################################################################

superbPlot.pointjitterviolin <- function(
    summarydata,               # a summary result data.frame
    xfactor,                   # the factor on the horizontal axis  
    groupingfactor,            # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    rawdata,                   # the raw data in long format
    # what follows are optional
    pointParams    = list(), 
    jitterParams   = list(), 
    violinParams   = list(), 
    errorbarParams = list(),
    facetParams    = list()
) {
    runDebug("pointjitterviolin", "Entering superbPlot.pointjitterviolin", 
        c("xfactor2", "groupingfactor2", "addfactors2","pointParams2","jitterParams2","violinParams2","errorbarParams2"), list(xfactor, groupingfactor, addfactors, pointParams, jitterParams, violinParams, errorbarParams))

    # determining the type of jitter based on the presence or not of a groupingfac
    if (is.null(groupingfactor)) {
        do_jitters = do.call(geom_jitter, modifyList(
                        list(data = rawdata, alpha = 0.2, width = 0.2, height = 0.0,
                             mapping = aes_string( y = "DV" ) ),
                        jitterParams
                    ) )
        do_violins = do.call( geom_violin, modifyList(
                        list(data     = rawdata,
                             mapping  = aes_string( y = "DV" ), 
                             scale    = "area", trim = FALSE, alpha = 0.25),
                        violinParams
                    ) )
    } else {
        do_jitters = do.call(geom_point, modifyList(
                        list(data = rawdata , alpha = 0.2,
                            position = position_jitterdodge(jitter.width=0.1 , dodge.width=.75 ),
                            mapping = aes_string(y = "DV", group = groupingfactor  ) ),
                        jitterParams
                    ) )
        do_violins = do.call( geom_violin, modifyList(
                        list(data    = rawdata, 
                             position= position_dodge(.75), #"dodge",
                             mapping = aes_string( y = "DV", fill = groupingfactor), 
                             scale   = "area", trim = FALSE, alpha = 0.25),
                        violinParams
                    ) )
    }

    # let's do the plot!
    plot <- ggplot(data    = summarydata, 
                   mapping = aes_string(x = xfactor, colour = groupingfactor )
        ) +
        # violin in the back
        do_violins +
        # jitter second
        do_jitters +
        # and finally the point and the error bars
        do.call( geom_point, modifyList(
            list(mapping = aes_string(group = groupingfactor, y = "center"), 
                size = 4, position = position_dodge(.75) ),
            pointParams) ) +
        do.call( geom_errorbar, modifyList(
            list(mapping = aes_string(group = groupingfactor, ymin = "center+lowerwidth", ymax = "center+upperwidth"), 
                position = position_dodge(.75), width = 0.4),
            errorbarParams) )+
        do.call( facet_grid, modifyList(
            list( rows = addfactors ),
            facetParams
        ))

    return(plot)
}



 
######################################################################################
#' @name superbPlot.pointindividualline
#'
#' @title superbPlot point and individual-line layout for within-subject design
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
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_errorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#'
#' @return a ggplot object
#'
#' @export superbPlot.pointindividualline
#'
#' @importFrom utils modifyList
#'
######################################################################################

superbPlot.pointindividualline <- function(
    summarydata,               # a summary result data.frame
    xfactor,                   # the factor on the horizontal axis  
    groupingfactor,            # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    rawdata,                   # the raw data in long format
    # what follows are optional
    pointParams    = list(), 
    lineParams     = list(),  
    errorbarParams = list(),
    facetParams    = list() 
) {
    runDebug("pointindividualline", "Entering superbPlot.pointindividualline", 
        c("xfactor2", "groupingfactor2", "addfactors2","pointParams2","lineParams2","errorbarParams2"), list(xfactor, groupingfactor, addfactors, pointParams, lineParams, errorbarParams))

    # let's do the plot!
    plot <- ggplot(
        data = summarydata, 
        aes_string(
            x = xfactor,   
            colour = groupingfactor
    )) + 
    # the individual lines 
    do.call(geom_line, modifyList(
        list(data = rawdata,
            size=0.2, alpha = 0.25,
            mapping = aes_string( y = "DV", group = "id" ) ),
        lineParams
    )) +
    # the individual points 
    do.call(geom_point, modifyList(
        list(data = rawdata, alpha = 0.25,
            mapping = aes_string(y = "DV", group = "id") ),
        pointParams
    )) + 
    # the points 
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .5), 
            size=4,
            mapping = aes_string(y = "center", group = groupingfactor) ),
        pointParams
    )) + 
    # the error bars; define ymin, ymax only in errorbar
    do.call(geom_errorbar, modifyList(
        list(position = position_dodge(.5), width = 0.1,
            mapping = aes_string(group = groupingfactor, ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
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
