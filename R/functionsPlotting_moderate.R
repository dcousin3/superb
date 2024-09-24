##################################################################
##################################################################
##  Three moderately-advanced plot layout: 
##       pointjitter, pointjitterviolin, pointindividualline 
##################################################################
##################################################################



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
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with jittered points, aka dot plots
#' superb(
#'    len ~ dose + supp,
#'    ToothGrowth, 
#'    plotStyle="pointjitter" 
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
#' #superbPlot.pointjitter(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
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
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # rename column "DV" to "center"
    rawdata$center <- rawdata$DV
					
    # depending on the scale of the x-axis.
    if (!xAsFactor) {
        summarydata[[xfactor]] = as.numeric(summarydata[[xfactor]])
        rawdata[[xfactor]] = as.numeric(rawdata[[xfactor]])
    }

    # determining the type of jitter based on the presence or not of a groupingfac
    if (is.null(groupingfactor)) {
        do_jitters = do.call(geom_jitter, modifyList(
                        list(data = rawdata, alpha = 0.2, width = 0.2, height = 0.0,
                             mapping = aes(y = center ) ),
                        jitterParams
                    ) )
    } else {
        do_jitters = do.call(geom_point, modifyList(
                        list(data = rawdata , alpha = 0.2,
                            position = position_jitterdodge(jitter.width=0.1 , dodge.width=0.5 ),
                            #mapping = aes_string(y = "center", color = groupingfactor  ) ),
                            mapping = aes(y = center, color = !!mysym(groupingfactor)  ) ),
                        jitterParams
                    ) )
    }

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        #aes_string( x = xfactor, color = groupingfactor )
        aes( x = !!mysym(xfactor), color = !!mysym(groupingfactor) )
    ) + 
    # the jitters 
    do_jitters +
    # the points 
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .5), 
            size=3,
            #mapping = aes_string(group = groupingfactor, y = "center" ) ),
            mapping = aes(group = !!mysym(groupingfactor), y = center ) ),
        pointParams
    )) + 
    # the error bars; define ymin, ymax only in errorbar
    do.call(geom_superberrorbar, modifyList(
        list(position = position_dodge(.5), width = 0.1, linewidth = 0.75,
            #mapping = aes_string(group = groupingfactor, ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
            mapping = aes(group = !!mysym(groupingfactor), ymin = center + lowerwidth, ymax = center + upperwidth) ),
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
#'     this modified geom_violin has additional options "direction"/"antagonize" and "push".
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with jittered points and violins for the overall distribution
#' superb(
#'    len ~ dose + supp,
#'    ToothGrowth, 
#'    plotStyle = "pointjitterviolin" 
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
#' #superbPlot.pointjitterviolin(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
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
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # rename column "DV" as "center"
    rawdata$center <- rawdata$DV

    # find ... (for antagonize = TRUE option)
    rawdataB     <- rawdata
    rawdataB$dir <- rep(0, dim(rawdataB)[1])

    # remove antagonize option if present
    temp <- FALSE
    if (exists("antagonize", where = violinParams)){
        temp = violinParams$antagonize
        violinParams$antagonize = NULL
    } else if (exists("direction", where = violinParams)) {
        rawdataB$dir <- violinParams$direction
    } 
    # add the "dir" column with alternating violin directions
    if (temp) {
      # alternate the directions
      tt = unique(rawdataB[[xfactor]])
      rawdataB$dir <- 2*(as.numeric(rawdataB[[xfactor]]) %/% 2) -1
    }

    # determining the type of jitter based on the presence or not of a groupingfac
    if (is.null(groupingfactor)) {
        do_jitters = do.call(geom_jitter, modifyList(
                        list(data = rawdata, alpha = 0.2, width = 0.2, height = 0.0,
                             mapping = aes( y = center ) ),
                        jitterParams
                    ) )
        do_violins = do.call( geom_flat_violin, modifyList(
                        list(data     = rawdataB,
                             mapping  = aes( y = center, direction = dir ), 
                             scale    = "area", trim = FALSE, alpha = 0.25),
                        violinParams
                    ) )
    } else {
        do_jitters = do.call(geom_point, modifyList(
                        list(data = rawdata , alpha = 0.2,
                            position = position_jitterdodge(jitter.width=0.1 , dodge.width=.75 ),
                            mapping = aes(y = center, group = !!mysym(groupingfactor)  ) ),
                        jitterParams
                    ) )
        do_violins = do.call( geom_flat_violin, modifyList(
                        list(data    = rawdataB, 
                             position= position_dodge(0.75), #"dodge",
                             mapping = aes( y = center, fill = !!mysym(groupingfactor), direction = dir ), 
                             scale   = "area", trim = FALSE, alpha = 0.25),
                        violinParams
                    ) )
    }
    
    # let's do the plot!
    plot <- ggplot(data    = summarydata, 
                   #mapping = aes_string(x = xfactor, colour = groupingfactor )
                   mapping = aes(x = !!mysym(xfactor), colour = !!mysym(groupingfactor) )
        ) +
        # violins in the back
        do_violins +
        # jitters second
        do_jitters +
        # and finally the points and the error bars
        do.call( geom_point, modifyList(
            list(
                #mapping = aes_string(group = groupingfactor, y = "center"), 
                mapping = aes(group = !!mysym(groupingfactor), y = center), 
                size = 3, position = position_dodge(.75) ),
            pointParams) ) +
        do.call( geom_superberrorbar, modifyList(
            list(
                #mapping = aes_string(group = groupingfactor, ymin = "center+lowerwidth", ymax = "center+upperwidth"), 
                mapping = aes(group = !!mysym(groupingfactor), ymin = center+lowerwidth, ymax = center+upperwidth), 
                position = position_dodge(.75), width = 0.1, linewidth = .75),
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
#' @param pointParams (optional) list of graphic directives that are sent to the geom_point layer
#' @param datapointParams (optional) list of graphic directives that are sent to the geom_point layer of the individual lines
#' @param lineParams (optional) list of graphic directives that are sent to the geom_bar layer;
#'    the parameter colorize=TRUE with use a distinct color for decreasing segments of line
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with points and individual lines for each subject's scores
#'
#' # we take the Orange built-in data.frame but shorten the names...
#' names(Orange) <- c("Tree","age","circ")

#' # Makes the plot:
#'  superb( circ ~ age | Tree, 
#'    Orange, 
#'    adjustments = list(purpose = "difference", decorrelation = "none"),
#'    plotStyle= "pointindividualline"
#'  )
#' 
#' # if you extract the data, you can 
#' # run this layout directly
#' #processedData <- superb( circ ~ age | Tree, 
#' #  Orange,
#' #  adjustments = list(purpose = "difference", decorrelation = "none"),
#' #)
#' #
#' #superbPlot.pointindividualline(processedData$summaryStatistic,
#' #   "age",
#' #   NULL,
#' #   ".~.",
#' #   processedData$rawData)
#' 
#' @export superbPlot.pointindividualline
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
    datapointParams = list(), 
    pointParams     = list(), 
    lineParams      = list(),  
    errorbarParams  = list(),
    facetParams     = list() 
) {
    runDebug("pointindividualline", "Entering superbPlot.pointindividualline", 
        c("xfactor2", "groupingfactor2", "addfactors2","pointParams2","lineParams2","errorbarParams2"), list(xfactor, groupingfactor, addfactors, pointParams, lineParams, errorbarParams))
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # rename column "DV" as "center"
    rawdata$center <- rawdata$DV

    # find which segments are increasing (for colorize=TRUE option)
    rawdataB <- rawdata
    if (exists("id", where = rawdata)) {
        # indicate if data are increasing or decreasing
        rawdataB          <- rawdata[order(rawdata$id),]
        rawdataB$ypost    <- c(with(rawdataB, embed(center,2)[,1]),0)
        rawdataB$increase <- factor(rawdataB$ypost > rawdataB$center)
    } else {
        #print("there is no id column")
    }

    # remove colorize option if present
    temp <- FALSE
    if (exists("colorize", where = lineParams)){
        temp <- lineParams$colorize
        lineParams$colorize <- NULL
    }

    # compute individual lines
    if (temp) {
        dolines <- do.call(geom_line, modifyList(
            list(data = rawdataB,
                linewidth=0.2, alpha = 0.25,
                mapping = aes( y = center, group = id, color = increase ) ),
            lineParams
        ))
    } else {
        dolines <- do.call(geom_line, modifyList(
            list(data = rawdata,
                linewidth=0.2, alpha = 0.25,
                mapping = aes( y = center, group = id ) ),
            lineParams
        ))
    }

    # let's do the plot!
    plot <- ggplot(
        data = summarydata, 
        aes(
            x      = !!mysym(xfactor),   
            y      = center, 
            colour = !!mysym(groupingfactor)
    )) + 
    # the individual lines 
    dolines +
    # the individual points 
    do.call(geom_point, modifyList(
        list(data = rawdata, alpha = 0.25,
            mapping = aes(group = id) ),
        datapointParams
    )) + 
    # the error bars; define ymin, ymax only in errorbar
    do.call(geom_superberrorbar, modifyList(
        list(position = position_dodge(.5), width = 0.1, linewidth = 0.75,
            mapping = aes(group = !!mysym(groupingfactor), ymin = center + lowerwidth, ymax = center + upperwidth) ),
        errorbarParams
    )) + 
    # the summary statistics 
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .5), 
            size=3,
            mapping = aes(group = !!mysym(groupingfactor)) ),
        pointParams
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
