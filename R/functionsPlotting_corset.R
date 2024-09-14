######################################################
######################################################
##  One advanced plot layout: corset
######################################################
######################################################


######################################################################################
#' @name superbPlot.corset
#'
#' @title superbPlot 'corset' layout
#'
#' @md
#'
#' @description superbPlot comes with a few built-in templates for making the final plots.
#' The corset plot is specifically devised for 2-repeated-measure design: it merges
#' the "pointindividualline" layout with a raincloud layout \insertCite{kb21}{superb}.
#' All layout produces ggplot objects that can be further customized. Additionally, it is
#' possible to create custom-make templates (see vignette 5). The functions, to be
#' "superbPlot-compatible", must have these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors;
#' @param lineParams (optional) list of graphic directives that are sent to the geom_line layer;
#' @param pointParams (optional) list of graphic directives that are sent to the geom_bar layer;
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer;
#' @param jitterParams (optional) list of graphic directives that are sent to the geom_jitter layer;
#' @param violinParams (optional) list of graphic directives that are sent to the geom_boxplot layer;
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer;
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete).
#'
#' @return a ggplot object
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # We first generate randomly a 2-measurement dataset with 50 participants and a large effect
#' dta <- GRD(SubjectsPerGroup = 50, WSFactors = "moment(2)", Effects = list("moment"=slope(3)))
#'
#' # This will make a basic corset plot 
#' superbPlot(dta, 
#'    WSFactors = "moment(2)", variables = c("DV.1","DV.2"),
#'    plotStyle = "corset" 
#' )
#'
#' # This will color the increasing and decreasing individuals
#' superbPlot(dta, 
#'    WSFactors = "moment(2)", variables = c("DV.1","DV.2"),
#'    plotStyle = "corset",
#'    lineParams = list(colorize=TRUE) 
#' )
#'
#' # This layout has similarities with the "pointindividualline" layout
#' superbPlot(dta, 
#'    WSFactors = "moment(2)", variables = c("DV.1","DV.2"),
#'    plotStyle = "pointindividualline" 
#' )
#'
#' # if you extract the data with superbData, you can 
#' # run this layout directly
#' processedData <- superbData(dta, 
#'    WSFactors = "moment(2)", variables = c("DV.1","DV.2")
#' )
#' 
#' superbPlot.corset(processedData$summaryStatistic, 
#'    "moment", NULL, ".~.", 
#'    processedData$rawData, 
#'    lineParams = list(colorize=TRUE) )
#'
#' 
#' @export superbPlot.corset
#'
######################################################################################

superbPlot.corset <- function(
    summarydata,               # a summary result data.frame
    xfactor,                   # the factor on the horizontal axis  
    groupingfactor,            # the factor for multiple lines/bars within the plot
    addfactors,                # the factor(s) to make multiple panels
    # what follows is unused and optional
    rawdata        = NULL,     # unused
    # what follows are optional
    lineParams      = list(),
    pointParams     = list(), 
    errorbarParams  = list(),
    jitterParams    = list(),
    violinParams    = list(),
    facetParams     = list(),
    xAsFactor       = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("point", "Entering superbPlot.corset", c("xfactor2", "groupingfactor2", "addfactors2"), list(xfactor, groupingfactor, addfactors))
    mysym <- function(x) { if(is.character(x)) sym(x) else x }

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] = as.numeric(summarydata[[xfactor]])

    # rename column "DV" as "center"
    rawdata$center <- rawdata$DV

    # find which segments are increasing (for colorize=TRUE option)
    rawdataB <- rawdata
    if (exists("id", where = rawdata)) {
        # indicate if data are increasing or decreasing
        rawdataB          <- rawdata[order(rawdata$id),]
        rawdataB$ypost    <- c(with(rawdataB, embed(center,2)[,1]),0)
        rawdataB$increase <- factor(rawdataB$ypost > rawdataB$center)
    }

    # remove colorize option if present
    temp <- FALSE
    if (exists("colorize", where = lineParams)){
        temp <- lineParams$colorize
        lineParams$colorize <- NULL
    }

    # set direction (for antagonize = TRUE option)
    rawdataB$dir <- 2*(as.numeric(rawdataB[[xfactor]]) %/% 2) -1


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


    # determining the type of jitter based on the presence or not of a groupingfac
    if (is.null(groupingfactor)) {
        do_jitters = do.call(geom_jitter, modifyList(
                        list(data = rawdata, alpha = 0.2, width = 0.025, height = 0.0,
                             mapping = aes( y = center ) ),
                        jitterParams
                    ) )
        do_violins = do.call( geom_flat_violin, modifyList(
                        list(data     = rawdataB,
                             mapping  = aes( y = center, direction = dir ), 
                             push     = 0.1, width = 0.5,
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
                        list(data     = rawdataB, 
                             position = position_dodge(0.75), #"dodge",
                             mapping  = aes( y = center, fill = !!mysym(groupingfactor), direction = dir ), 
                             push     = 0.1, width = 0.5,
                             scale    = "area", trim = FALSE, alpha = 0.25),
                        violinParams
                    ) )
    }
 
 

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes(
            x      = !!mysym(xfactor), 
            y      = center, 
            shape  = !!mysym(groupingfactor), 
            fill   = !!mysym(groupingfactor), 
            colour = !!mysym(groupingfactor)
    )) + 
    # violins in the back
    do_violins +
    # jitters second
    do_jitters +
    # the individual lines 
    dolines +
    # the error bars
    do.call(geom_superberrorbar, modifyList(
         list(linewidth = 0.75, width=0.15, position = position_dodge2(width=0.15, padding=0.4),  #width = 0.2, 
            #mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
            mapping = aes(ymin = center + lowerwidth, ymax = center + upperwidth) ),
         errorbarParams
    )) +
    # the points 
    do.call(geom_point, modifyList(
        list(size = 3, position = position_dodge2(width =0.8-0.1/1), 
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

