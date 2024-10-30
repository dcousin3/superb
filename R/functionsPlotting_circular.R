##################################################################
##################################################################
##  Circular plot layouts: circularpoint, circularline,
##                         circularpointjitter, circularpointlinejitter 
##                         circularlineBand, 
##################################################################
##################################################################

##################################################################
## common to all
##################################################################

# Based on Erwan Le Pennec: "From Parallel Plot to Radar Plot"
# http://www.cmap.polytechnique.fr/~lepennec/en/post/radar/radarandparallelplots/
# Also see https://stackoverflow.com/q/42562128/5181513
coord_superbradar <- function (theta = "x", start = 0, direction = 1) {
    if (direction == 0) stop("coord_radar:: Somehow, the direction parameter is 0. Exiting...")
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") "y" else "x"
    ggproto("CoordRadar", CoordPolar, theta = theta, r = r, 
            start = start, 
            direction = sign(direction),
            is_linear = function(coord) TRUE,
            clip = "off"
    )
}

# Loop the lines by adding a value for 0 that is the same as last
add_rowmax <- function(df, xcol){
    lastline <- df[ dim(df)[1],]
    lastline[[xcol]] <- 0
    df <- rbind(lastline, df)
    return(df)
}



#1#####################################################################################
#' @name superbPlot.circularpoint
#'
#' @title superbPlot 'circularpoint' layout
#'
#' @md
#'
#' @description superb comes with a few circular layouts for making plots.
#' It produces ggplot objects that can be further customized.  
#' 
#' It has these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param pointParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param radarParams (optional)  list of arguments to the radar coordinates (seel `coord_radial()` ).
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with bars
#' superb(
#'    len ~ dose + supp, 
#'    ToothGrowth, 
#'    plotStyle="circularpoint" 
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
#' #superbPlot.circularpoint(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @export superbPlot.circularpoint
#'
######################################################################################


superbPlot.circularpoint <- function(
    summarydata,
    xfactor,       
    groupingfactor,
    addfactors, 
    rawdata        = NULL,        
    # what follows are optional
    pointParams    = list(), 
    errorbarParams = list(),
    facetParams    = list(),
    radarParams    = list(),
    xAsFactor      = TRUE  
) {
    runDebug("circularpoint", "Entering superbPlot.circularpoint", c("xfactor1", "groupingfactor1", "addfactors1", "params1"), 
        list(xfactor, groupingfactor, addfactors, list(pointParams=pointParams, errorbarParams=errorbarParams))
    )
    # duplicate the dataset and keep their labels if any
    sd    <- summarydata
    lbls  <- unique(summarydata[[xfactor]])

    # Closing the loop
    # a) xfactor MUST be numeric AND contiguous
    sd[[xfactor]] <- factor(sd[[xfactor]], levels = lbls)
    sd[[xfactor]] <- as.numeric(sd[[xfactor]])
    # b) spliting in subgroups if any
    if (!is.null(groupingfactor)) { #two or more factors
        relevantfactors <- paste(groupingfactor, addfactors, sep="~")
        relevantfactors <- gsub("[[:space:]]", "", strsplit(relevantfactors,"~")[[1]]) 
        relevantfactors <- relevantfactors[relevantfactors != "."]
        subdta <- split(sd, sd[,relevantfactors ] )
    } else {# only 1 factor
        subdta <- list(sd)
    }
    tmp    <- lapply(subdta, add_rowmax, xfactor)
    csd    <- do.call(rbind, tmp) # circular summarydata

    morecompact <- position_dodge(0.25)
    plot1 <- superbPlot.point(csd,
            xfactor,
            groupingfactor,
            addfactors,
            rawdata,
            # reduces the dogding
            modifyList( if(!is.null(addfactors)) {list( position = morecompact)}, pointParams ),
            modifyList( if(!is.null(addfactors)) {list( position = morecompact)}, errorbarParams ),
            facetParams,
            xAsFactor
        ) + ylim(0,NA) + 
        scale_x_continuous(
            name = xfactor,
            oob = scales::oob_keep, 
            limits =   c(0, 0.00001+length(lbls) ), 
            n.breaks = length(lbls) +1,
            labels =   c("", levels(lbls) )   # restore labels
        ) + 
        coord_superbradar()

    return(plot1)
}


 


#2#####################################################################################
#' @name superbPlot.circularline
#'
#' @title superbPlot 'circularline' layout
#'
#' @md
#'
#' @description superb comes with a few circular layouts for making plots.
#' It produces ggplot objects that can be further customized.  
#' 
#' @details A few things to note:
#' * You can at any time undo the polar coordinates by using
#' ` + coord_cartesian()`. It is sometimes easier when developping the plots.
#' * Also, if ever you want to modify the scale post-hoc (e.g., to change 
#' the labels of the group), you can, but your `scale_x_continuous` __must
#' absolutely__ contains the two arguments:
#' scale_x_continuous(
#'            oob = scales::oob_keep, 
#'            limits = c(0, 0.00001+ *NUMBER OF CONDITIONS* ), 
#'            # any other argument such as labels = c("",...)
#'        )
#'
#'
#' It has these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param pointParams (optional) list of graphic directives that are sent to the `geom_bar()` layer
#' @param lineParams (optional) list of graphic directives that are sent to the `geom_line()` layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the `geom_superberrorbar()` layer
#' @param facetParams (optional) list of graphic directives that are sent to the `facet_grid()` layer
#' @param radarParams (optional)  list of arguments to the radar coordinates (seel `coord_radial()` ).
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should be continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with lines
#' superb(
#'    len ~ dose + supp, 
#'    ToothGrowth, 
#'    plotStyle="circularline" 
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
#' #superbPlot.circularline(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @export superbPlot.circularline
#'
######################################################################################

superbPlot.circularline <- function(
    summarydata,
    xfactor,       
    groupingfactor,
    addfactors, 
    rawdata        = NULL,        
    # what follows are optional
    pointParams    = list(), 
    lineParams     = list(), 
    errorbarParams = list(),
    facetParams    = list(),
    radarParams    = list(),
    xAsFactor      = TRUE  
) {
    runDebug("circularline", "Entering superbPlot.circularline", c("xfactor1", "groupingfactor1", "addfactors1", "params1"), 
        list(xfactor, groupingfactor, addfactors, list(pointParams=pointParams, lineParams=lineParams, errorbarParams=errorbarParams))
    )
    # duplicate the dataset and keep their labels if any
    sd    <- summarydata
    lbls  <- unique(summarydata[[xfactor]])

    # Closing the loop
    # a) xfactor MUST be numeric AND contiguous
    sd[[xfactor]] <- factor(sd[[xfactor]], levels = lbls)
    sd[[xfactor]] <- as.numeric(sd[[xfactor]])
    # b) spliting in subgroups if any
    if (!is.null(groupingfactor)) { #two or more factors
        relevantfactors <- paste(groupingfactor, addfactors, sep="~")
        relevantfactors <- gsub("[[:space:]]", "", strsplit(relevantfactors,"~")[[1]]) 
        relevantfactors <- relevantfactors[relevantfactors != "."]
        subdta <- split(sd, sd[,relevantfactors ] )
    } else {# only 1 factor
        subdta <- list(sd)
    }
    tmp    <- lapply(subdta, add_rowmax, xfactor)
    csd    <- do.call(rbind, tmp) # circular summarydata

    morecompact <- position_dodge( 0.25 )
    plot1 <- superbPlot.line(csd,
            xfactor,
            groupingfactor,
            addfactors,
            rawdata,
            # reduces the dogding
            modifyList( list( position = morecompact), pointParams ),
            modifyList( list( position = morecompact), lineParams ),
            modifyList( list( position = morecompact), errorbarParams ),
            facetParams,
            xAsFactor
        ) + ylim(0,NA) + 
        scale_x_continuous(
            name = xfactor,
            oob = scales::oob_keep, 
            limits =   c(0, 0.00001+length(lbls) ), 
            n.breaks = length(lbls) +1,
            labels =   c("", levels(lbls) )   # restore labels
        ) + 
        coord_superbradar()

    return(plot1)
}

 

#3#####################################################################################
#' @name superbPlot.circularpointjitter
#'
#' @title superbPlot 'circularpointjitter' layout
#'
#' @md
#'
#' @description superb comes with a few circular layouts for making plots.
#' It produces ggplot objects that can be further customized.  
#' 
#' It has these parameters:
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
#' @param radarParams (optional)  list of arguments to the radar coordinates (seel `coord_radial()` ).
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with points
#' superbPlot(ToothGrowth, 
#'    BSFactors = c("dose","supp"), variables = "len",
#'    plotStyle = "circularpointjitter" 
#' )
#'
#' # if you extract the data with superbData, you can 
#' # run this layout directly
#' #processedData <- superbData(ToothGrowth, 
#' # BSFactors = c("dose","supp"), variables = "len"
#' #)
#' #
#' #superbPlot.circularpointjitter(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @export superbPlot.circularpointjitter
#'
######################################################################################


superbPlot.circularpointjitter <- function(
    summarydata,
    xfactor,       
    groupingfactor,
    addfactors, 
    rawdata        = NULL,        
    # what follows are optional
    pointParams    = list(), 
    jitterParams   = list(), 
    errorbarParams = list(),
    facetParams    = list(),
    radarParams    = list(),
    xAsFactor      = TRUE  
) {
    runDebug("circularpointjitter", "Entering superbPlot.circularpointjitter", c("xfactor1", "groupingfactor1", "addfactors1", "params1"), 
        list(xfactor, groupingfactor, addfactors, list(pointParams=pointParams, jitterParams = jitterParams, errorbarParams=errorbarParams))
    )
    # duplicate the dataset and keep their labels if any
    sd    <- summarydata
    rd    <- rawdata
    lbls  <- unique(summarydata[[xfactor]])

    # Closing the loop
    # a) xfactor MUST be numeric AND contiguous
    sd[[xfactor]] <- factor(sd[[xfactor]], levels = lbls)
    sd[[xfactor]] <- as.numeric(sd[[xfactor]])
    rd[[xfactor]] <- factor(rd[[xfactor]], levels = lbls)
    rd[[xfactor]] <- as.numeric(rd[[xfactor]])
    # b) spliting in subgroups if any
    if (!is.null(groupingfactor)) { #two or more factors
        relevantfactors <- paste(groupingfactor, addfactors, sep="~")
        relevantfactors <- gsub("[[:space:]]", "", strsplit(relevantfactors,"~")[[1]]) 
        relevantfactors <- relevantfactors[relevantfactors != "."]
        subdta <- split(sd, sd[,relevantfactors ] )
    } else {# only 1 factor
        subdta <- list(sd)
    }
    tmp    <- lapply(subdta, add_rowmax, xfactor)
    csd    <- do.call(rbind, tmp) # circular summarydata

#    morecompact <- position_jitterdodge(jitter.width=0.1 , dodge.width=0.25 )
    morecompact <- position_dodge(0.25)
    plot1 <- superbPlot.pointjitter(csd,
            xfactor,
            groupingfactor,
            addfactors,
            rd,
            # reduces the dogding
            modifyList( list( position = morecompact), pointParams ),
            modifyList( list( position = morecompact), jitterParams ),
            modifyList( list( position = morecompact), errorbarParams ),
            facetParams,
            xAsFactor
        ) + ylim(0,NA) + 
        scale_x_continuous(
            name = xfactor,
            oob = scales::oob_keep, 
            limits =   c(0, 0.00001+length(lbls) ), 
            n.breaks = length(lbls) +1,
            labels =   c("", levels(factor(lbls)) )   # restore labels
        ) + 
        do.call( coord_superbradar, modifyList(
            list(),  # default options are empty
            radarParams
        ) )

    return(plot1)
}




#4#####################################################################################
#' @name superbPlot.circularpointlinejitter
#'
#' @title superbPlot 'circularpointlinejitter' layout
#'
#' @md
#'
#' @description superb comes with a few circular layouts for making plots.
#' It produces ggplot objects that can be further customized.  
#' 
#' It has these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param pointParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param lineParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param jitterParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_superberrorbar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param radarParams (optional)  list of arguments to the radar coordinates (seel `coord_radial()` ).
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with points
#' superbPlot(ToothGrowth, 
#'    BSFactors = c("dose","supp"), variables = "len",
#'    plotStyle = "circularpointlinejitter" 
#' )
#'
#' # if you extract the data with superbData, you can 
#' # run this layout directly
#' #processedData <- superbData(ToothGrowth, 
#' # BSFactors = c("dose","supp"), variables = "len"
#' #)
#' #
#' #superbPlot.circularpointlinejitter(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @export superbPlot.circularpointlinejitter
#'
######################################################################################


superbPlot.circularpointlinejitter <- function(
    summarydata,
    xfactor,       
    groupingfactor,
    addfactors, 
    rawdata        = NULL,        
    # what follows are optional
    pointParams    = list(), 
    lineParams     = list(), 
    jitterParams   = list(), 
    errorbarParams = list(),
    facetParams    = list(),
    radarParams    = list(),
    xAsFactor      = TRUE  
) {
    runDebug("circularpointlinejitter", "Entering superbPlot.circularpointlinejitter", c("xfactor1", "groupingfactor1", "addfactors1", "params1"), 
        list(xfactor, groupingfactor, addfactors, list(pointParams=pointParams, lineParams=lineParams, errorbarParams=errorbarParams))
    )
    # duplicate the dataset and keep their labels if any
    sd    <- summarydata
    rd    <- rawdata
    lbls  <- unique(summarydata[[xfactor]])

    # Closing the loop
    # a) xfactor MUST be numeric AND contiguous
    sd[[xfactor]] <- factor(sd[[xfactor]], levels = lbls)
    sd[[xfactor]] <- as.numeric(sd[[xfactor]])
    rd[[xfactor]] <- factor(rd[[xfactor]], levels = lbls)
    rd[[xfactor]] <- as.numeric(rd[[xfactor]])
    # b) spliting in subgroups if any
    if (!is.null(groupingfactor)) { #two or more factors
        relevantfactors <- paste(groupingfactor, addfactors, sep="~")
        relevantfactors <- gsub("[[:space:]]", "", strsplit(relevantfactors,"~")[[1]]) 
        relevantfactors <- relevantfactors[relevantfactors != "."]
        subdta <- split(sd, sd[,relevantfactors ] )
    } else {# only 1 factor
        subdta <- list(sd)
    }
    tmp    <- lapply(subdta, add_rowmax, xfactor)
    csd    <- do.call(rbind, tmp) # circular summarydata

#    morecompact <- position_jitterdodge(jitter.width=0.1 , dodge.width=0.25 )
    morecompact <- position_dodge(0.25)
    plot1 <- superbPlot.pointlinejitter(csd,
            xfactor,
            groupingfactor,
            addfactors,
            rd,
            # reduces the dogding (first argument has priority)
            modifyList( list( position = morecompact), pointParams ),
            modifyList( list( position = morecompact), lineParams ),
            modifyList( list( position = morecompact), jitterParams ),
            modifyList( list( position = morecompact), errorbarParams ),
            facetParams,
            xAsFactor
        ) + ylim(0,NA) + 
        scale_x_continuous(
            name = xfactor,
            oob = scales::oob_keep, 
            limits =   c(0, 0.00001+length(lbls) ), 
            n.breaks = length(lbls) +1,
            labels =   c("", levels(factor(lbls)) )   # restore labels
        ) + 
        do.call( coord_superbradar, modifyList(
            list(),  # default options are empty
            radarParams
        ) )

    return(plot1)
}



 

#5#####################################################################################
#' @name superbPlot.circularlineBand
#'
#' @title superbPlot 'circularlineBand' layout
#'
#' @md
#'
#' @description superb comes with a few circular layouts for making plots.
#' It produces ggplot objects that can be further customized.  
#' 
#' It has these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param pointParams (optional) list of graphic directives that are sent to the `geom_bar()` layer
#' @param lineParams (optional) list of graphic directives that are sent to the `geom_line()` layer
#' @param facetParams (optional) list of graphic directives that are sent to the `facet_grid()` layer
#' @param errorbandParams (optional) list of graphic directives that are sent to the `geom_ribbon()` layer
#' @param radarParams (optional)  list of arguments to the radar coordinates (seel `coord_radial()` ).
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
#'
#' @examples
#' # This will make a plot with points
#' superbPlot(ToothGrowth, 
#'    BSFactors = c("dose","supp"), variables = "len",
#'    plotStyle = "circularlineBand" 
#' )
#'
#' # if you extract the data with superbData, you can 
#' # run this layout directly
#' #processedData <- superbData(ToothGrowth, 
#' # BSFactors = c("dose","supp"), variables = "len"
#' #)
#' #
#' #superbPlot.circularlineBand(processedData$summaryStatistic,
#' #   "dose",
#' #   "supp",
#' #   ".~.",
#' #   processedData$rawData)
#'
#' @export superbPlot.circularlineBand
#'
######################################################################################


superbPlot.circularlineBand <- function(
    summarydata,
    xfactor,       
    groupingfactor,
    addfactors, 
    rawdata        = NULL,        
    # what follows are optional
    pointParams    = list(), 
    lineParams     = list(), 
    errorbandParams = list(),
    facetParams    = list(),
    radarParams    = list(),
    xAsFactor      = TRUE  
) {
    runDebug("circularlineBand", "Entering superbPlot.circularlineBand", c("xfactor1", "groupingfactor1", "addfactors1", "params1"), 
        list(xfactor, groupingfactor, addfactors, list(pointParams=pointParams, lineParams=lineParams, errorbandParams=errorbandParams))
    )
    # duplicate the dataset and keep their labels if any
    sd    <- summarydata
    lbls  <- unique(summarydata[[xfactor]])

    # Closing the loop
    # a) xfactor MUST be numeric AND contiguous
    sd[[xfactor]] <- factor(sd[[xfactor]], levels = lbls)
    sd[[xfactor]] <- as.numeric(sd[[xfactor]])
    # b) spliting in subgroups if any
    if (!is.null(groupingfactor)) { #two or more factors
        relevantfactors <- paste(groupingfactor, addfactors, sep="~")
        relevantfactors <- gsub("[[:space:]]", "", strsplit(relevantfactors,"~")[[1]]) 
        relevantfactors <- relevantfactors[relevantfactors != "."]
        subdta <- split(sd, sd[,relevantfactors ] )
    } else {# only 1 factor
        subdta <- list(sd)
    }
    tmp    <- lapply(subdta, add_rowmax, xfactor)
    csd    <- do.call(rbind, tmp) # circular summarydata

#    morecompact <- position_jitterdodge(jitter.width=0.1 , dodge.width=0.25 )
    morecompact <- position_dodge(0.25)

    plot1 <- superbPlot.lineBand(csd,
            xfactor,
            groupingfactor,
            addfactors,
            rawdata,
            # reduces the dogding
            modifyList( list( position = morecompact), pointParams ),
            modifyList( list( position = morecompact), lineParams ),
            modifyList( list( position = morecompact), errorbandParams ),
            facetParams,
            xAsFactor
        ) + ylim(0,NA) + 
        scale_x_continuous(
            name = xfactor,
            oob = scales::oob_keep, 
            limits =   c(0, 0.00001+length(lbls) ), 
            n.breaks = length(lbls) +1,
            labels =   c("", levels(lbls) )   # restore labels
        ) + 
        # coord_superbradar
        do.call( coord_superbradar, modifyList(
            list(),  # default options are empty
            radarParams
        ) )

    return(plot1)
}



######################################################
######################################################
## end of the cicular layouts
######################################################
######################################################
