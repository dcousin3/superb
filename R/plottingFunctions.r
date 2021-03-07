#' @export superbPlot.bar
#' @export superbPlot.line
#' @export superbPlot.point
#' @export superbPlot.pointjitter
#' @export superbPlot.pointjitterdensity



######################################################
######################################################
##  the basic plot formats: bar, line, point
######################################################
######################################################


superbPlot.bar <- function(data,  
    xvar,           # the factor on the horizontal axis  
    groupingfac,    # the factor for multiple lines/bars within the plot
    addfactors,     # the factor(s) to make multiple panels
    # what follows are optional
    pointParams = list(), 
    errorParams = list(),
    # what follows is unused and optional
    rawdata = NULL
) {
    plot <- ggplot(
        data, 
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
        pointParams
    )) +
    # the error bars; do.call so that errorParams can be integrated
    do.call( geom_errorbar, modifyList(
        list(position = position_dodge(.95), mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
        errorParams
    )) +
    # the panels (rows or both rows and columns, NULL if no facet)
    facet_grid( addfactors)
        
    return(plot)
}


 
######################################################

superbPlot.line <- function(data,  
    xvar,           # the factor on the horizontal axis  
    groupingfac,    # the factor for multiple lines/bars within the plot
    addfactors,     # the factor(s) to make multiple panels
    # what follows are optional
    pointParams = list(), 
    errorParams = list(),
    # what follows is unused and optional
    rawdata = NULL
) {
    plot <- ggplot(
        data, 
        aes_string(
            x = xvar, y = "center", ymin = "center + lowerwidth", ymax = "center + upperwidth", 
            fill = groupingfac, 
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
            mapping = aes_string(group = groupingfac) ),
        pointParams
    )) +
    # the error bars
    do.call(geom_errorbar, modifyList(
        list(position = position_dodge(.15)),
        errorParams
        )
    ) + 
    # the panels (rows or both rows and columns, NULL if no facet)
    facet_grid( addfactors )
        
    return(plot)
}

 
######################################################


superbPlot.point <- function(data,  
    xvar,           # the factor on the horizontal axis  
    groupingfac,    # the factor for multiple lines/bars within the plot
    addfactors,     # the factor(s) to make multiple panels
    # what follows are optional
    pointParams = list(), 
    errorParams = list(),
    # what follows is unused and optional
    rawdata = NULL
) {
    plot <- ggplot(
        data, 
        aes_string(
            x = xvar, y = "center", 
            fill = groupingfac, 
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
         list(position = position_dodge(.15), mapping = aes_string(ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
         errorParams
    )) +
    # the panels (rows or both rows and columns, NULL if no facet)
    facet_grid( addfactors )
    
    return(plot)
}




######################################################
######################################################
## the newer ones
######################################################
######################################################


superbPlot.pointjitter <- function(data,  
    xvar,           # the factor on the horizontal axis  
    groupingfac,    # the factor for multiple lines/bars within the plot
    addfactors,     # the factor(s) to make multiple panels
    # what follows are optional
    pointParams = list(), 
    errorParams = list(),
    # what follows is mandatory
    rawdata
) {
    plot <- ggplot(
        data, 
        aes_string(
            x = xvar, y = "center",  
            fill = groupingfac, 
            shape = groupingfac, 
            colour = groupingfac
    )) + 
    # the points 
    do.call(geom_point, modifyList(
        list(position = position_dodge(width = .15), 
            stat = "identity", size=3,
            mapping = aes_string(group = groupingfac) ),
        pointParams
    )) + 
    # the error bars; define ymin, ymax only in errorbar
    do.call(geom_errorbar, modifyList(
         list(position = position_dodge(.15), mapping = aes_string(group = groupingfac, ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
         errorParams
    )) + 
    # the jitters 
    do.call(geom_jitter, modifyList(
        list(data = rawdata, width = 0.1,
             mapping = aes_string(group = groupingfac, y = "DV") ),
        pointParams
    )) +
    # the panels (rows or both rows and columns, NULL if no facet)
    facet_grid( addfactors )

    return(plot)
}


 
######################################################
myseq <- function(lo,hi,step,mul){
    if (mul<=1) { mean(c(lo,hi))          }
    else        { seq(lo,hi,step/(mul-1) ) }
}

superbPlot.pointjitterdensity <- function(data,  
    xvar,           # the factor on the horizontal axis  
    groupingfac,    # the factor for multiple lines/bars within the plot
    addfactors,     # the factor(s) to make multiple panels
    # what follows are optional
    pointParams = list(), 
    errorParams = list(),
    # what follows is mandatory
    rawdata
) {
    require("ggsci")

    # needs to compute the rep based on number of either conditions
    n1 <- length(unique(data[[xvar]]))
    n2 <- ifelse(!is.null(groupingfac),length(unique(data[[groupingfac]])),0)
    s  <- rep(myseq(-1/20,+1/20,2*1/20,n2),n1)

    # adjusting the facet_grid for this plot only
print(paste("here adjust facet_grid",groupingfac, addfactors))
    fctrs <- unlist(strsplit(addfactors, "~"))
    fctrs <- fcts[which(fcts!=".")]
    if(length(fctrs)==2) {warning(paste("There are four factors but only three can be used with this plot. Dropping ", fctrs[2]))}
    addfactors=paste(ifelse(is.na(fctrs[1]),".",fctrs[1]),xvar, sep="~")
print(addfactors)

    ggplot(data, aes_string(color = groupingfac, group = groupingfac, fill = groupingfac, shape = groupingfac)) +
      do.call( geom_point, modifyList(
            list(mapping = aes_string(y = -1/2+s, x = "center"), size = 4),
            pointParams))+
      do.call( geom_errorbarh, modifyList(
            list(stat = "identity", mapping = aes_string(y = -1/2+s, xmin = "center+lowerwidth", xmax = "center+upperwidth"), height = 0.1),
            errorParams))+
      geom_jitter(data = rawdata, mapping = aes_string(x = "DV", y = "-1/4"), height = 0.1) +
      geom_density(data = rawdata, mapping = aes_string(group = groupingfac, x = "DV"), alpha = .5) +
      scale_y_continuous(expand = c(0, 0.02)) +
      coord_flip() +
      theme_light(base_size = 14) + 
      scale_colour_jco() +
      scale_fill_jco() +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank() ) +
      labs(color = NULL, fill = NULL) +
      facet_grid( addfactors ) 
}



