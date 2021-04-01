######################################################
######################################################
##  Advanced plot layouts: raincloud
######################################################
######################################################




### This script creates an R function to generate raincloud plots, then simulates
### data for plots. If using for your own data, you only need lines 1-80.
### It relies largely on code previously written by David Robinson
### (https://gist.github.com/dgrtwo/eb7750e74997891d7c20)
### and the package ggplot2 by Hadley Wickham.
###
### Code from:
### Allen M, Poggiali D, Whitaker K, Marshall TR, Kievit R. (2018) 
###   RainCloudPlots tutorials and codebase (Version v1.1). Zenodo. 
###   http://doi.org/10.5281/zenodo.3368186


# Check if required packages are installed ----
#packages <- c("ggplot2", "dplyr", "smooth", "Hmisc")
#if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#  install.packages(setdiff(packages, rownames(installed.packages())))
#}

# Load packages ----
#library(ggplot2)

# Defining the geom_flat_violin function ----
# Note (from the original authors): the below code modifies the
# existing github page by removing a parenthesis in line 50

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#### ' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
# ' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      # data %>%
      #  dplyr::group_by(group) %>%
      #  dplyr::mutate(
      #    ymin = min(y),
      #    ymax = max(y),
      #    xmin = x,
      #    xmax = x + width / 2
      #  )
      # Note (from D. Cousineau): did it without the pipe which was generating complaints from CRAN
      dplyr::mutate(dplyr::group_by(data, group),
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },

    draw_group = function(data, panel_scales, coord) {
      # Find the points for the line to go all the way around
      data <- transform(data,
        xminv = x,
        xmaxv = x + violinwidth * (xmax - x)
      )

      # Make sure it's sorted properly to draw the outline
      newdata <- rbind(
        plyr::arrange(transform(data, x = xminv), y),
        plyr::arrange(transform(data, x = xmaxv), -y)
      )

      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1, ])

      ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },

    draw_key = draw_key_polygon,

    default_aes = aes(
      weight = 1, colour = "grey20", fill = "white", size = 0.5,
      alpha = NA, linetype = "solid"
    ),

    required_aes = c("x", "y")
  )


######################################################################################
#' @name superbPlot.raincloud
#'
#' @title superbPlot 'raincloud' layout
#'
#' @md
#'
#' @description The raincloud layout display jittered dots as well as a "cloud" (half of a violin) above them.
#' See Allen, Poggiali, Whitaker, Marshall, & Kievit (2018)
#' The functions has these parameters:
#' 
#' @param summarydata a data.frame with columns "center", "lowerwidth" and "upperwidth" for each level of the factors;
#' @param xfactor a string with the name of the column where the factor going on the horizontal axis is given;
#' @param groupingfactor a string with the name of the column for which the data will be grouped on the plot;
#' @param addfactors a string with up to two additional factors to make the rows and columns panels, in the form "fact1 ~ fact2";
#' @param rawdata always contains "DV" for each participants and each level of the factors
#' @param errorbarParams (optional) list of graphic directives that are sent to the geom_errorbar layer
#' @param jitterParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param violinParams (optional) list of graphic directives that are sent to the geom_bar layer
#' @param facetParams (optional) list of graphic directives that are sent to the facet_grid layer
#' @param xAsFactor (optional) Boolean to indicate if the factor on the horizontal should continuous or discrete (default is discrete)
#'
#' @return a ggplot object
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
    jitterParams   = list(),   # merged into geom_flat_violin
    errorbarParams = list(),   # merged into geom_errorbar
    facetParams    = list(),   # merged into facet_grid
    xAsFactor      = TRUE      # should the horizontal axis be continuous?
) {
    runDebug("raincloud", "Entering superbPlot.raincloud", c("xfactor2", "groupingfactor2", "addfactors2"), list(xfactor, groupingfactor, addfactors))

    # depending on the scale of the x-axis.
    if (!xAsFactor) 
        summarydata[[xfactor]] = as.numeric(summarydata[[xfactor]])

    # determining the type of jitter based on the presence or not of a groupingfac
    if (is.null(groupingfactor)) {
        do_jitters = do.call(geom_jitter, modifyList(
                        list(data = rawdata, alpha = 0.2, width = 0.2, height = 0.0,
                             mapping = aes_string(y = "DV" ) ),
                        jitterParams
                    ) )
    } else {
        do_jitters = do.call( geom_point, modifyList(
                        list(data = rawdata, alpha = 0.5,
                            position = position_jitterdodge(dodge.width = .15, jitter.width=0.15), size = .5,
                            mapping = aes_string(y = "DV") ),
                        jitterParams
                    ))
    }

    # let's do the plot!
    plot <- ggplot(
        summarydata, 
        aes_string(
            x = xfactor, y = "center", 
            fill = groupingfactor, 
            shape = groupingfactor, 
            colour = groupingfactor
    )) +
    # the flat_violin; do.call so that violinParams can be integrated
    do.call( geom_flat_violin, modifyList(
       list(data = rawdata, trim = FALSE, alpha = 0.2,
            position = position_nudge(x = .25, y = 0), size = 0.25,
            mapping = aes_string(y = "DV") ),
        violinParams
    )) +
    # the jittered data
    do_jitters +
    # the summary data
    do.call( geom_point, modifyList(
        list(position = position_dodge(.25),  size = 3.5, color = "black" ),
        jitterParams
    )) +
    # the error bars; do.call so that errorbarParams can be integrated
    do.call( geom_errorbar, modifyList(
        list(position = position_dodge(.25), width = 0.2, size = 1., color = "black",
            mapping = aes_string( ymin = "center + lowerwidth", ymax = "center + upperwidth") ),
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
## end of advanced plot layouts
######################################################
######################################################