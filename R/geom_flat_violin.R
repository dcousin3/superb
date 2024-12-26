
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


######################################################################################
#' @name geom_flat_violin
#'
#' @title geom_flat_violin for expanded density displays 
#'
#' @md
#'
#' @description ``geom_flat_violin()`` is a geom for ggplots; it is based on 
#'      the original script to create  raincloud plots.
#' It relies largely on code previously written by David Robinson
#' (https://gist.github.com/dgrtwo/eb7750e74997891d7c20)
#' and the package ggplot2 by Hadley Wickham.
#'
#' Code from \insertCite{allen2019;textual}{superb}
#'
#' It is expanded in tow different ways. First, it is possible to
#' decide the direction of the violin using the `direction` argument    
#' (values are 0 = symmetrical; 1 = extending to the right; -1 = extending
#' to the left); the last two cases are "half"-violin. The second
#' argument is `push` which pushed the violin away from the median line (default = 0).
#'      
#'
#' @param mapping (as usual) see `geom_violin()`
#' @param data  (as usual) see `geom_violin()`
#' @param stat (as usual) see `geom_violin()`
#' @param position (as usual) see `geom_violin()`
#' @param trim If `TRUE` (default), trim the tails of the violins
#'    to the range of the data. If `FALSE`, don't trim the tails.
#' @param scale if "area" (default), all violins have the same area 
#'    (before trimming the tails). If "count", areas are scaled 
#'    proportionally to the number of observations. If "width", all
#'    violins have the same maximum width.
#' @param ... all additional parameters are sent to the underlying `geom_path()`. It includes
#'    * ``direction`` (NEW) either -1,0, or +1;
#'    * ``push`` (NEW) a positive number.
#'    * ``na.rm`` (as usual) see `geom_violin()`
#'    * ``orientation`` (as usual) see `geom_violin()`
#' @param show.legend (as usual) see `geom_violin()`
#' @param inherit.aes (as usual) see `geom_violin()`
#'
#' @return a layer containing violins in a ggplot object
#'
#' @examples
#'
#' library(superb) # to import the geom_flat_violin
#' library(ggplot2)
#'
#' # let's have a fake data frame with three groups:
#' dta <- dta <- GRD( SubjectsPerGroup = 20,
#'     BSFactors = "Vacations(yes,no,maybe)",
#'     RenameDV = "tiredeness",
#'     Population = list(mean=75, stddev=15),
#'     Effects = list("Vacations" = custom(-20,+20,+10))
#' )
#'
#' # The most basic plot = a regular error bar
#' superb( tiredeness ~ Vacations, dta)
#'
#' # an example with default violins
#' superb( tiredeness ~ Vacations, dta, 
#'     plotLayout = "pointjitterviolin" )
#'
#' # the same with some ornementations:  
#' superb( tiredeness ~ Vacations, dta, 
#'     plotLayout = "pointjitterviolin",
#'     violinParams = list(direction = 1, push = 0.2, fill="green", alpha = 0.3)
#' ) + theme_bw() + coord_flip() + ylab("Tiredeness")
#'
#' # This new geom is integrated inside superb() so that you can use it 
#' # directly. Let's see examples:
#' 
#' # show the violins only
#' ggplot(dta, aes(y = tiredeness, x = Vacations ) ) +
#'    geom_flat_violin()
#' 
#' # change the parameters of the violins
#' ggplot(dta, aes(y = tiredeness, x = Vacations ) ) +
#'    geom_flat_violin( fill = "green")
#'
#' # all the arguments manipulated
#' ggplot(dta, aes(y = tiredeness, x = Vacations ) ) +
#'     geom_flat_violin( fill = "green", direction = 1, push =0.)
#'
#' # using direction within aes
#' dta <- transform(dta, dir = ifelse(Vacations == "no", 1, -1))
#' 
#' ggplot(dta, aes(y = tiredeness, x = Vacations, direction = dir ) ) +
#'     geom_flat_violin( fill = "green", push =0.)
#'
#' @references
#' \insertAllCited{}
#'
#' @export geom_flat_violin
#'
######################################################################################


# Defining the geom_flat_violin function ----
# Note (from the original authors): the below code modifies the
# existing github page by removing a parenthesis in line 50

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

GeomFlatViolin <-
    ggproto("GeomFlatViolin", Geom,
        setup_data = function(data, params) {
            data$width <- data$width %||%
                params$width %||% (resolution(data$x, FALSE) * 0.9)

            # new addition: D. Cousineau, 2024.08.03
            data$push      <- abs(data$push %||% params$push %||% 0)
            data$direction <- data$direction %||% params$direction %||% 0

            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            # Note (from D. Cousineau): did it without the pipes which were generating complaints from CRAN
            dplyr::mutate(dplyr::group_by(data, group),
                ymin = min(y),
                ymax = max(y),    
                xmin = x - width / 2,
                xmax = x + width / 2
            )
        },

        draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data,
                        xmaxv = abs((1+direction)/2)*(x+push) + abs((1-direction)/2)*(x - violinwidth * (x-xmin)-push),
                        xminv = abs((1+direction)/2)*(x + violinwidth * (x - xmin)+push) + abs((1-direction)/2)*(x-push)
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
                    weight = 1, colour = "grey20", fill = "white", linewidth = 0.5,
                    alpha = NA, linetype = "solid",
                    # new items
                    direction = 1, push = 0),

        required_aes = c("x", "y")
  )

