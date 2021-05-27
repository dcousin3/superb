######################################################################################
#' @title geom_superberrorbar for expanded error bar displays 
#'
#' @description ``geom_superberrorbar()`` is a geom for ggplots; it is based on 
#'      the original geom_errorbar (and is totally compatible with it) but
#'      expands this geom in three different ways. First, it is possible to
#'      decide whether the error bar tips are unidirectional, pointing to 
#'      the "left" or to the "right" or if they go in "both" directions.
#'      Second, it is possible to "double" or "triple" the horizontal marks
#'      at the extremities of the error bar, with a "tipgap" of your liking.
#'      Third, a new characteristici is vcolour to set a different colour for the
#'      vertical part of the error bar. The colour can also be "NA" to have it invisible.
#'
#' @md
#'
#' @param mapping (as usual) see geom_errorbar
#' @param data  (as usual) see geom_errorbar
#' @param stat (as usual) see geom_errorbar
#' @param position (as usual) see geom_errorbar
#' @param direction (NEW) "left", "right" or "both" (Default is "both")
#' @param tipformat (NEW) "single", "double" or "triple" to add additional 
#'        marker lines to the tips (default is "single")
#' @param tipgap (NEW) The spacing between the markers when "double" or "triple" is used (default 0.1)
#' @param ... all additional parameters are sent to the underlying geom_path
#' @param na.rm (as usual) see geom_errorbar
#' @param orientation (as usual) see geom_errorbar
#' @param show.legend (as usual) see geom_errorbar
#' @param inherit.aes (as usual) see geom_errorbar
#'
#' @return a layer containing error bars in a ggplot object
#'
#' @examples
#' library(superb) # to import the geom_superberrorbar
#' library(ggplot2)
#'
#' # let's have a fake data frame
#' dta <- data.frame(grp = c(1,2,3), center=c(1,2,3), width = c(1,1,1.5) )
#'
#' # an example with none of the new features = a regular error bar
#' ggplot(dta, aes_string(ymin="center-width", ymax="center+width", x = "grp" ) ) +
#'   geom_superberrorbar()
#'
#' # an example with left-pointing error bars
#' ggplot(dta, aes_string(ymin="center-width", ymax="center+width", x = "grp" ) ) +
#'   geom_superberrorbar(direction="left", width = 0.1)
#'
#' # an example with doubled-tipped error bar and the default tipgap
#' ggplot(dta, aes_string(ymin="center-width", ymax="center+width", x = "grp" ) ) +
#'   geom_superberrorbar(tipformat = "double", width = 0.1)
#'
#' # an example with left-pointing tripled-tip error bars with small gaps
#' ggplot(dta, aes_string(ymin="center-width", ymax="center+width", x = "grp" ) ) +
#'   geom_superberrorbar(tipformat = "triple", width= 0.1, tipgap = 0.04, direction = "left")
#' 
#' # a final example with two-coloured, left-pointing tripled-tip error bars with small gaps
#' ggplot(dta, aes_string(ymin="center-width", ymax="center+width", x = "grp" ) ) +
#'   geom_superberrorbar(tipformat = "triple", width= 0.1, tipgap = 0.04, direction = "left",
#'            colour = "black", vcolour = "NA")
#' 
#' # This new geom is integrated inside superbPlot() so that you can vary the 
#' # error bar shapes. Let's see examples:
#' 
#' # using GRD to generate random data with a moderate effect
#' options(superb.feedback = 'none') # shut down 'warnings' and 'design' interpretation messages
#' test <- GRD(WSFactors = "Moment(5)", 
#'             Effects = list("Moment" = extent(10) ),
#'             Population = list(mean = 100, stddev = 25, rho = 0.8) ) 
#' 
#' ornate = list(
#'         labs(title =paste("(left)      95% confidence intervals",
#'                         "\n(right)    99% confidence intervals",
#'                         "\n(center) 99.9% confidence intervals")),
#'         xlab("Moment"), ylab("Score"),
#'         coord_cartesian( ylim = c(85,110) )
#' )
#' 
#' plt1 <- superbPlot(test, 
#'             WSFactors = "Moment(5)",  
#'             variables = c("DV.1","DV.2","DV.3","DV.4","DV.5"), 
#'             adjustments=list(purpose = "difference", decorrelation = "CA"), 
#'             errorbarParams = list(direction = "left", 
#'                                   width = 0.2, position = position_nudge(-0.05) ),
#'             gamma     = 0.95,
#'             plotStyle = "line" ) + ornate
#' plt2 <- superbPlot(test, 
#'             WSFactors = "Moment(5)",  
#'             variables = c("DV.1","DV.2","DV.3","DV.4","DV.5"), 
#'             adjustments=list(purpose = "difference", decorrelation = "CA"), 
#'             errorbarParams = list(direction = "right", tipgap = 0.5, tipformat = "double", 
#'                                   width = 0.2, position = position_nudge(+0.05) ),
#'             gamma     = 0.99,
#'             plotStyle = "line" ) + ornate 
#' plt3 <- superbPlot(test, 
#'             WSFactors = "Moment(5)",  
#'             variables = c("DV.1","DV.2","DV.3","DV.4","DV.5"), 
#'             adjustments=list(purpose = "difference", decorrelation = "CA"), 
#'             errorbarParams = list(direction = "both", tipformat = "single", 
#'                                   width = 0.2, position = position_nudge(0) ),
#'             gamma     = 0.999,
#'             plotStyle = "line" ) + ornate 
#' 
#' # transform the ggplots into "grob" so that they can be manipulated
#' plt1 <- ggplotGrob(plt1)
#' plt2 <- ggplotGrob(plt2 + makeTransparent() )
#' plt3 <- ggplotGrob(plt3 + makeTransparent() )
#' 
#' # put the grobs onto an empty ggplot 
#' ggplot() + 
#'     annotation_custom(grob=plt1) + 
#'     annotation_custom(grob=plt2) + 
#'     annotation_custom(grob=plt3)
#'
#' @export geom_superberrorbar
######################################################################################


geom_superberrorbar <- function(
    mapping     = NULL, 
    data        = NULL,
    stat        = "identity", 
    position    = "identity",
    direction   = "both",    # new: "left", "right", "both"
    tipformat   = "single",  # new: "single", "double", "triple"
    tipgap      = 0.1,       # new: spacing between the double tips 
    ...,
    na.rm       = FALSE,
    orientation = NA,
    show.legend = NA,
    inherit.aes = TRUE
) {
    layer(
        data        = data,
        mapping     = mapping,
        stat        = stat,
        geom        = GeomsuperbErrorbar,
        position    = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params      = list(
            na.rm       = na.rm,
            orientation = orientation,
            direction   = direction,
            tipformat   = tipformat,
            tipgap      = tipgap,
            ...
        )
    )
}

# from rlang
`%||%` <- function(x, y) {if (is.null(x)) y else x }

GeomsuperbErrorbar <- ggproto("GeomsuperbErrorbar", Geom,
    default_aes = aes( # the parameters
        colour    = "black", 
        vcolour   = NULL, 
        size      = 0.5, 
        linetype  = 1, 
        width     = 0.5,
        alpha     = NA 
    ),
    draw_key = draw_key_path,
    required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),
    setup_params = function(data, params) {GeomLinerange$setup_params(data, params) },
    extra_params = c("na.rm", "orientation", "direction", "tipformat", "tipgap"),

    setup_data = function(data, params) {
        # Based on direction, change xmin or xmax with a multiplier
        lefmul <- 1; rigmul <- 1;
        if (params$direction == "left")  {rigmul <- 0}
        if (params$direction == "right") {lefmul <- 0}

        data$flipped_aes <- params$flipped_aes
        data             <- flip_data(data, params$flipped_aes)
        data$width       <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)
        data3 <- data2   <- data # a quick copy
        # generates the main data frame
        data <- transform(data,
            xmin = x - lefmul*width/2,         xmax = x + rigmul*width/2, width = NULL
        )
        # if "double", double the data with shorter lines...
        if ((params$tipformat == "double")|(params$tipformat == "triple")) {
            data2 <- transform(data2,
                ymin = ymin + params$tipgap,   ymax = ymax - params$tipgap,
                xmin = x - lefmul*width/2,     xmax = x + rigmul*width/2, width = NULL
            )
            data <- rbind(data, data2)
        }
        # if "triple", do it one more time...
        if (params$tipformat == "triple") {
            data3 <- transform(data3,
                ymin = ymin + 2*params$tipgap, ymax = ymax - 2*params$tipgap,
                xmin = x - lefmul*width/2,     xmax = x + rigmul*width/2, width = NULL
            )
            data <- rbind(data, data3)
        }
        flip_data(data, params$flipped_aes)
    },

    draw_panel = function(data, panel_params, coord, width = NULL, flipped_aes = FALSE) {
        data <- flip_data(data, flipped_aes)
        x    <- as.vector(rbind(data$xmin, data$xmax, NA, data$x,    data$x,    NA, data$xmin, data$xmax))
        y    <- as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$ymin, NA, data$ymin, data$ymin))


        # make the color list with 3 x for the upper tip, 2 x for the vertical, and 3 x for the lower tip
        data$vcolour     <- data$vcolour %||% data$colour %||% params$colour 
        collist = c()
            for (i in 1:length(data$colour)) {
            collist = c(collist, rep(data$colour[i],3),rep(data$vcolour[i],2),rep(data$colour[i],3))
        }
        data <- vctrs::new_data_frame(list(
          x         = x,
          y         = y,
#          colour    = rep(data$colour, each = 8), # when only a single color, use it 8 times
          colour    = collist,
          alpha     = rep(data$alpha, each = 8),
          size      = rep(data$size, each = 8),
          linetype  = rep(data$linetype, each = 8),
          group     = rep(1:(nrow(data)), each = 8),
          row.names = 1:(nrow(data) * 8)
        ))
        data <- flip_data(data, flipped_aes)
        GeomPath$draw_panel(data, panel_params, coord)
    }
)


