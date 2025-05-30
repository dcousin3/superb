######################################################################################
#' @name geom_superberrorbar
#'
#' @title geom_superberrorbar for expanded error bar displays 
#'
#' @md
#'
#' @description ``geom_superberrorbar()`` is a geom for ggplots; it is based on 
#'      the original geom_errorbar (and is totally compatible with it) but
#'      expands this geom in four different ways. First, it is possible to
#'      decide the error bar tips `direction` which can be unidirectional, pointing to 
#'      the "left" or to the "right" or go in "both" directions.
#'      Second, it is possible set `tipformat` to "double" or "triple" the horizontal marks
#'      at the extremities of the error bar, with a `tipgap` of your liking.
#'      Third, an additional characteristic is `vcolour` to set a different colour
#'      for the vertical part of the error bar or the pair
#'      `vcolour` and `wcolour` for the top half and bottom half of the vertical
#'      error bar. The colour(s) can also be "NA" to
#'      have it invisible. Lastly, the error bar can be `pointing` "up" and "down"
#'      or go in "both" (the default)
#'
#' @param mapping (as usual) see geom_errorbar
#' @param data  (as usual) see geom_errorbar
#' @param stat (as usual) see geom_errorbar
#' @param position (as usual) see geom_errorbar
#' @param ... all additional parameters are sent to the underlying geom_path. Includes
#'    * ``pointing`` (NEW) either "up", "down" or "both" up and down;
#'    * ``direction`` (NEW) "left", "right" or "both" (Default is "both")
#'    * ``tipformat`` (NEW) "single", "double" or "triple" to add additional 
#'        marker lines to the tips (default is "single")
#'    * ``tipgap`` (NEW) The spacing between the markers when "double" or "triple" is used (default 0.1)
#'    * ``vcolour`` (NEW) for the vertical part of the error bar
#'    * ``wcolour`` (NEW) if specified, for the second half of the vertical part of the error bar.
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
#' ggplot(dta, aes(ymin=center-width, ymax=center+width, x = grp ) ) +
#'   geom_superberrorbar()
#'
#' # an example with left-pointing error bars
#' ggplot(dta, aes(ymin=center-width, ymax=center+width, x = grp ) ) +
#'   geom_superberrorbar(direction="left", width = 0.1)
#'
#' # an example with doubled-tipped error bar and the default tipgap
#' ggplot(dta, aes(ymin=center-width, ymax=center+width, x = grp ) ) +
#'   geom_superberrorbar(tipformat = "double", width = 0.1)
#'
#' # an example with left-pointing tripled-tip error bars with small gaps
#' ggplot(dta, aes(ymin=center-width, ymax=center+width, x = grp ) ) +
#'   geom_superberrorbar(tipformat = "triple", width= 0.1, tipgap = 0.04, direction = "left")
#' 
#' # an example with unidirectional error bars (here "up" bars)
#' ggplot(dta, aes(y= center, ymin=center-width, ymax=center+width, x = grp ) ) +
#'   geom_bar(stat="identity", fill = "yellow") + 
#'   geom_superberrorbar(pointing = "up")
#' 
#' # a final example with two-coloured, left-pointing tripled-tip error bars with small gaps
#' ggplot(dta, aes(ymin=center-width, ymax=center+width, x = grp ) ) +
#'   geom_superberrorbar(tipformat = "triple", width= 0.1, tipgap = 0.04, direction = "left",
#'            colour = "black", vcolour = "orange")
#' 
#' # This new geom is integrated inside superb() so that you can vary the 
#' # error bar shapes. Let's see examples:
#' 
#' # using GRD to generate random data with a moderate effect
#' options(superb.feedback = 'none') # shut down 'warnings' and 'design' interpretation messages
#' test <- GRD(SubjectsPerGroup  = 20,
#'			   WSFactors = "Moment(5)", 
#'             Effects = list("Moment" = extent(10) ),
#'             Population = list(mean = 100, stddev = 15, rho = 0.8) ) 
#' 
#' ornate = list(
#'         labs(title =paste("(left)            95% confidence intervals",
#'                         "\n(right)          99% confidence intervals",
#'                         "\n(center, up) 99.9% confidence intervals")),
#'         xlab("Moment"), ylab("Score"),
#'         coord_cartesian( ylim = c(85,125) )
#' )
#' 
#' plt1 <- superb(
#'             crange(DV.1, DV.5) ~ ., 
#'             test, 
#'             WSFactors = "Moment(5)",  
#'             adjustments=list(purpose = "difference", decorrelation = "CA"), 
#'             errorbarParams = list(direction = "left", color="purple", 
#'                                   width = 0.2, position = position_nudge(-0.05) ),
#'             gamma     = 0.95,
#'             plotLayout = "line" ) + ornate
#' plt2 <- superb( 
#'             crange(DV.1, DV.5) ~ ., 
#'             test, 
#'             WSFactors = "Moment(5)",  
#'             adjustments=list(purpose = "difference", decorrelation = "CA"), 
#'             errorbarParams = list(direction = "right", tipgap = 0.25, tipformat = "double", 
#'                                   width = 0.2, position = position_nudge(+0.05) ),
#'             gamma     = 0.99,
#'             plotLayout = "line" ) + ornate 
#' plt3 <- superb( 
#'             crange(DV.1, DV.5) ~ ., 
#'             test, 
#'             WSFactors = "Moment(5)",  
#'             adjustments=list(purpose = "difference", decorrelation = "CA"), 
#'             errorbarParams = list(direction = "both", tipformat = "single", pointing="up", 
#'                                   width = 0.2, position = position_nudge(0) ),
#'             gamma     = 0.999,
#'             plotLayout = "line" ) + ornate 
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
#'
#' # all of them as aesthetics
#' set.seed(1)
#' library(dplyr)
#' dat <- data.frame(Trial = c(rep("Pre",9),rep("Post",9)), 
#'                      Time = rep.int(seq(0,120,15),2), 
#'                      var = c(rnorm(9,15,2),rnorm(9,22,2)),
#'                      var_sd = c(rnorm(18,3,1)))
#' dat <- mutate(dat, point = ifelse(Trial == "Pre","down","up"))
#' dat <- mutate(dat, direc = ifelse(Trial == "Pre","left","right"))
#' dat <- mutate(dat, tipfo = ifelse(Trial == "Pre","double","triple"))
#' dat <- mutate(dat, vcolo = ifelse(Trial == "Pre","red","blue"))
#'    
#' ggplot(data = dat, 
#'        aes(x = Time, y = var, group = Trial)) +
#'    geom_line(aes(linetype = Trial)) +  
#'    geom_point(aes(shape= Trial, fill = Trial), size=2) +
#'    geom_superberrorbar(aes(ymin=var-var_sd, 
#'                            ymax=var+var_sd,
#'                            direction = direc,
#'                            pointing = point, 
#'                            wcolour = vcolo,  
#'                            vcolour = "green",
#'                            tipformat = tipfo
#'        ), 
#'        width = 4)
#'
#' @export geom_superberrorbar
#'
######################################################################################


geom_superberrorbar <- function(
    mapping     = NULL, 
    data        = NULL,
    stat        = "identity", 
    position    = "identity",
# Ok, all the additional aes() go within ...
#   pointing    = "both",    # new: "up", "down", "both"
#   direction   = "both",    # new: "left", "right", "both"
#   tipformat   = "single",  # new: "single", "double", "triple"
#   tipgap      = 0.1,       # new: spacing between the double tips
#   vcolour     = "black",   
#   wcolour     = "black",   
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
        geom        = GeomSuperbErrorbar,
        position    = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params      = list( 
            na.rm       = na.rm,
            orientation = orientation,
            ...
        )
    )
}

# from rlang
`%||%` <- function(x, y) {if (is.null(x)) y else x }

GeomSuperbErrorbar <- ggproto("GeomSuperbErrorbar", Geom,
    default_aes = aes( # the parameters
        colour    = "black", 
        linewidth = 0.5, 
        linetype  = 1, 
        width     = 0.5,
        alpha     = NA,
        # novel arguments
        direction = "both",
        pointing  = "both",
        tipformat = "single",
        tipgap    = 0.1,
        vcolour   = NULL, 
        wcolour   = NULL 
    ),
    draw_key     = draw_key_path,
    required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),
    setup_params = function(data, params) {
        # there must be a better way to do this caliss
        plist <- GeomLinerange$setup_params(data, params) 
        c(plist, direction="both", pointing="both", tipformat="single", tipgap=0.1,vcolour=NULL, wcolour=NULL)
    },
    extra_params = c("na.rm", "orientation"),

    setup_data = function(data, params) {

        # Based on direction, change xmin or xmax with a multiplier
        lefmul <- 1; rigmul <- 1;
        if (params$direction == "left")  {rigmul <- 0}
        if (params$direction == "right") {lefmul <- 0}

        data$flipped_aes <- params$flipped_aes
        data             <- flip_data(data, params$flipped_aes)
        data$width       <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)
        data$direction <- data$direction %||% params$direction
        data$pointing  <- data$pointing  %||% params$pointing
        data$tipformat <- data$tipformat %||% params$tipformat
        data$tipgap    <- data$tipgap    %||% params$tipgap
        data$vcolour   <- data$vcolour   %||% params$vcolour
        data$wcolour   <- data$wcolour   %||% params$wcolour

        data3 <- data2   <- data # a quick copy
        # generates the main data frame
        data <- transform(data,
            xmin  = x - ifelse((direction=="both")|(direction=="left"),1,0)*width/2,
            xmax  = x + ifelse((direction=="both")|(direction=="right"),1,0)*width/2
        )

        # if "double", double the data with shorter lines...
        data2 <- transform(data2, 
            ymin  = ifelse((tipformat == "double")|(tipformat == "triple"), ymin + tipgap, NA ),
            ymax  = ifelse((tipformat == "double")|(tipformat == "triple"), ymax - tipgap, NA ),
            xmin  = ifelse((tipformat == "double")|(tipformat == "triple"), x - ifelse((direction=="both")|(direction=="left"),1,0)*width/2, NA ),
            xmax  = ifelse((tipformat == "double")|(tipformat == "triple"), x + ifelse((direction=="both")|(direction=="right"),1,0)*width/2, NA )
        )
        data <- rbind(data, data2[!is.na(data2$ymin),])

        # if "triple", do it one more time...
        data3 <- transform(data3, 
            ymin  = ifelse((tipformat == "triple"), ymin + 2*tipgap, NA ),
            ymax  = ifelse((tipformat == "triple"), ymax - 2*tipgap, NA ),
            xmin  = ifelse((tipformat == "triple"), x - ifelse((direction=="both")|(direction=="left"),1,0)*width/2, NA ),
            xmax  = ifelse((tipformat == "triple"), x + ifelse((direction=="both")|(direction=="right"),1,0)*width/2, NA )
        )
        data <- rbind(data, data3[!is.na(data3$ymin),])

        # if the aesthetic y is not given, add it to the data frame (used for the center)
        if (is.null(data$y) ) {data$y = (data$ymin+data$ymax)/2}

        flip_data(data, params$flipped_aes)
    },

    draw_panel = function(data, panel_params, coord, width = NULL, flipped_aes = FALSE) {

        data <- flip_data(data, flipped_aes)
        # enter the top line, the median lines (in two halves), the bottom line
        # i.e.: il faut 10 segments pour faire une barre.
        ifelse ( ((data$ymin <= data$y) & (data$y <= data$ymax) ), {
            # if the error bar passes through the center, split it in two (for "pointing")
            x    <- as.vector(rbind(data$xmin, data$xmax, NA, data$x,    data$x,    data$x,    data$x,    NA, data$xmin, data$xmax))
            y    <- as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$y,    data$y,    data$ymin, NA, data$ymin, data$ymin))
        } , {
            # if the error bar hangs out there, away from the center, just have one error bar ("pointing" is disabled de facto)
            x    <- as.vector(rbind(data$xmin, data$xmax, NA, NA,        data$x,    data$x,    NA,        NA, data$xmin, data$xmax))
            y    <- as.vector(rbind(data$ymax, data$ymax, NA, NA,        data$ymax, data$ymin, NA,        NA, data$ymin, data$ymin))
        } )
        # make the color list with 3 x for the upper tip, 4 x for the vertical, and 3 x for the lower tip
        data$vcolour     <- data$vcolour %||% data$colour %||% params$colour
        data$wcolour     <- data$wcolour %||% data$vcolour %||% data$colour %||% params$colour
        collist = c()
        for (i in 1:length(data$colour)) {
            collist = c(collist, rep(data$colour[i],3),rep(data$vcolour[i],2),rep(data$wcolour[i],2),rep(data$colour[i],3))
        }

        nblock = 10 #10 segments for each error bars: the top line, NA, the median line in two halfes, NA, the bottom line

        thealphas    <- as.vector(sapply(data$pointing, \(i){
                                c( rep(ifelse(i != "down", data$alpha, 0), 5 ),
                                   rep(ifelse(i != "up",   data$alpha, 0), 5 ) )
                            }))

        data <- vctrs::new_data_frame(list(
          x         = x,
          y         = y,
          colour    = collist,
          alpha     = thealphas,
          linewidth = rep(data$linewidth, each = nblock),
          linetype  = rep(data$linetype, each = nblock),
          group     = rep(1:(nrow(data)), each = nblock),
          row.names = 1:(nrow(data) * nblock)
        ))

        data <- flip_data(data, flipped_aes)
        GeomPath$draw_panel(data, panel_params, coord)
    }
)


