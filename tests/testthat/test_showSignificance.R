context("Testing showSignificance")


test_that("Simplest example", {
    # quelques tests systematiques...
    library(superb)
    library(ggplot2)
    library(grid)
    options(superb.feedback = 'none')

    # making one random data set with three factors 2 x 3 x (3)
    dta <- GRD(
        BSFactors = c("Group(2)","Age(3)"), 
        WSFactors = c("Moment(3)"),
        Population = list(mean = 75, stddev = 5),
        Effects   = list("Group" = slope(10) )
    )

    # making a two-factor plot 
    plt2 <- superbPlot(dta, 
            BSFactor = c("Group"),
            WSFactor = c("Moment(3)"),
            variables = c("DV.1","DV.2","DV.3"),
            adjustments = list(purpose="difference"),
            factorOrder = c("Moment","Group")
        )

    # lets decorate this plot a bit...
    plt2 <- plt2 +  scale_fill_manual( name = "Group", 
            labels = c("Easy", "Hard", "Unthinkable"), 
            values = c("blue", "purple")) + 
      scale_colour_manual( name = "Group", 
            labels = c("Easy", "Hard", "Unthinkable"), 
            values = c("blue", "purple")) +
      coord_cartesian( ylim = c(50,100) )

    # a very basic example
    plt2 <- plt2 + 
        showSignificance( c(0.75, 1.25), 90, -1, "++1++")
    expect_equal( "ggplot" %in% class(plt2), TRUE)
  
  # restores default information
    options(superb.feedback = c('design','warnings','summary'))
})



test_that("Example with panels", {
    library(superb)
    library(ggplot2)
    library(grid)
    options(superb.feedback = 'none')

    # making one random data set with three factors 2 x 3 x (3)
    dta <- GRD(
        BSFactors = c("Group(2)","Age(3)"), 
        WSFactors = c("Moment(3)"),
        Population = list(mean = 75, stddev = 5),
        Effects   = list("Group" = slope(10) )
    )

    plt3 <- superbPlot(dta, 
            BSFactor = c("Group","Age"),
            WSFactor = c("Moment(3)"),
            variables = c("DV.1","DV.2","DV.3"),
            adjustments = list(purpose="difference"),
            factorOrder = c("Moment","Group","Age")
        )
    plt3 <- plt3 +  scale_fill_manual( name = "Group", 
            labels = c("Easy", "Hard", "Unthinkable"), 
            values = c("blue", "purple")) + 
      scale_colour_manual( name = "Group", 
            labels = c("Easy", "Hard", "Unthinkable"), 
            values = c("blue", "purple")) +
      coord_cartesian( ylim = c(50,100) )

    # an example with panels; the "panel" argument is used to identify on which panel to put the annotation
    plt3 + 
        showSignificance( c(0.75, 1.25), 90, -1, "++1++", panel = list(Age= 1)) + 
        showSignificance( c(1.75, 2.25), 90, -1, "++2++", panel = list(Age= 2)) + 
        showSignificance( c(0.75, 1.25), 90, -1, "++3++", panel = list(Age= 3)) +
        showSignificance( c(1.75, 3.25), 95, -1, "++4++", panel = list(Age= 3))  

    expect_equal( "ggplot" %in% class(plt3), TRUE)
  
  # restores default information
    options(superb.feedback = c('design','warnings','summary'))
})


test_that("Advanced example", {
    library(superb)
    library(ggplot2)
    library(grid)
    options(superb.feedback = 'none')

    # making one random data set with three factors 2 x 3 x (3)
    dta <- GRD(
        BSFactors = c("Group(2)","Age(3)"), 
        WSFactors = c("Moment(3)"),
        Population = list(mean = 75, stddev = 5),
        Effects   = list("Group" = slope(10) )
    )

    plt3 <- superbPlot(dta, 
            BSFactor = c("Group","Age"),
            WSFactor = c("Moment(3)"),
            variables = c("DV.1","DV.2","DV.3"),
            adjustments = list(purpose="difference"),
            factorOrder = c("Moment","Group","Age")
        )
    plt3 <- plt3 +  scale_fill_manual( name = "Group", 
            labels = c("Easy", "Hard", "Unthinkable"), 
            values = c("blue", "purple")) + 
      scale_colour_manual( name = "Group", 
            labels = c("Easy", "Hard", "Unthinkable"), 
            values = c("blue", "purple")) +
      coord_cartesian( ylim = c(50,100) )

    # here, we send additional directives to the annotations
    plt3 + 
        showSignificance( c(0.75, 1.25), 90, -5,  "++1++", panel = list(Age= 1)) + 
        showSignificance( c(1.75, 2.25), 95, -10, "++2++", panel = list(Age = 2),
            textParams    = list(size = 2,              # smaller font
                                family  = "mono",       # courrier font
                                colour= "chartreuse3"   # dark green color
            ), 
            segmentParams = list(linewidth = 1.,             # thicker lines
                                arrow   = arrow(length = unit(0.2, "cm") ), # arrow heads
                                colour = "chartreuse3"  # dark green color as well
            )
        ) +
        showSignificance( c(1.75, 3.25), 95, -30, "++3++", panel = list(Age = 3),
            textParams    = list(size = 5,              # larger font
                                family  = "serif",      # times font
                                alpha = 0.2 ),          # transparent
            segmentParams = list(linewidth = 2., 
                                arrow   = arrow(length = unit(0.2, "cm") ), 
                                alpha = 0.2, 
                                lineend = "round"       # so that line end overlap nicely
            )
        )


    expect_equal( "ggplot" %in% class(plt3), TRUE)
  
  # restores default information
    options(superb.feedback = c('design','warnings','summary'))
})