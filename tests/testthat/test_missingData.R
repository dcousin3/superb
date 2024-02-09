context("Testing missing data")


test_that("Between-subject design", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(ggplot2)

    dataToPlotBS <- GRD( BSFactors="grp(2)",
        SubjectsPerGroup = 1000,
        Effects = list("grp" = slope(10) ),
        Population=list(mean=100, stddev=15),
        Contaminant=list(scores = "NA", proportion=0.2)
    )
    pltB <- superbPlot(dataToPlotBS,
        BSFactors   = "grp", 
        variables   = "DV",
        statistic   = "meanNArm", # because of missing data
        adjustments = list(
            purpose       = "difference",
            decorrelation = "none" 
        )
    ) + coord_cartesian(ylim=c(85,115))

    expect_equal( "ggplot" %in% class(pltB), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Within-subject design", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(ggplot2)

    dataToPlotWS <- GRD( WSFactors="moment(2)",
        SubjectsPerGroup = 1000,
        Effects = list("moment" = slope(10) ),
        Population=list(mean=100, stddev=15, rho=0.80),
        Contaminant=list(scores = "NA", proportion=0.2)
    )
    pltW <- superbPlot(dataToPlotWS,
        WSFactors   = "moment(2)", 
        variables   = c("DV.1","DV.2"),
        statistic   = "meanNArm", # because of missing data
        adjustments = list(
            purpose       = "difference",
            decorrelation = "CA" 
        )
    ) + coord_cartesian(ylim=c(85,115))

    expect_equal( "ggplot" %in% class(pltW), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})

