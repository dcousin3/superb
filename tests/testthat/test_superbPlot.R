context("Testing suberbPlot")


test_that("PRELIMINARY TESTS (1/4)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')
	
    library(grid)
    library(gridExtra)

    # files are exported for validation with Mathematica's MeanPlot
    # write.table(ToothGrowth, file = "file0.dat", sep = "\t", col.names = FALSE)

    plt <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", plotStyle="bar" )

    expect_equal( "ggplot" %in% class(plt), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("PRELIMINARY TESTS (2/4)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    library(grid)
    g0 <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", 
      adjustments = list(purpose = "difference"), plotStyle="bar"
    ) 
    g1 <- g0 + xlab("Dose") + ylab("Tooth Growth") + labs(title="adsf") +
    theme_light(base_size=20) + annotation_custom(grid.text("allo",x=.5,y=.5,gp=gpar(fontsize=20, col="grey")))
    g2 <- g1 + theme(axis.text.x = element_text(size=30, colour="red") ) + coord_cartesian(ylim=c(5,45))

    expect_equal( "ggplot" %in% class(g2), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})

test_that("PRELIMINARY TESTS (3/4)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    res <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), 
      variables = "len", showPlot=FALSE )

    expect_output( str(res), "data.frame")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})

test_that("PRELIMINARY TESTS (4/4)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    p <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", plotStyle="line" )

    expect_equal( "ggplot" %in% class(p), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})




#########################################
# SYSTEMATIC TESTS OF THE STATISTICS
#########################################

test_that("test 1a: 3 groupes inpependants", { 
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    dta1a <- GRD( SubjectsPerGroup = 20, BSFactors = "Group(3)", Population = list( mean=10, stddev = 5) )
    # write.table(dta1a, file = "test1a.dat", sep = "\t", col.names = FALSE)
    p <- superbPlot(dta1a, BSFactor = "Group", variables = "DV",
      statistic = "mean", errorbar = "SE", plotStyle="line")

    expect_equal( "ggplot" %in% class(p), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 1b: factorielle a grps independants; 3 x 2", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    dta1b <- GRD( SubjectsPerGroup = 20, BSFactors = "Group(3): Sex(2)", Population = list( mean=10, stddev = 5))
    # write.table(dta1b, file = "test1b.dat", sep = "\t", col.names = FALSE)
    p <- superbPlot(dta1b, BSFactor = c("Group","Sex"), variables = "DV",
      statistic = "mean", errorbar = "SE" )

    expect_equal( "ggplot" %in% class(p), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 2a: 1 facteur a 3 mesures repetees; (3)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'warnings')

    dta2a <- GRD( WSFactors = "Moment(3)", SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5))
    # write.table(dta2a, file = "test2a.dat", sep = "\t", col.names = FALSE)
    expect_message( p <- superbPlot(dta2a, WSFactor = "moment(3)", 
      adjustments=list(decorrelation="CA"),
      errorbar = "CI", plotStyle="line",
      variables = c("DV.1","DV.2","DV.3") 
    ))

    expect_equal( "ggplot" %in% class(p), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 2b: 2 facteurs a mesures repetees; (3 x 2)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    dta2b <- GRD( WSFactors = "Moment(3): Dose(2)", SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5, rho = .80))
    # write.table(dta2b, file = "test2b.dat", sep = "\t", col.names = FALSE)
    p <- superbPlot(dta2b, WSFactor = c("moment(3)","Dose(2)"), 
      variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
      statistic="mean", errorbar = "CI", gamma = 0.90, plotStyle = "line",
      adjustments = list(purpose="difference", decorrelation="CM"),
      errorbarParams = list(position = position_dodge(width = .15)),
      pointParams = list(position = position_dodge(width = .15)),
    )

    expect_equal( "ggplot" %in% class(p), TRUE)
    options("superb.feedback" = c("design","warnings") ) # restores to default
})


test_that("test 3: scheme mixte; 3 x (3)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'warnings')

    dta3 <- GRD( BSFactors = "Group(3)", WSFactors = "Moment(3)", 
      SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5),
      Effects = list("Moment" = slope(5))
    )
    # write.table(dta3, file = "test3.dat", sep = "\t", col.names = FALSE)
    expect_message( p <- superbPlot(dta3, WSFactor = "Moment(3)", BSFactor = "Group", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE", plotStyle="line",
        adjustments = list(purpose="single", decorrelation="CM")
    ))

    expect_equal( "ggplot" %in% class(p), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 4a: scheme a trois facteurs, 2 etant between  3 x 3 x (3)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    dta4a <- GRD( BSFactors = "Group(3) : Dose(3)", WSFactors = "Moment(3)", 
      SubjectsPerGroup = 4, Population = list( mean=10, stddev = 5),
      Effects = list("Moment" = slope(5))
    )
    # write.table(dta4a, file = "test4a.dat", sep = "\t", col.names = FALSE)
    p <- superbPlot(dta4a, BSFactor = c("Group","Dose"), WSFactor = "Moment(3)", 
      variables = c("DV.1","DV.2","DV.3"), plotStyle = "line",
      statistic = "mean", errorbar = "SE",
      adjustments = list(purpose="difference", decorrelation="none"),
      factorOrder = c("Dose","Group","Moment"), showPlot = T)

    expect_equal( "ggplot" %in% class(p), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})
    
  
test_that("test 5a: scheme a quatre facteurs; 5 x 4 (3 x 2)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    dta5a <- GRD( SubjectsPerGroup = 20, BSFactors = "Group(5) : Dose(4)", WSFactors = "Moment(3):Hand(2)", 
        Population = list( mean=10, stddev = 5, rho = .90),
        Effects = list("Moment" = slope(5), "Hand" = slope(10)) )
    # write.table(dta5a, file = "test5a.dat", sep = "\t", col.names = FALSE)
    p <- superbPlot(dta5a, plotStyle="line",
        WSFactor = c("Moment(3)","Hand(2)"), 
        BSFactor= c("Group","Dose"),
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "CI", gamma = .9999,
        adjustments = list(purpose="difference", decorrelation="CM")
    )

    expect_equal( "ggplot" %in% class(p), TRUE)
    options("superb.feedback" = c("design","warnings") ) # restores to default
})


#########################################
# SYSTEMATIC TESTS OF THE OPTIONS
#########################################

test_that("test 6: Some data", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)",
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 5, rho = 0.8) )

    expect_output( str(dta6), "data.frame")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 6a: factorOrder", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    library(gridExtra)
    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)",
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 5, rho = 0.8) )
    # factorOrder
    p1 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "SE", factorOrder = c("Moment", "Hand") )
    p2 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "SE", factorOrder = c("Hand","Moment") )
    p <- grid.arrange(p1,p2,ncol=2)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 6b: adjustments CA vs CM vs LM vs UA", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    library(gridExtra)
    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)", 
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 5, rho = 0.8) )
    # adjustments CA vs CM vs LM vs UA
    p1 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        adjustments = list(purpose="difference", decorrelation="CA") )+
      coord_cartesian( ylim = c(8,30) ) + labs(title="CA") 
    p2 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        adjustments = list(purpose="difference", decorrelation="CM") )+
      coord_cartesian( ylim = c(8,30) ) + labs(title="CM") 
    p3 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        adjustments = list(purpose="difference", decorrelation="LM") )+
      coord_cartesian( ylim = c(8,30) ) + labs(title="LM") 
    p4 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        adjustments = list(purpose="difference", decorrelation="UA") )+
      coord_cartesian( ylim = c(8,30) ) + labs(title="UA") 
    p <- grid.arrange(p1,p2,p3,p4,ncol=4)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    expect_equal( "ggplot" %in% class(p3), TRUE)
    expect_equal( "ggplot" %in% class(p4), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 6c: statistics of central tendency mean, median and gmean", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    library(gridExtra)
    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)",  
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 1, rho = 0.8) )
    p1 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "CI"  ) +
      coord_cartesian( ylim = c(8,30) ) + labs(title="mean") 
    p2 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "median", errorbar = "CI"  ) +
      coord_cartesian( ylim = c(8,30) ) + labs(title="median") 
    p3 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "gmean", errorbar = "CI"  ) +
      coord_cartesian( ylim = c(8,30) ) + labs(title="geometric mean")
    p <- grid.arrange(p1,p2,p3,ncol=3)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    expect_equal( "ggplot" %in% class(p3), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 6d: statistics of dispersion sd and MAD", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = "none") # to supress design confirmation; unneeded in tests

    library(gridExtra)
    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)",  
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 5, rho = 0.8) )    # functions; SD should be asymmetrical; fisherskew should be about 0
    # fisherkurtosis is less stable; MAD should be about 2.5;
    # gmean requires only positive data;
    p1 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "sd", errorbar = "CI"  )
    p2 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"),  
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "MAD", errorbar = "CI"  )
    p <- grid.arrange(p1,p2,ncol=2)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})



test_that("test 6e: adding ggplot graphics directives", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    # ggplot arguments
    p1 <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", errorbar = "CI", gamma = .999,
      adjustments = list(purpose = "difference") )
    p2 <- p1 + 
      # all these are added to the plot
      xlab("Dose per day") + ylab("Tooth Growth after study") + 
      theme_light(base_size=14 )

    expect_equal( "ggplot" %in% class(p2), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})

      
test_that("test 6f: adding ggplot arguments to the error bars, to the points", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    p <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", errorbar = "CI", gamma = .999,
      adjustments = list(purpose = "difference"),
      # see geom_errorbar for the possible arguments
      errorbarParams = list(width = .8, linewidth = 3, colour = "gray"),
      # see geom_point or geom_bar for possible arguments
      barParams = list(linetype = 3, colour = "black", linewidth = .5)  
    )

    expect_equal( "ggplot" %in% class(p), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 6g: adding ggplot arguments to the error bars, to the points (bis)", {
	old <- options() 
	on.exit(options(old)) 
	options("superb.feedback" = 'none')

    p <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", errorbar = "CI", gamma = .999,
      adjustments = list(purpose = "difference"),
      plotStyle = "line",
      # see geom_errorbar for the possible arguments
      errorbarParams = list(width = .02, linewidth = 0.1, colour = "gray"),
      # see geom_point or geom_bar for possible arguments
      pointParams = list(colour = "gray", size = 10.5)  
    )

    expect_equal( "ggplot" %in% class(p), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


#########################################
# TESTS WITH ICC
#########################################

test_that("Explorations for ICC", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('warnings'))

    library(gridExtra)
    dta99 <- GRD( WSFactors = "Moment(3)", SubjectsPerGroup = 15, 
        Population = list( mean=20, stddev = 5),
        Effects = list("Moment" = slope(3) ) )
    # add cluster information at the end
    dta99$myclus <- sort(rep(1:5, 3))
    # create some intraclass correlation artificially
    dta99$DV.1 <- dta99$DV.1 + 10 * dta99$myclus
    dta99$DV.2 <- dta99$DV.2 + 10 * dta99$myclus
    dta99$DV.3 <- dta99$DV.3 + 10 * dta99$myclus

    # write.table(dta99, file = 'file9.dat', sep = "\t", col.names = FALSE)

    expect_message( noncluster <- superbPlot(dta99, WSFactor = "moment(3)", 
      adjustments = list(decorrelation="CM"),
      errorbar = "CI", showPlot=TRUE,
      variables = c("DV.1","DV.2","DV.3") 
    )+ labs(title="Without cluster information") + coord_cartesian( ylim = c(40,60) ) )
    expect_message( yescluster <- superbPlot(dta99, WSFactor = "moment(3)", 
      adjustments = list(decorrelation="CM", samplingDesign = "CRS"),
      clusterColumn = "myclus",
      errorbar = "CI", showPlot=TRUE,
      variables = c("DV.1","DV.2","DV.3") 
    )+ labs(title="with cluster information") + coord_cartesian( ylim = c(40,60) ) )
    p <- grid.arrange(noncluster, yescluster, ncol=2)

    expect_equal( "ggplot" %in% class(noncluster), TRUE)
    expect_equal( "ggplot" %in% class(yescluster), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})



########################################################
# latest tests based on dta3 verifying CM and popSize  #
########################################################

test_that("Verifying CM and popSize ", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('warnings'))

    library(gridExtra)
    dta3 <- GRD( BSFactors = "Group(2)", WSFactors = "Moment(3)", 
      SubjectsPerGroup = 5, Population = list (mean = 20, stddev = 5, rho = 0.8),
      Effects = list("Moment" = slope(5)) )
    # write.table(dta3, file = "file3.dat", sep = "\t", col.names = FALSE)

    expect_message( p1 <- superbPlot(dta3, WSFactor = "Moment(3)", BSFactor = "Group", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE",
        adjustments = list(purpose="single", decorrelation="CM", popSize = Inf )
      ) + labs(title="Infinite populations") )
    expect_message( p2 <- superbPlot(dta3, WSFactor = "Moment(3)", BSFactor = "Group", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE",
        adjustments = list(purpose="single", decorrelation="CM", popSize = c(Inf,6) )
      ) + labs(title="population of 6 in grp 2") )
    p <- grid.arrange(p1,p2,ncol=2)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


#########################################
# testing pre and post processing: OK!
#########################################

test_that("Testing pre and post processing", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('warnings'))

    library(ggplot2)
    library(gridExtra)
    dta9 <- GRD( WSFactors = "Moment(3)", SubjectsPerGroup = 5, 
        Population = list( mean=20, stddev = 5),
        Effects = list("Moment" = slope(3) ) )
    # write.table(dta9, file = "file9.dat", sep = "\t", col.names = FALSE)

    expect_message( 
        truecm <- superbPlot(dta9, WSFactor = "moment(3)",
          adjustments=list(decorrelation="CM"),
          errorbar = "CI", showPlot=T,
          variables = c("DV.1","DV.2","DV.3") 
        )+ labs(title="With decorrelation = CM") 
    )
    altcm <- superbPlot(dta9, WSFactor = "moment(3)", 
      adjustments=list(decorrelation="none"),
      preprocessfct = "subjectCenteringTransform",
      postprocessfct = c("biasCorrectionTransform"),
      errorbar = "CI", showPlot=T,
      variables = c("DV.1","DV.2","DV.3") 
    )+ labs(title="with pre and post processing")
    pcm <- grid.arrange(truecm,altcm,ncol=2)

    expect_message( 
        truelm <- superbPlot(dta9, WSFactor = "moment(3)", 
          adjustments=list(decorrelation="LM"),
          errorbar = "CI", showPlot=T,
          variables = c("DV.1","DV.2","DV.3") 
        )+ labs(title="with decorrelation = LM") 
    )
    altlm <- superbPlot(dta9, WSFactor = "moment(3)", 
      adjustments=list(decorrelation="none"),
      preprocessfct = "subjectCenteringTransform",
      postprocessfct = c("biasCorrectionTransform","poolSDTransform"),
      errorbar = "CI", showPlot=T,
      variables = c("DV.1","DV.2","DV.3") 
    )+ labs(title="with pre and post processing")
    plm <- grid.arrange(truelm,altlm,ncol=2)

    expect_message( 
        truecmvslm <- superbPlot(dta9, WSFactor = "moment(3)", 
          adjustments=list(decorrelation="LM"),
          errorbar = "CI", showPlot=T,
          variables = c("DV.1","DV.2","DV.3") 
        )+ labs(title="with decorrelation = LM") 
    )
    expect_message( altcmvslm <- superbPlot(dta9, WSFactor = "moment(3)", 
        adjustments=list(decorrelation="CM"),
        postprocessfct = c("poolSDTransform"),
        errorbar = "CI", showPlot=T,
        variables = c("DV.1","DV.2","DV.3") 
        )+ labs(title="with decorrelation = CM and pooling") 
    )
    pcmvslm <- grid.arrange(truecmvslm,altcmvslm,ncol=2)
    
    expect_equal( "ggplot" %in% class(truecm), TRUE)
    expect_equal( "ggplot" %in% class(altcm), TRUE)
    expect_equal( "ggplot" %in% class(truelm), TRUE)
    expect_equal( "ggplot" %in% class(altlm), TRUE)
    expect_equal( "ggplot" %in% class(truecmvslm), TRUE)
    expect_equal( "ggplot" %in% class(altcmvslm), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})



#########################################
# testing Violinplots a bit more!
#########################################
test_that("Many tests with the extended violin format", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('none'))

    library(ggplot2)
    dta9 <- GRD( WSFactors = "Moment(3)", SubjectsPerGroup = 500, 
        Population = list( mean=20, stddev = 5),
        Effects = list("Moment" = slope(3) ) )

    plt1 <- superbPlot(dta9,
            WSFactor = "T(2)",
            variables = c("DV.1","DV.2"),
            adjustments = list(purpose="difference", decorrelation="CM"),
            plotStyle = "pointjitterviolin",
            violinParams = list(alpha =0.7, color="red")
        ) 
    plt2 <- superbPlot(dta9,
            WSFactor = "T(2)",
            variables = c("DV.1","DV.2"),
            adjustments = list(purpose="difference", decorrelation="CM"),
            plotStyle = "pointjitterviolin",
            jitterParams = list(width = 0.1),
            violinParams = list(alpha =0.7, color="red", direction=1, push=0.1)
        ) 
    plt3 <- superbPlot(dta9,
            WSFactor = "T(2)",
            variables = c("DV.1","DV.2"),
            adjustments = list(purpose="difference", decorrelation="CM"),
            plotStyle = "pointjitterviolin",
            jitterParams = list(width = 0.1),
            violinParams = list(alpha =0.7, color="red", direction=-1, push=0.1)
        ) 
    plt4 <- superbPlot(dta9,
            WSFactor = "T(2)",
            variables = c("DV.1","DV.2"),
            adjustments = list(purpose="difference", decorrelation="CM"),
            plotStyle = "raincloud",
            violinParams = list(alpha =0.7, color="red", direction=-1)
        ) 
    plt5 <- superbPlot(dta9,
            WSFactor = "T(2)",
            variables = c("DV.1","DV.2"),
            adjustments = list(purpose="difference", decorrelation="CM"),
            plotStyle = "raincloud",
            jitterParams = list(width = 0.1),
            violinParams = list(alpha =0.7, color="red", direction=-1, push=0.1)
        ) 
    plt6 <- superbPlot(dta9,
            WSFactor = "T(2)",
            variables = c("DV.1","DV.2"),
            adjustments = list(purpose="difference", decorrelation="CM"),
            plotStyle = "raincloud",
            jitterParams = list(width = 0.1),
            violinParams = list(alpha =0.7, color="red", direction=+1, push=0.1)
        ) 

    expect_error( print(plt1), NA )
    expect_error( print(plt2), NA )
    expect_error( print(plt3), NA )
    expect_error( print(plt4), NA )
    expect_error( print(plt5), NA )
    expect_error( print(plt6), NA )
    expect_equal( "ggplot" %in% class(plt1), TRUE)
    expect_equal( "ggplot" %in% class(plt2), TRUE)
    expect_equal( "ggplot" %in% class(plt3), TRUE)
    expect_equal( "ggplot" %in% class(plt4), TRUE)
    expect_equal( "ggplot" %in% class(plt5), TRUE)
    expect_equal( "ggplot" %in% class(plt6), TRUE)

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})



#########################################
# testing multiple formats!
#########################################

test_that("Many tests with TMB1964r", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('none'))

    library(ggplot2)
    mee = TMB1964r[TMB1964r$Language == "English"|TMB1964r$Language == "French",]

    mp <- function(style, ...) {
        superbPlot(mee,
            WSFactor = "T(7)",
            BSFactor = c("Condition","Sex"),
            variables = c("T1","T2","T3","T4","T5","T6","T7"),
            adjustments = list(purpose="difference", decorrelation="CM"),
            plotStyle = style,
            ...
        ) 
    }
    ###### BASIC PLOTS ######
    plt1 <- mp("bar",
        errorbarParams = list(linewidth=0.75, position = position_dodge(.95) ),
        barParams = list(linewidth=0.5)
    ) + 
    scale_colour_manual( name = "asdf", 
        labels = c("Context 0", "Context 2", "Context 4", "Context 8"), 
        values = c("blue", "black", "purple", "red")) +
    scale_fill_manual( name = "asdf", 
        labels = c("Context 0", "Context 2", "Context 4", "Context 8"), 
        values = c("blue", "black", "purple", "red")) +
    theme_bw(base_size = 16) +
    labs(x = "Exposure duration (ms)", y = "Mean of correct responses" )+ 
    scale_x_discrete(labels=c("1" = "16.67", "2" = "33.33",
        "3"="50.00", "4" = "66.67", "5"="83.33", "6"="100.00", "7"="116.67"))

    plt2 <- mp("line",
        errorbarParams = list(linewidth=0.75, width = 0.2, position = position_dodge(.5) ),
        pointParams = list(size=2.5, position = position_dodge(.5)),
        lineParams = list(linewidth=0.25)
    )

    plt3 <- mp("point",
        errorbarParams = list(position = position_dodge(.5) ),
        pointParams = list(size=2.5, position = position_dodge(.5))
    )

    ###### ADVANCED PLOTS ######
    plt4 <- mp("pointjitter",
        errorbarParams = list(position = position_dodge(.5) ),
        pointParams = list(size=3.5, position = position_dodge(.5)),
        jitterParams = list(size = 0.5)
    )

    plt5 <- mp("pointjitterviolin",
        errorbarParams = list(position = position_dodge(.5) ),
        pointParams = list(size=3.5, position = position_dodge(.5)),
        jitterParams = list(size = 0.5),
        violinParams = list(alpha =0.7)
    )

    dta <- superb::GRD( WSFactors = "timepoints (100) : condition(2)", 
        SubjectsPerGroup = 40,
        RenameDV = "activation",
        Effects = list("timepoints" = extent(5), "condition" = extent(3) ),
        Population=list(mean=50,stddev=10,rho=0.75)
    )
    plt6 <- superbPlot(dta, 
       WSFactors   = c("timepoints(100)", "condition(2)"),
       variables = colnames(dta)[2:201],   ## all the names of the dataframe except "id"
       adjustments = list(
            purpose       = "single",
            decorrelation = "CM"        ## or none for no decorrelation
       ),
       plotStyle="lineBand",            # note the uppercase B 
       pointParams = list(size= 1) 
    )

    expect_error( print(plt1), NA )
    expect_error( print(plt2), NA )
    expect_error( print(plt3), NA )
    expect_error( print(plt4), NA )
    expect_error( print(plt5), NA )
    expect_error( print(plt6), NA )
    expect_equal( "ggplot" %in% class(plt1), TRUE)
    expect_equal( "ggplot" %in% class(plt2), TRUE)
    expect_equal( "ggplot" %in% class(plt3), TRUE)
    expect_equal( "ggplot" %in% class(plt4), TRUE)
    expect_equal( "ggplot" %in% class(plt5), TRUE)
    expect_equal( "ggplot" %in% class(plt6), TRUE)

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Heterogeneous variances", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('none'))

    dta <- GRD(
        BSFactors = "dif(3) : grp (2)", 
        WSFactors="day(1,2)",
        SubjectsPerGroup = 3,
        Population=list(
            mean = 100, 
            scores = "rnorm(1, mean = GM, sd = 100 * (grp-1) +0.1)"
        )
    )
    options("superb.feedback" = c('warnings'))
    expect_message( 
        superbPlot(dta,
            BSFactor = c("dif","grp"),
            WSFactor = "day(2)",
            variables = c("DV.1","DV.2"),
            adjustment = list( purpose = "difference") )
    )
	options("superb.feedback" = c('none'))
    plt <- superbPlot(dta,
        BSFactor = c("dif","grp"),
        WSFactor = "day(2)",
        variables = c("DV.1","DV.2"),
        adjustment = list( purpose = "tryon") )

    expect_equal( "ggplot" %in% class(plt), TRUE )
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Tryon vs. difference", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('warnings'))

    #lets have data with massively heterogeneous variances
    dta <-GRD( BSFactors="grp(3)",
        RenameDV = "score",
        Population=list(
            mean = 100, 
            scores = "rnorm(1, mean = GM, sd = 10 * grp)"
        ),
        SubjectsPerGroup = 50,
        Effects = list("grp" = slope(15) ) 
    )
    # do the plot twice with transparant background
    expect_message(plt1 <- superbPlot(dta, 
        BSFactor = "grp", plotStyle="line",
        variables = "score",  
        errorbarParams = list(color="blue",position = position_nudge(-0.1) ),
        adjustments = list( purpose = "difference") ) + 
     labs(title="(blue) Difference-adjusted 95% confidence intervals\n(red) Tryon 95% confidence intervals") +
     coord_cartesian( ylim = c(65,135) ) + 
     theme(panel.background = element_rect(fill = "transparent"),
           plot.background = element_rect(fill = "transparent", color = "white")) 
    )
    expect_message(plt2 <- superbPlot(dta, 
        BSFactor = c("grp"), plotStyle="line",
        variables = "score",  
        errorbarParams = list(color="red",position = position_nudge(+0.1) ),
        adjustments = list( purpose = "tryon") ) +
     labs(title="(blue) Difference-adjusted 95% confidence intervals\n(red) Tryon 95% confidence intervals") +
     coord_cartesian( ylim = c(65,135) ) + 
     theme(panel.background = element_rect(fill = "transparent"),
           plot.background = element_rect(fill = "transparent", color = "white")) )
    # transform the plots in manipulable objets
    plt1g <- ggplotGrob(plt1)
    plt2g <- ggplotGrob(plt2)
    # put the two grob onto an empty ggplot (as the positions are the same, they will be overlayed)
    plt <- ggplot() + 
        annotation_custom(grob=plt1g) + 
        annotation_custom(grob=plt2g)
    expect_equal( "ggplot" %in% class(plt), TRUE)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})





#########################################
# testing multiple formats!
#########################################

test_that("Testing corset plot", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('none'))

    ## corset plot
    dta <- GRD(SubjectsPerGroup = 50, WSFactors = "moment(2)", Effects = list("moment"=slope(3)))
    plt <- superbPlot(dta, WSFactors = "moment(2)", variables = c("DV.1","DV.2"),
      plotStyle = "corset" )
    expect_equal( "ggplot" %in% class(plt), TRUE)

    plt <- superbPlot(dta, WSFactors = "moment(2)", variables = c("DV.1","DV.2"),
        plotStyle    = "corset", 
        lineParams   = list(colorize=TRUE),
        violinParams = list(fill = "green", alpha = 0.2 ) 
    ) + theme_bw() + 
    theme(axis.line.y = element_line(color="black"), legend.position=c(0.1,0.75), panel.border = element_blank() ) +
    scale_color_manual('Direction\n of change', values=c("red","gray50"), labels=c('decreasing', 'increasing'))
    expect_equal( "ggplot" %in% class(plt), TRUE)

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Testing local decorrelation plot", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('none'))

    #test
    X <- GRD(SubjectsPerGroup = 50, WSFactors = "time(10)", Effects = list("time"=extent(3)))
    go <- function(m) {
        superbPlot(X, 
            WSFactor = "time(10)",
            variables = names(X)[-1],
            adjustments = list(decorrelation = m),
            plotStyle = "lineBand"
        )+ylim(c(-6,6))
    }
    plt <- go("LD2")
    expect_equal( "ggplot" %in% class(plt), TRUE)

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})



