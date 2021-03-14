context("Testing suberb")


test_that("PRELIMINARY TESTS (1/4)", {
    library(grid)
    library(gridExtra)

    # files are exported for validation with Mathematica's MeanPlot
    # write.table(ToothGrowth, file = "file0.dat", sep = "\t", col.names = FALSE)

    plt <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", Debug=F, plotStyle="bar" )

    expect_equal( "ggplot" %in% class(plt), TRUE)
})


test_that("PRELIMINARY TESTS (2/4)", {
    library(grid)
    g0 <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", 
      adjustments = list(purpose = "difference"), plotStyle="bar"
    ) 
    g1 <- g0 + xlab("Dose") + ylab("Tooth Growth") + labs(title="adsf") +
    theme_light(base_size=20) + annotation_custom(grid.text("allo",x=.5,y=.5,gp=gpar(fontsize=20, col="grey")))
    g2 <- g1 + theme(axis.text.x = element_text(size=30, colour="red") ) + coord_cartesian(ylim=c(5,45))

    expect_equal( "ggplot" %in% class(g2), TRUE)
})

test_that("PRELIMINARY TESTS (3/4)", {
    res <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), 
      variables = "len", showPlot=FALSE )

    expect_output( str(res), "data.frame")
})

test_that("PRELIMINARY TESTS (4/4)", {
    p <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", plotStyle="line" )

    expect_equal( "ggplot" %in% class(p), TRUE)
})




#########################################
# SYSTEMATIC TESTS OF THE STATISTICS
#########################################

test_that("test 1a: 3 groupes inpépendants", { 
    dta1a <- GRD( BSFactors = "Group(3)", Population = list( mean=10, stddev = 5) )
    # write.table(dta1a, file = "test1a.dat", sep = "\t", col.names = FALSE)
    p <- superbPlot(dta1a, BSFactor = "Group", variables = "DV",
      statistic = "mean", errorbar = "SE", plotStyle="line", Debug=F )

    expect_equal( "ggplot" %in% class(p), TRUE)
})


test_that("test 1b: factorielle à grps indépendants; 3 x 2", {
    dta1b <- GRD( BSFactors = "Group(3): Sex(2)", Population = list( mean=10, stddev = 5))
    # write.table(dta1b, file = "test1b.dat", sep = "\t", col.names = FALSE)
    p <- superbPlot(dta1b, BSFactor = c("Group","Sex"), variables = "DV",
      statistic = "mean", errorbar = "SE" )

    expect_equal( "ggplot" %in% class(p), TRUE)
})


test_that("test 2a: 1 facteur à 3 mesures répétées; (3)", {
    dta2a <- GRD( WSFactors = "Moment(3)", SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5))
    # write.table(dta2a, file = "test2a.dat", sep = "\t", col.names = FALSE)
    expect_warning( p <- superbPlot(dta2a, WSFactor = "moment(3)", 
      adjustments=list(decorrelation="CA"),
      errorbar = "CI", plotStyle="line",
      variables = c("DV.1","DV.2","DV.3") 
    ))

    expect_equal( "ggplot" %in% class(p), TRUE)
})


test_that("test 2b: 2 facteurs à mesures répétées; (3 x 2)", {
    dta2b <- GRD( WSFactors = "Moment(3): Dose(2)", SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5, rho = .80))
    # write.table(dta2b, file = "test2b.dat", sep = "\t", col.names = FALSE)
    p <- superbPlot(dta2b, WSFactor = c("moment(3)","Dose(2)"), 
      variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
      statistic="mean", errorbar = "CI", gamma = 0.90, plotStyle = "line",
      adjustments = list(purpose="difference", decorrelation="CM"),
      errorbarParams = list(position = position_dodge(width = .15)),
      pointParams = list(position = position_dodge(width = .15)),
      Quiet=T # to supress design confirmation; unneeded in tests
    )

    expect_equal( "ggplot" %in% class(p), TRUE)
})


test_that("test 3: schème mixte; 3 x (3)", {
    dta3 <- GRD( BSFactors = "Group(3)", WSFactors = "Moment(3)", 
      SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5),
      Effects = list("Moment" = slope(5))
    )
    # write.table(dta3, file = "test3.dat", sep = "\t", col.names = FALSE)
    expect_warning( p <- superbPlot(dta3, WSFactor = "Moment(3)", BSFactor = "Group", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE", plotStyle="line",
        adjustments = list(purpose="single", decorrelation="CM")
    ))

    expect_equal( "ggplot" %in% class(p), TRUE)
})


test_that("test 4a: schème à trois facteurs, 2 étant between  3 x 3 x (3)", {
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
})
    
  
test_that("test 5a: schème à quatre facteurs; 5 x 4 (3 x 2)", {
    dta5a <- GRD( BSFactors = "Group(5) : Dose(4)", WSFactors = "Moment(3):Hand(2)", 
      Debug=FALSE, Summary=FALSE, Population = list( mean=10, stddev = 5, rho = .90),
      Effects = list("Moment" = slope(5), "Hand" = slope(10)) )
    # write.table(dta5a, file = "test5a.dat", sep = "\t", col.names = FALSE)
    p <- superbPlot(dta5a, plotStyle="line",
        WSFactor = c("Moment(3)","Hand(2)"), 
        BSFactor= c("Group","Dose"),
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "CI", gamma = .9999,
        adjustments = list(purpose="difference", decorrelation="CM"), Debug=F, Quiet=T
    )

    expect_equal( "ggplot" %in% class(p), TRUE)
})


#########################################
# SYSTEMATIC TESTS OF THE OPTIONS
#########################################

test_that("test 6: Some data", {
    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)",
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 5, rho = 0.8) )

    expect_output( str(dta6), "data.frame")
})


test_that("test 6a: factorOrder", {
    library(gridExtra)
    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)",
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 5, rho = 0.8) )
    # factorOrder
    p1 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "SE", factorOrder = c("Moment", "Hand") )
    p2 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "SE", factorOrder = c("Hand","Moment") )
    p <- grid.arrange(p1,p2,ncol=2)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
})


test_that("test 6b: adjustments CA vs CM vs LM", {
    library(gridExtra)
    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)", 
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 5, rho = 0.8) )
    # adjustments CA vs CM vs LM
    p1 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        adjustments = list(purpose="difference", decorrelation="CA") )+
      coord_cartesian( ylim = c(8,30) ) + labs(title="CA") 
    p2 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        adjustments = list(purpose="difference", decorrelation="CM") )+
      coord_cartesian( ylim = c(8,30) ) + labs(title="CM") 
    p3 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        adjustments = list(purpose="difference", decorrelation="LM") )+
      coord_cartesian( ylim = c(8,30) ) + labs(title="LM") 
    p <- grid.arrange(p1,p2,p3,ncol=3)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    expect_equal( "ggplot" %in% class(p3), TRUE)
})


test_that("test 6c: statistics of central tendency mean, median and gmean", {
    library(gridExtra)
    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)",  
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 5, rho = 0.8) )
    p1 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "CI"  ) +
      coord_cartesian( ylim = c(8,30) ) + labs(title="mean") 
    p2 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "median", errorbar = "CI"  ) +
      coord_cartesian( ylim = c(8,30) ) + labs(title="median") 
    p3 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "gmean", errorbar = "CI"  ) +
      coord_cartesian( ylim = c(8,30) ) + labs(title="geometric mean")
    p <- grid.arrange(p1,p2,p3,ncol=3)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    expect_equal( "ggplot" %in% class(p3), TRUE)
})


test_that("test 6d: statistics of dispersion sd and MAD", {
    library(gridExtra)
    dta6 <- GRD( WSFactors = "Moment(3):Hand(2)",  
        Effects = list("Moment" = slope(5), "Hand" = slope(3)),
        SubjectsPerGroup = 6,
        Population = list (mean = 20, stddev = 5, rho = 0.8) )    # functions; SD should be asymmetrical; fisherskew should be about 0
    # fisherkurtosis is less stable; MAD should be about 2.5;
    # gmean requires only positive data;
    p1 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "sd", errorbar = "CI"  )
    p2 <- superbPlot(dta6, 
        WSFactor = c("Moment(3)","Hand(2)"), Quiet=T, 
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "MAD", errorbar = "CI"  )
    p <- grid.arrange(p1,p2,ncol=2)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
})



test_that("test 6e: adding ggplot options", {
    # ggplot options
    p1 <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", errorbar = "CI", gamma = .999,
      adjustments = list(purpose = "difference") )
    p2 <- p1 + 
      # all these are added to the plot
      xlab("Dose per day") + ylab("Tooth Growth after study") + 
      theme_light(base_size=14 )

    expect_equal( "ggplot" %in% class(p2), TRUE)
})

      
test_that("test 6f: adding ggplot options to the error bars, to the points", {
    p <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", errorbar = "CI", gamma = .999,
      adjustments = list(purpose = "difference"),
      # see geom_errorbar for the possible arguments
      errorbarParams = list(width = .2, size = 3, colour = "gray"),
      # see geom_point or geom_bar for possible arguments
      barParams = list(linetype = 3, colour = "black", size = .5)  
    )

    expect_equal( "ggplot" %in% class(p), TRUE)
})


test_that("test 6g: adding ggplot options to the error bars, to the points (bis)", {
    p <- superbPlot(ToothGrowth, BSFactor = c("dose","supp"), variables = "len",
      statistic = "mean", errorbar = "CI", gamma = .999,
      adjustments = list(purpose = "difference"),
      plotStyle = "line",
      # see geom_errorbar for the possible arguments
      errorbarParams = list(width = .02, size = 0.1, colour = "gray"),
      # see geom_point or geom_bar for possible arguments
      pointParams = list(colour = "gray", size = 10.5)  
    )

    expect_equal( "ggplot" %in% class(p), TRUE)
})


#########################################
# TESTS WITH ICC
#########################################

test_that("Explorations for ICC", {
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

    expect_warning( noncluster <- superbPlot(dta99, WSFactor = "moment(3)", 
      adjustments = list(decorrelation="CM"),
      errorbar = "CI", showPlot=T,
      variables = c("DV.1","DV.2","DV.3") 
    )+ labs(title="Without cluster information") + coord_cartesian( ylim = c(40,60) ) )
    expect_warning( yescluster <- superbPlot(dta99, WSFactor = "moment(3)", 
      adjustments = list(decorrelation="CM", samplingDesign = "CRS"),
      clusterColumn = "myclus",
      errorbar = "CI", showPlot=T,
      variables = c("DV.1","DV.2","DV.3") 
    )+ labs(title="with cluster information") + coord_cartesian( ylim = c(40,60) ) )
    p <- grid.arrange(noncluster, yescluster, ncol=2)

    expect_equal( "ggplot" %in% class(noncluster), TRUE)
    expect_equal( "ggplot" %in% class(yescluster), TRUE)
})



########################################################
# latest tests based on dta3 verifying CA and popSize  #
########################################################

test_that("Verifying CA and popSize ", {
    library(gridExtra)
    dta3 <- GRD( BSFactors = "Group(2)", WSFactors = "Moment(3)", 
      SubjectsPerGroup = 5, Population = list (mean = 20, stddev = 5, rho = 0.8),
      Effects = list("Moment" = slope(5)) )
    # write.table(dta3, file = "file3.dat", sep = "\t", col.names = FALSE)

    expect_warning( p1 <- superbPlot(dta3, WSFactor = "Moment(3)", BSFactor = "Group", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE",
        adjustments = list(purpose="single", decorrelation="CM", popSize = Inf ),
        Debug = F ) + labs(title="Infinite populations") )
    expect_warning( p2 <- superbPlot(dta3, WSFactor = "Moment(3)", BSFactor = "Group", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE",
        adjustments = list(purpose="single", decorrelation="CM", popSize = c(Inf,6) ),
        Debug = F ) + labs(title="population of 6 in grp 2") )
    p <- grid.arrange(p1,p2,ncol=2)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
})


#########################################
# testing pre and post processing: OK!
#########################################

test_that("Testing pre and post processing", {
    library(ggplot2)
    library(gridExtra)
    dta9 <- GRD( WSFactors = "Moment(3)", SubjectsPerGroup = 5, 
        Population = list( mean=20, stddev = 5),
        Effects = list("Moment" = slope(3) ) )
    # write.table(dta9, file = "file9.dat", sep = "\t", col.names = FALSE)

    expect_warning( 
        truecm <- superbPlot(dta9, WSFactor = "moment(3)",
          adjustments=list(decorrelation="CM"),
          errorbar = "CI", showPlot=T,
          variables = c("DV.1","DV.2","DV.3") 
        )+ labs(title="With decorrelation = CM") 
    )
    altcm <- superbPlot(dta9, WSFactor = "moment(3)", 
      adjustments=list(decorrelation="none"),
      preprocessfct = "superb:::subject_centering_transform",
      postprocessfct = c("superb:::bias_correction_transform"),
      errorbar = "CI", showPlot=T,
      variables = c("DV.1","DV.2","DV.3") 
    )+ labs(title="with pre and post processing")
    pcm <- grid.arrange(truecm,altcm,ncol=2)

    expect_warning( 
        truelm <- superbPlot(dta9, WSFactor = "moment(3)", 
          adjustments=list(decorrelation="LM"),
          errorbar = "CI", showPlot=T,
          variables = c("DV.1","DV.2","DV.3") 
        )+ labs(title="with decorrelation = LM") 
    )
    altlm <- superbPlot(dta9, WSFactor = "moment(3)", 
      adjustments=list(decorrelation="none"),
      preprocessfct = "subject_centering_transform",
      postprocessfct = c("bias_correction_transform","pool_sd_transform"),
      errorbar = "CI", showPlot=T,
      variables = c("DV.1","DV.2","DV.3") 
    )+ labs(title="with pre and post processing")
    plm <- grid.arrange(truelm,altlm,ncol=2)

    expect_warning( 
        truecmvslm <- superbPlot(dta9, WSFactor = "moment(3)", 
          adjustments=list(decorrelation="LM"),
          errorbar = "CI", showPlot=T,
          variables = c("DV.1","DV.2","DV.3") 
        )+ labs(title="with decorrelation = LM") 
    )
    expect_warning( altcmvslm <- superbPlot(dta9, WSFactor = "moment(3)", 
        adjustments=list(decorrelation="CM"),
        postprocessfct = c("pool_sd_transform"),
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
})



