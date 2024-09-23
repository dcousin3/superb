context("Testing suberbData()")


test_that("TESTS (1/4)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    res <- superbData(ToothGrowth, BSFactor = c("dose","supp"), 
        variables = "len",
        statistic = "mean" )
    expect_output( str(res), "List of 2")
    expect_equal( res$summaryStatistics[1,3], 13.23)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("TESTS (2/4)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    res <- superbData(ToothGrowth, BSFactor = c("dose","supp"), 
        variables = "len",
        statistic = "mean", 
        adjustments = list(purpose = "difference")
    ) 
    expect_output( str(res), "List of 2")
    expect_equal( res$summaryStatistics[1,3], 13.23)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("TESTS (3/4)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    res <- superbData(ToothGrowth, BSFactor = c("dose","supp"), 
        variables = "len",
        statistic = "median" )
    expect_output( str(res), "List of 2")
    expect_equal( res$summaryStatistics[1,3], 12.25)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("TESTS (4/4)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    res <- superbData(ToothGrowth, BSFactor = c("dose","supp"), 
        variables = "len",
        statistic = "sd" )
    expect_output( str(res), "List of 2")
    expect_equal( round(res$summaryStatistics[1,3],5), 4.45971)
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

    dta1a <- GRD( BSFactors = "Group(3)", Population = list( mean=10, stddev = 5) )
    res <- superbData(dta1a, BSFactor = "Group", variables = "DV",
      statistic = "mean", errorbar = "SE")
    expect_output( str(res), "List of 2")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 1b: factorielle a grps independants; 3 x 2", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta1b <- GRD( BSFactors = "Group(3): Sex(2)", Population = list( mean=10, stddev = 5))
    res <- superbData(dta1b, BSFactor = c("Group","Sex"), variables = "DV",
      statistic = "mean", errorbar = "SE" )
    expect_output( str(res), "List of 2")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 2a: 1 facteur a 3 mesures repetees; (3)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('warnings'))

    dta2a <- GRD( WSFactors = "Moment(3)", SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5))
    expect_message( res <- superbData(dta2a, WSFactor = "moment(3)", 
      adjustments=list(decorrelation="CA"),
      errorbar = "CI",
      variables = c("DV.1","DV.2","DV.3") 
    ))
    expect_output( str(res), "List of 2")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 2b: 2 facteurs a mesures repetees; (3 x 2)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = "none") # to supress design confirmation; unneeded in tests

    dta2b <- GRD( WSFactors = "Moment(3): Dose(2)", SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5, rho = .80))
    res <- superbData(dta2b, WSFactor = c("moment(3)","Dose(2)"), 
      variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
      statistic="mean", errorbar = "CI", gamma = 0.90,
      adjustments = list(purpose="difference", decorrelation="CM")
    )
    expect_output( str(res), "List of 2")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 3: scheme mixte; 3 x (3)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = c('warnings'))

    dta3 <- GRD( BSFactors = "Group(3)", WSFactors = "Moment(3)", 
      SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5),
      Effects = list("Moment" = slope(5))
    )
    expect_message( res <- superbData(dta3, WSFactor = "Moment(3)", BSFactor = "Group", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE", 
        adjustments = list(purpose="single", decorrelation="CM")
    ))
    expect_output( str(res), "List of 2")
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
    res <- superbData(dta4a, BSFactor = c("Group","Dose"), WSFactor = "Moment(3)", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE",
        adjustments = list(purpose="difference", decorrelation="none"),
        factorOrder = c("Dose","Group","Moment"))
    expect_output( str(res), "List of 2")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("test 5a: scheme a quatre facteurs; 5 x 4 (3 x 2)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = "none") # to supress design confirmation; unneeded in tests

    dta5a <- GRD( BSFactors = "Group(5) : Dose(4)", WSFactors = "Moment(3):Hand(2)", 
        Population = list( mean=10, stddev = 5, rho = .90),
        Effects = list("Moment" = slope(5), "Hand" = slope(10)) )
    res <- superbData(dta5a,
        WSFactor = c("Moment(3)","Hand(2)"), 
        BSFactor= c("Group","Dose"),
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "CI", gamma = .9999,
        adjustments = list(purpose="difference", decorrelation="CM")
    )
    expect_output( str(res), "List of 2")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


