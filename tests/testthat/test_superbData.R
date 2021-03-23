context("Testing suberbData")


test_that("TESTS (1/4)", {
    res <- superbData(ToothGrowth, BSFactor = c("dose","supp"), 
        variables = "len",
        statistic = "mean" )
    expect_output( str(res), "data.frame")
    expect_equal( res[1,3], 13.23)
})


test_that("TESTS (2/4)", {
    res <- superbData(ToothGrowth, BSFactor = c("dose","supp"), 
        variables = "len",
        statistic = "mean", 
        adjustments = list(purpose = "difference")
    ) 
    expect_output( str(res), "data.frame")
    expect_equal( res[1,3], 13.23)
})

test_that("TESTS (3/4)", {
    res <- superbData(ToothGrowth, BSFactor = c("dose","supp"), 
        variables = "len",
        statistic = "median" )
    expect_output( str(res), "data.frame")
    expect_equal( res[1,3], 12.25)
})

test_that("TESTS (4/4)", {
    res <- superbData(ToothGrowth, BSFactor = c("dose","supp"), 
        variables = "len",
        statistic = "sd" )
    expect_output( str(res), "data.frame")
    expect_equal( round(res[1,3],5), 4.45971)
})




#########################################
# SYSTEMATIC TESTS OF THE STATISTICS
#########################################

test_that("test 1a: 3 groupes inpépendants", { 
    dta1a <- GRD( BSFactors = "Group(3)", Population = list( mean=10, stddev = 5) )
    # write.table(dta1a, file = "test1a.dat", sep = "\t", col.names = FALSE)
    res <- superbData(dta1a, BSFactor = "Group", variables = "DV",
      statistic = "mean", errorbar = "SE")
    expect_output( str(res), "data.frame")
})


test_that("test 1b: factorielle à grps indépendants; 3 x 2", {
    dta1b <- GRD( BSFactors = "Group(3): Sex(2)", Population = list( mean=10, stddev = 5))
    # write.table(dta1b, file = "test1b.dat", sep = "\t", col.names = FALSE)
    res <- superbData(dta1b, BSFactor = c("Group","Sex"), variables = "DV",
      statistic = "mean", errorbar = "SE" )
    expect_output( str(res), "data.frame")
})


test_that("test 2a: 1 facteur à 3 mesures répétées; (3)", {
    dta2a <- GRD( WSFactors = "Moment(3)", SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5))
    # write.table(dta2a, file = "test2a.dat", sep = "\t", col.names = FALSE)
    expect_warning( res <- superbData(dta2a, WSFactor = "moment(3)", 
      adjustments=list(decorrelation="CA"),
      errorbar = "CI",
      variables = c("DV.1","DV.2","DV.3") 
    ))
    expect_output( str(res), "data.frame")
})


test_that("test 2b: 2 facteurs à mesures répétées; (3 x 2)", {
    options(superb.debug = "none") # to supress design confirmation; unneeded in tests
    dta2b <- GRD( WSFactors = "Moment(3): Dose(2)", SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5, rho = .80))
    res <- superbData(dta2b, WSFactor = c("moment(3)","Dose(2)"), 
      variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
      statistic="mean", errorbar = "CI", gamma = 0.90,
      adjustments = list(purpose="difference", decorrelation="CM")
    )
    expect_output( str(res), "data.frame")
    options(superb.debug = c("design","warnings") ) # restores to default
})


test_that("test 3: schème mixte; 3 x (3)", {
    dta3 <- GRD( BSFactors = "Group(3)", WSFactors = "Moment(3)", 
      SubjectsPerGroup = 5, Population = list( mean=10, stddev = 5),
      Effects = list("Moment" = slope(5))
    )
    expect_warning( res <- superbData(dta3, WSFactor = "Moment(3)", BSFactor = "Group", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE", 
        adjustments = list(purpose="single", decorrelation="CM")
    ))
    expect_output( str(res), "data.frame")
})


test_that("test 4a: schème à trois facteurs, 2 étant between  3 x 3 x (3)", {
    dta4a <- GRD( BSFactors = "Group(3) : Dose(3)", WSFactors = "Moment(3)", 
      SubjectsPerGroup = 4, Population = list( mean=10, stddev = 5),
      Effects = list("Moment" = slope(5))
    )
    res <- superbData(dta4a, BSFactor = c("Group","Dose"), WSFactor = "Moment(3)", 
        variables = c("DV.1","DV.2","DV.3"), 
        statistic = "mean", errorbar = "SE",
        adjustments = list(purpose="difference", decorrelation="none"),
        factorOrder = c("Dose","Group","Moment"))
    expect_output( str(res), "data.frame")
})
    
  
test_that("test 5a: schème à quatre facteurs; 5 x 4 (3 x 2)", {
    options(superb.debug = "none") # to supress design confirmation; unneeded in tests
    dta5a <- GRD( BSFactors = "Group(5) : Dose(4)", WSFactors = "Moment(3):Hand(2)", 
      Summary=FALSE, Population = list( mean=10, stddev = 5, rho = .90),
      Effects = list("Moment" = slope(5), "Hand" = slope(10)) )
    res <- superbData(dta5a,
        WSFactor = c("Moment(3)","Hand(2)"), 
        BSFactor= c("Group","Dose"),
        variables = c("DV.1.1","DV.2.1","DV.3.1","DV.1.2","DV.2.2","DV.3.2"), 
        statistic = "mean", errorbar = "CI", gamma = .9999,
        adjustments = list(purpose="difference", decorrelation="CM")
    )
    expect_output( str(res), "data.frame")
    options(superb.debug = c("design","warnings") ) # restores to default
})


