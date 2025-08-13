context("Testing suberb()")

test_that("TESTS that the string arguments are indeed string-ed", {
	old    <- options() 
	on.exit(options(old)) 

	options("superb.feedback" = 'none')
    set.seed(42)
    dta <- GRD( BSFactors = c("a(2)"))
    plt <- superb( DV ~ a , dta)
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    expect_error( superb( DV ~ a , dta, statistic = mean) )
    expect_error( superb( DV ~ a , dta, errorbar = CI) )
    expect_error( superb( DV ~ a , dta, plotLayout = line ) )
    

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})




test_that("TESTS for the various formats", {
	old    <- options() 
	on.exit(options(old)) 

    library(lsr)
	options("superb.feedback" = 'none')

    # basic test with ToothGrowth
    plt    <- superb( len ~ dose * supp, ToothGrowth)
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    # basic test with sleep as repeated measure
    plt    <- superb( extra ~ group | ID, sleep )
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)
    sleepW <- superbToWide(sleep, "ID",NULL,"group", "extra")
    plt    <- superb( cbind(extra.1,extra.2) ~ ., sleepW, WSFactors = "g(2)" )
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    # basic test with Orange as repeated measure
	names(Orange) <- c("Tree","age","circumference")
    plt     <- superb( circumference ~ age | Tree, Orange)
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)
    orangeW <- superbToWide(Orange, "Tree",NULL,"age", "circumference")
    plt     <- superb( crange(circumference.118, circumference.1582) ~ ., orangeW, WSFactors = "age(7)")
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    # a mixed design (land(2) x age(7))
    land         <- c(rep(1, 21), rep(2,14))
    Orange2      <- Orange
    Orange2$land <- land
    plt          <- superb( circumference ~ land * age | Tree, Orange2)
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    ## WS(2): running multiple tests with GRD here...
    set.seed(42)
    dta <- GRD( WSFactors = c("a(2)"))
    plt <- superb( cbind(DV.1, DV.2) ~ . , dta, WSFactors = c("a(2)"))
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)
    lng <- lsr::wideToLong(dta, within="a", sep=".")
    plt <- superb( DV ~ a | id, lng )
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # 2024.12.30: Did some digging here
    #             WSFactors are understood the other way around...
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    ## WS(2x3): running multiple tests with GRD here...
    dta <- GRD( WSFactors = c("a(2)","b(3)"))
    # produit "a" slow (1.1, 1.2, 1.3, 2.1, ...)
    
    plt1 <- superb( cbind(DV.1.1, DV.1.2,DV.1.3, DV.2.1,DV.2.2, DV.2.3) ~ . , dta, WSFactors = c("a(2)","b(3)"))
    print(plt1); expect_equal( "ggplot" %in% class(plt1), TRUE)
    # attend "a" slow...
    
    plt2 <- superb( crange(DV.1.1, DV.2.3) ~ . , dta, WSFactors = c("a(2)","b(3)"))
    print(plt2); expect_equal( "ggplot" %in% class(plt2), TRUE)
    # attend "a" slow...

    # convert to long
    lng <- lsr::wideToLong(dta, within=c("a","b"), sep=".")

    plt3 <- superb( DV ~ a + b | id, lng )
    print(plt3); expect_equal( "ggplot" %in% class(plt3), TRUE)
    # produit et attend "a" slow

    # check back-conversion...
    wde1 <- superbToWide(lng, "id", NULL, c("a","b"), "DV")
    wde2 <- reshape2::dcast( lng, "id ~ a + b", value.var = "DV" )
    expect_equal(all(dta==wde1), TRUE)
    expect_equal(all(dta==wde2), TRUE)
    # produisent "a" slow

    ## WS(2x3x4): running multiple tests with GRD here...
    dta <- GRD( WSFactors = c("a(2)","b(3)","c(4)"))
    plt <- superb( crange(DV.1.1.1, DV.2.3.4) ~ . , dta, WSFactors = c("a(2)","b(3)","c(4)"))
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)
    lng <- lsr::wideToLong(dta, within=c("a","b","c"), sep=".")
    plt <- superb( DV ~ a + b + c | id, lng )
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    ## BS(2): running multiple tests with GRD here...
    dta <- GRD( BSFactors = c("a(2)"))
    plt <- superb( DV ~ a , dta)
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    ## BS(2): running multiple tests with GRD here...
    dta <- GRD( BSFactors = c("a(2)"), SubjectsPerGroup = c(12,25))
    plt <- superb( DV ~ a , dta)
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    ## BS(2x3): running multiple tests with GRD here...
    dta <- GRD( BSFactors = c("a(2)","b(3)"))
    plt <- superb( DV ~ a + b , dta)
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    ## MX((2)x3): running multiple tests with GRD here...
    dta <- GRD( WSFactors = "a(2)", BSFactors = "b(3)")
    plt <- superb( cbind(DV.1, DV.2) ~ b , dta, WSFactors = "a(2)")
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)
    plt <- superb( crange(DV.1, DV.2) ~ b , dta, WSFactors = "a(2)")
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)
    lng <- lsr::wideToLong(dta, within=c("a"), sep=".")
    plt <- superb( DV ~ a + b | id, lng )
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    ## MX((2)x3): running multiple tests with GRD here...
    dta <- GRD( WSFactors = "a(2)", BSFactors = "b(3)", SubjectsPerGroup = c(12,25,50))
    plt <- superb( cbind(DV.1, DV.2) ~ b , dta, WSFactors = "a(2)")
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)
    plt <- superb( crange(DV.1, DV.2) ~ b , dta, WSFactors = "a(2)")
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)
    lng <- lsr::wideToLong(dta, within=c("a"), sep=".")
    plt <- superb( DV ~ a + b | id, lng )
    print(plt)
    expect_equal( "ggplot" %in% class(plt), TRUE)

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})
