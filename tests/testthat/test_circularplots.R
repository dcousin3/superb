context("Testing circular plots")


test_that("Simplest tests", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    set.seed(663)
    dta1 <- GRD(SubjectsPerGroup = 20,
        BSFactors = "groupe(5)",
        Population = list(mean=75,stddev=15))

    # bars has to start at zero.
    p1 <- superb( DV ~ groupe, dta1,
      errorbarParams = list(width = 0.5) ) 
    print(p1)
    p1 + aes(fill=groupe) + coord_polar()
    expect_equal( "ggplot" %in% class(p1), TRUE)
    
    
  
  # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})




test_that("Tests with 2 factors", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    set.seed(663)
    dta2a <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(5)","genre(3)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))
    dta2b <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(a1,x23,t234,b4e3,llka)","genre(H,F,Q)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))
    dta2c <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(a1,x23,t234,b4e3,llka)","genre(H,F,Q)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre*groupe" = custom(-30,0,30,10,10,10,30,-20,30,0,0,0,0,0,0)))
    dta2c$groupe <- factor(dta2c$groupe)

    # two factors    
    p2a <- superb( DV ~ groupe+genre, dta2a,
      errorbarParams = list(width = 0.5, position = position_dodge(.9)) )    
    p2a + coord_polar()
    print(p2a)

    p2b <- superb( DV ~ groupe+genre, dta2a,
        plotStyle="line",
        errorbarParams = list(width = 0.5) )    
    p2b + ylim(0, NA) + coord_polar()
    print(p2b)

    expect_equal( "ggplot" %in% class(p2a), TRUE)
    expect_equal( "ggplot" %in% class(p2b), TRUE)
  
  # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Tests with 2 factors (bis)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    set.seed(663)
    dta2a <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(5)","genre(3)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))
    dta2b <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(a1,x23,t234,b4e3,llka)","genre(H,F,Q)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))
    dta2c <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(a1,x23,t234,b4e3,llka)","genre(H,F,Q)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre*groupe" = custom(-30,0,30,10,10,10,30,-20,30,0,0,0,0,0,0)))
    dta2c$groupe <- factor(dta2c$groupe)

    pd <- superbData(dta2a, 
      BSFactors = c("groupe","genre"), variables = "DV"
    )
    p2 <- superbPlot.circularline(pd$summaryStatistic,
       "groupe", "genre", ".~.", pd$rawData)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    print(p2)


    # As a test, compile the data manually
    # library(ggradar) # only takes wide summary data frame...
    library(dplyr)
    mns <- dta2a %>% group_by(genre, groupe ) %>% summarise(mns = mean(DV))
    mns <- data.frame(mns)
    mnw <- reshape(mns, idvar = "genre", timevar = "groupe", 
                    direction = "wide")
    #g1 <- suppressWarnings(ggradar(mnw)) #generates plot.data >  grid.max warning.
    # This works fine, but the loop is not closed
    g2 <- superb( DV ~ groupe+genre, dta2a,
        plotStyle="circularline",
        errorbarParams = list(width = 0.2) 
    ) 
    #print(g1)
    print(g2)
    expect_equal( "ggplot" %in% class(g2), TRUE)
  

  # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Tests with 2 factors (ter)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    set.seed(663)
    dta1 <- GRD(SubjectsPerGroup = 20,
        BSFactors = "groupe(5)",
        Population = list(mean=75,stddev=15))
    dta2a <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(5)","genre(3)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))
    dta2b <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(a1,x23,t234,b4e3,llka)","genre(H,F,Q)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))
    dta2c <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(a1,x23,t234,b4e3,llka)","genre(H,F,Q)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre*groupe" = custom(-30,0,30,10,10,10,30,-20,30,0,0,0,0,0,0)))
    dta2c$groupe <- factor(dta2c$groupe)

    ## checks when x are numeric (dta2a) strings (dta2b) factor (dta2c)
    makeplt <- function(dta, xfactor, groupingfactor) {
        superbPlot( 
            variables = "DV",
            BSFactors = c(xfactor, groupingfactor ), 
            data = dta,
            plotStyle="circularline",
            errorbarParams = list(width = 0.2) 
        )
    }
    p1 <- makeplt(dta1, "groupe", NULL)
    p2 <- makeplt(dta2a,"groupe","genre")
    p3 <- makeplt(dta2b,"groupe","genre")
    p4 <- makeplt(dta2c,"groupe","genre")
    print(p1)
    print(p2)
    print(p3)
    print(p4)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    expect_equal( "ggplot" %in% class(p3), TRUE)
    expect_equal( "ggplot" %in% class(p4), TRUE)

    pa <- superb( DV ~ groupe + genre, data = dta2a,
            factorOrder = c("genre","groupe"),
            plotStyle="circularline",
            errorbarParams = list(width = 0.2) 
        )
    pb <- makeplt(dta2a,"genre","groupe")
    print(pa)
    print(pb)

    expect_equal( "ggplot" %in% class(pa), TRUE)
    expect_equal( "ggplot" %in% class(pb), TRUE)
  

  # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})











test_that("Tests with 3 factors", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    set.seed(663)
    dta3 <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(a1,x23,t234,b4e3,llka)","genre(H,F,Q)"),
        WSFactors = c("moment(2)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))

    p1 <- superb( cbind(DV.1,DV.2) ~ groupe + genre, data = dta3,
            WSFactors = "moment(2)",
            factorOrder = c("groupe","genre","moment"),
            plotStyle="circularline",
            errorbarParams = list(width = 0.2) 
        )
    p2 <- superb( cbind(DV.1,DV.2) ~ groupe + genre, data = dta3,
            WSFactors = "moment(2)",
            factorOrder = c("groupe","moment","genre"),
            plotStyle="circularline",
            errorbarParams = list(width = 0.2) 
        )
    p3 <- superb( cbind(DV.1,DV.2) ~ groupe + genre, data = dta3,
            WSFactors = "moment(2)",
            factorOrder = c("moment","groupe","genre"),
            plotStyle="circularline",
            errorbarParams = list(width = 0.2) 
        )
    print(p1)
    print(p2)
    print(p3)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    expect_equal( "ggplot" %in% class(p3), TRUE)

  

  # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})





test_that("Tests with 4 factors", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    set.seed(663)
    dta4 <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(a1,x23,t234,b4e3,llka)","genre(H,F,Q)"),
        WSFactors = c("moment(2)","winter(6)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))

    # rotate the factor orders to check all combinations...
    p1 <- superb( crange(DV.1.1,DV.2.6) ~ groupe + genre, data = dta4,
            WSFactors = c("moment(2)","winter(6)"),
            factorOrder = c("moment","groupe","genre","winter"),
            plotStyle="circularline",
            errorbarParams = list(width = 0.2)  )
    p2 <- superb( crange(DV.1.1,DV.2.6) ~ groupe + genre, data = dta4,
            WSFactors = c("moment(2)","winter(6)"),
            factorOrder = c("groupe","genre","winter","moment"),
            plotStyle="circularline",
            errorbarParams = list(width = 0.2)  )
    p3 <- superb( crange(DV.1.1,DV.2.6) ~ groupe + genre, data = dta4,
            WSFactors = c("moment(2)","winter(6)"),
            factorOrder = c("genre","winter","moment","groupe"),
            plotStyle="circularline",
            errorbarParams = list(width = 0.2)  )
    p4 <- superb( crange(DV.1.1,DV.2.6) ~ groupe + genre, data = dta4,
            WSFactors = c("moment(2)","winter(6)"),
            factorOrder = c("winter","moment","groupe","genre"),
            plotStyle="circularline",
            errorbarParams = list(width = 0.2)  )
    print(p1)
    print(p2)
    print(p3)
    print(p4)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    expect_equal( "ggplot" %in% class(p3), TRUE)
    expect_equal( "ggplot" %in% class(p4), TRUE)
  

  # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})



test_that("Tests of radarParams", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    set.seed(663)
    dta2a <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(5)","genre(3)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))

    p1 <- superb( DV ~ groupe + genre, data = dta2a,
            factorOrder = c("groupe","genre"),
            plotStyle="circularpoint"
    )
    p2 <- superb( DV ~ groupe + genre, data = dta2a,
            factorOrder = c("groupe","genre"),
            plotStyle="circularlineBand"
        )
    p3 <- superb( DV ~ groupe + genre, data = dta2a,
            factorOrder = c("groupe","genre"),
            plotStyle="circularlineBand",
            radarParams = list(direction = -1)
        )
    p4 <- superb( DV ~ groupe + genre, data = dta2a,
            factorOrder = c("groupe","genre"),
            plotStyle="circularlineBand",
            radarParams = list(start = 2*pi * 4/5 )
        )
    p5 <- superb( DV ~ groupe + genre, data = dta2a,
            factorOrder = c("groupe","genre"),
            plotStyle="circularlineBand",
            radarParams = list( direction =-1, start = 2*pi * 4/5)
        )
    print(p1)
    print(p2)
    print(p3)
    print(p4)
    print(p5)
    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
    expect_equal( "ggplot" %in% class(p3), TRUE)
    expect_equal( "ggplot" %in% class(p4), TRUE)
    expect_equal( "ggplot" %in% class(p5), TRUE)
  

  # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})




test_that("Tests of radarParams", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    set.seed(663)
    dta2a <- GRD(SubjectsPerGroup = 20,
        BSFactors = c("groupe(5)","genre(3)"),
        Population = list(mean=75,stddev=15),
        Effects = list("genre" = custom(-30,0,30)))

    # ICI...
    p1 <- superb( DV ~ groupe + genre, data = dta2a,
            factorOrder = c("groupe","genre"),
            plotStyle="circularpointjitter"
        )
    p2 <- superb( DV ~ groupe + genre, data = dta2a,
            factorOrder = c("groupe","genre"),
            plotStyle="circularpointlinejitter"
        ) + theme_bw()
    print(p1)
    print(p2)
        
    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
  

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})
