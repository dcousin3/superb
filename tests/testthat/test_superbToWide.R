context("Testing suberbToWide()")


test_that("TESTS (1/4)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

	names(Orange) <- c("Tree","age","DV")
	Owide <- superbToWide(Orange, id = "Tree", WSFactors = "age", variable = "DV" )

    expect_output( str(Owide), "data.frame")
    expect_equal( Owide$DV.118[Owide$Tree[1]], 
        dplyr::filter(Orange, Tree == Owide$Tree[1]& age == 118)$DV)
    expect_equal( Owide$DV.1582[Owide$Tree[5]], 
            dplyr::filter(Orange, Tree == Owide$Tree[5]& age == 1582)$DV)

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("TESTS (2/4)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

	ss <- 3
	dta <- GRD( BSFactors = "Moment (2)", SubjectsPerGroup = ss)
	dta$id = ((dta$id-1)%%ss)+1
	tt <- superbToWide(dta, id = "id", WSFactors = "Moment", variable = "DV" )

    expect_output( str(tt), "data.frame")
    expect_equal( dta$DV[1], tt$DV.1[1])
    expect_equal( dta$DV[ss*2], tt$DV.2[ss])

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("TESTS (3/4)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

	ss <- 7
	dta <- GRD( BSFactors = c("Moment (2)","time(3)","what(5)"), SubjectsPerGroup = ss)
	dta$id = ((dta$id-1)%%(ss))+1
	tt <- superbToWide(dta, id = "id", WSFactors = c("Moment","time","what"), variable = "DV" )

    expect_output( str(tt), "data.frame")
    expect_equal( dta$DV[1], tt$DV.1.1.1[1])
    expect_equal( dta$DV[ss*2*3*5], tt$DV.2.3.5[ss])

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})

test_that("TESTS (4/4)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

	ss <-13
	dta <- GRD( BSFactors = c("Moment (2)","time(3)","what(5)","bis(7)"), SubjectsPerGroup = ss)
	dta$id = ((dta$id-1)%%(ss))+1
	tt <- superbToWide(dta, id = "id", WSFactors = c("Moment","time","what","bis"), variable = "DV" )

    expect_output( str(tt), "data.frame")
    expect_equal( dta$DV[1], tt$DV.1.1.1.1[1])
    expect_equal( dta$DV[ss*2*3*5*7], tt$DV.2.3.5.7[ss])

    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})

test_that("TESTS (5/4...)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    #### emulate within-subject dataset
    dta <- GRD( 
        BSFactors = c("Factor(4)","Replication(2)"),
        Population = list( mean = 100, stddev = 15),
        Effects = list( Factor = extent(10) )
    )
    dta$id <- ((dta$id-1) %% 100)+1

    # works ok as replication is taken into account
    superb( DV ~ Factor + Replication  | id,
        dta,
        adjustments = list (decorrelation = 'none', purpose = 'difference'),
        errorbar = 'CI',
        plotLayout = 'line',
        errorbarParams = list (linewidth = 0.5, color = 'black'),
        pointParams = list (size = 3, color = 'black')
       ) + theme_bw()

    # should return a message as replication is left unspecified
    expect_message( superb( DV ~ Factor  | id,
        dta,
        adjustments = list (decorrelation = 'none', purpose = 'difference'),
        errorbar = 'CI',
        plotLayout = 'line',
        errorbarParams = list (linewidth = 0.5, color = 'black'),
        pointParams = list (size = 3, color = 'black')
       ) + theme_bw() )


    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


