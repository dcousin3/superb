context("Testing GRD")

#################################################
##                                             ##
##     (1) how to use GRD by examples          ##
##                                             ##
#################################################


test_that("This is the minimum specification", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD()
    head(dta)
    tail(dta)
    expect_output( str(hist(dta$DV)), "List of 6" )
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Renaming the dependant variable and setting the group size", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD( RenameDV = "score", SubjectsPerGroup = 200 )
    expect_output( str(hist(dta$score )), "List of 6" )
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Selecting a between-group experimental design...", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    expect_output( str(dta <- GRD( BSFactors = '3',SubjectsPerGroup = 20)), "data.frame")
    expect_output( str( GRD( BSFactors = "3 : 2"),SubjectsPerGroup = 20), "data.frame")
    expect_output( str( GRD( BSFactors = "(yes,no) : (CBT, Control, Exercice)"),SubjectsPerGroup = 20), "data.frame")
    expect_output( str( GRD( BSFactors = "Stress(3)"),SubjectsPerGroup = 20), "data.frame")
    expect_output( str( GRD( WSFactors = "Moment (2)"),SubjectsPerGroup = 20), "data.frame")
    expect_output( str( GRD( BSFactors = "Group(3)", WSFactors = "2 : 3"),SubjectsPerGroup = 20), "data.frame")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Obtaining feedback information (not required) and a summary", {
	old <- options() 
	on.exit(options(old)) 

    # turns on all feedback information
    options("superb.feedback" = 'all')
    expect_output( str( GRD( BSFactors = "Group(3)"),SubjectsPerGroup = 20), "data.frame")
    # turns only summary information on the design (recommended)
    options("superb.feedback" = 'summary')
    expect_output( str( GRD( BSFactors = "Group(3)"),SubjectsPerGroup = 20), "data.frame")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Defining population characteristics (ex. 1/2)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD( 
      RenameDV = "IQ",
	  SubjectsPerGroup = 20,
      Population=list(
        mean=100,  # will set GM to 100
        stddev=15  # will set STDDEV to 15
      ) 
    )
    expect_output( str(hist(dta$IQ)), "List of 6") 
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Defining population characteristics (ex. 2/2)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(lattice)
    dta <- GRD(BSFactors="difficulty(2)", SubjectsPerGroup = 200,
      Population=list(mean=100,stddev=15)
    )
    expect_output( str(histogram(~ DV | difficulty, data = dta)), "List of 45")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Adding effects (ex. 1/5)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(lattice)
    dta <- GRD(BSFactors="difficulty(2)", SubjectsPerGroup = 200,
      Population=list(mean=100,stddev=15),
      Effects = list("difficulty" = extent(50) )
    )
    expect_output( str(histogram(~ DV | difficulty, data = dta)), "List of 45")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Adding effects (ex. 2/5)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(lattice)
    dta <- GRD(BSFactors="difficulty(5)", SubjectsPerGroup = 200,
      Population=list(mean=0,stddev=5), 
      Effects = list("difficulty" = slope(50) )
    )
    expect_output( str(histogram(~ DV | difficulty, data = dta)), "List of 45")
    expect_output( str(hist(dta$DV, breaks=seq(-150,150,by=5) )), "List of 6")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Adding effects (ex. 3/5)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(lattice)
    dta <- GRD(BSFactors="difficulty(3):gender(2)", 
      Population=list(mean=100,stddev=15), SubjectsPerGroup = 200,
      Effects = list(
        "difficulty" = extent(10),
        "gender"=slope(10),
        "difficulty*gender"=custom(-300,+200,-100,0,0,0) 
      ) 
    )
    dta$gender = factor(dta$gender, labels=c("Male","Female"))
    dta$difficulty = factor(dta$difficulty, labels=c("easy","medium","hard"))
    plt <- histogram(~ DV | difficulty + gender, data = dta,
      type="density",breaks=seq(-300,400,by=10)
    )
    expect_output( str(plt), "List of 45")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Adding effects (ex. 4/5)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(lattice)
    dta <- GRD(
      BSFactors = 'Reply(yes, no) : Therapy(CBT, Exercise, Control)',
      SubjectsPerGroup = 200,
      Effects = list(
        "Reply*Therapy"=slope(5) 
      ) 
    )
    plt <- histogram(~ DV | Reply + Therapy, data = dta,
      type="density",breaks=seq(-20,20,by=1)
    )
    expect_output( str(plt), "List of 45")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Adding effects (ex. 5/5)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(lattice)
    # The Rexpression effects are given arbitrary names 
    # instead of factors on which to operate
    dta <- GRD(BSFactors="difficulty(5)", SubjectsPerGroup = 200,
      Population=list(mean=0,stddev=5),
      Effects = list(
        "code1" = Rexpression("if (difficulty ==1) {-50} else {0}"), 
        "code2" = Rexpression("if (difficulty ==3) {+50} else {0}") 
      )
    )
    dta$difficulty = factor(dta$difficulty, labels=c("easy","e-m","medium","m-h","hard"))
    plt <- histogram(~ DV | difficulty, data = dta,
      breaks=seq(min(dta$DV)-5,max(dta$DV)+5,by=2.5)
    )
    # Rexpression can be any expression which can be applied to the 
    # subject "id", the factor(s) values, and the DV itself
    expect_output( str(plt), "List of 45")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Specifying underlying distributions (ex. 1/3)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(SubjectsPerGroup = 200, 
      Population=list(mean=100,stddev=15)
    )
    plt <- hist(dta$DV,breaks=seq(min(dta$DV,40)-5,max(dta$DV,160)+5,by=2.5))
    expect_output( str(plt), "List of 6")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Specifying underlying distributions (ex. 2/3)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(lattice)
    # heterogeneous variances across groups
    dta <- GRD(SubjectsPerGroup = 200, 
      BSFactors = "Group(2)",
      Population=list(
        mean = 100, 
        scores = "rnorm(1, mean = GM, sd = 10 * Group)"
      ) 
    )
    dta$Group = factor(dta$Group, labels=c("compact group","Spread out group"))
    plt <- histogram(~ DV | Group, data = dta,
      type="density", breaks=seq(min(dta$DV)-5,max(dta$DV)+5,by=2.5) )
    expect_output( str(plt), "List of 45")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Specifying underlying distributions (ex. 3/3)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(SubjectsPerGroup = 200, 
      Population=list(
        scores = "rweibull(1, shape=2, scale=40)"
      )
    )
    plt <- hist(dta$DV,breaks=seq(min(dta$DV,5)-5,max(dta$DV,160)+5,by=2.5))
    # When using random number generator, always generate the numbers
    # one by one (so that the first argument must be 1) unless rho is set
    expect_output( str(plt), "List of 6")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Introducing contaminants (1/2)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(SubjectsPerGroup = 200, 
      Population=list(
        mean=100, stddev = 15  
      ), 
      Contaminant=list(
        mean=200, stddev = 15, proportion = 0.10
      )
    )
    plt <- hist(dta$DV,breaks=seq(min(dta$DV,5)-5,max(dta$DV,260)+5,by=2.5))
    expect_output( str(plt), "List of 6")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Introducing contaminants (2/2)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(SubjectsPerGroup = 2000, 
      Population=list(
        mean=100, stddev = 15  
      ), 
      Contaminant=list(
        scores="rweibull(1,shape=2, scale=30)+1.5*GM", proportion = 0.10
      )
    )
    plt <- hist(dta$DV,breaks=seq(min(dta$DV,5)-5,max(dta$DV,260)+5,by=2.5))
    expect_output( str(plt), "List of 6")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("Generating multivariate normal data (1/2)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(lattice)
    dta <- GRD( BSFactors="grp(2)",WSFactors = "Moment (2)", 
       SubjectsPerGroup = 200,
       Population=list(mean=0,stddev=20,rho=-0.85), 
       Contaminant=list(mean=100,stddev=4,rho=-0.99,proportion=0.25)
    )
    dta$grp = factor(dta$grp, labels=c("grp 1","grp 2"))
    plt1 <- histogram(~ DV.1 | grp, data = dta,
      breaks=seq(min(dta$DV.1)-5,max(dta$DV.1)+5,by=2.5) )
    plt2 <- plot(dta$DV.1, dta$DV.2)
    expect_output( str(plt1), "List of 45")
    expect_output( str(plt2), "NULL")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})     


test_that("Generating multivariate normal data (2/2)", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD( BSFactors="grp(2)",WSFactors = "Moment (2)", 
       SubjectsPerGroup = 200,
       Effects = list("grp" = slope(100) ),
       Population=list(mean=0,stddev=20,rho=-0.85), 
       Contaminant=list(mean=100,stddev=4,rho=-0.99,proportion=0.25)
    )
    oldpar <- par(mfrow=c(1,2))
    plot(dta[dta$grp == 1,]$DV.1,dta[dta$grp==1,]$DV.2, ylim=c(-150,150), xlim=c(-150,150))
    plt2 <- plot(dta[dta$grp == 2,]$DV.1,dta[dta$grp==2,]$DV.2, ylim=c(-150,150), xlim=c(-150,150)) 
    expect_output( str(plt2), "NULL")

    # restores parameters
    par(oldpar)
    options("superb.feedback" = c('design','warnings','summary'))
})


#################################################
##                                             ##
##  (2) Testing the examples from the article  ##
##                                             ##
#################################################


test_that("page 4", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD()
    expect_output( str(dta), "data.frame")
    expect_equal( dim(dta), c(100,2))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 5", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD()
    top <- head(dta,2)
    bot <- tail (dta,2)
    plt <- hist(dta$DV)
    expect_equal( dim(dta), c(100,2))
    expect_output( str(top), "data.frame")
    expect_output( str(bot), "data.frame")
    expect_output( str(plt), "List of 6")
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 6", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD( RenameDV = "score")
    expect_output( str(dta), "data.frame")
    expect_equal( dim(dta), c(100,2))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 7", { 
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta1 <- GRD( BSFactors = "3")
    head(dta1,2)
    dta2 <- GRD( BSFactors = "2 : 3")
    dta3 <- GRD( BSFactors = "(yes,no) : (CBT, Control, Exercice)")
    expect_equal( dim(dta1), c(300,3))
    expect_equal( dim(dta2), c(600,4))
    expect_equal( dim(dta3), c(600,4))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 8", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD( BSFactors = "Surgery(2) : Therapy(3)")
    expect_equal( dim(dta), c(600,4))
    dta <- GRD( BSFactors = "Surgery(yes,no) : Therapy(CBT, Control, Exercice)")
    expect_equal( dim(dta), c(600,4))
    dta <- GRD( BSFactors = "(yes,no) : Therapy(3)")
    expect_equal( dim(dta), c(600,4))
    dta <- GRD( WSFactors = "3")
    expect_equal( dim(dta), c(100,4))
    dta <- GRD( WSFactors = "Contrast(Low,Medium,High)")
    head(dta, 2)
    expect_equal( dim(dta), c(100,4))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 9", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(
      BSFactors = "Surgery(yes,no) : Therapy(CBT, Control, Exercice)",
      WSFactors = "Contrast(Low,Medium,High)"  
    )
    expect_equal( dim(dta), c(600,6))
    dta <- GRD( SubjectsPerGroup = 200 )
    expect_equal( dim(dta), c(200,2))
    dta <- GRD( BSFactors = "3", SubjectsPerGroup = c(20,25,50) )
    expect_equal( dim(dta), c(95,3))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 10", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(
      RenameDV = "IQ", 
      Population = list(mean = 100, stddev = 15)
    )
    expect_equal( dim(dta), c(100,2))
    hist(dta$IQ)
    dta <- GRD(
      BSFactors = "Group(2)",
      Population = list(scores = "1")
    )
    expect_equal( dim(dta), c(200,3))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 11", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(
      BSFactors = "Group(2)",
      Population = list(scores = "Group")
    )
    expect_equal( dim(dta), c(200,3))
    dta <- GRD(
      BSFactors = "Group(2)",
      Population = list(
        mean = 100,
        stddev = 15,
        scores = "rnorm(1, mean=GM, sd=STDDEV*Group)"
      )
    )
    expect_equal( dim(dta), c(200,3))
    dta <- GRD(
      BSFactors = "Group(2)",
      Population = list(
        scores = "rnorm(1, mean=100, sd=15*Group)"
      )
    )
    expect_equal( dim(dta), c(200,3))
    library(lawstat)
    tt <- levene.test(dta$DV, dta$Group, location="mean")
    expect_output( str(tt), "List of 5" )
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 12", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD( SubjectsPerGroup = 500,
      RenameDV = "RT", 
      Population = list(
        scores = "rweibull(1, shape=2, scale=40)+250"
      )
    )
    plt <- hist(dta$RT, breaks = seq(250, 425, by = 5) ) 
    expect_output( str(plt), "List of 6" )
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 14", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(
      BSFactors = "Therapy(CBT, Control, Exercice)",
      WSFactors = "Contrast(3)",
      SubjectsPerGroup = 200,
      Effects = list("Therapy" = slope(2) )
    )
    library(lattice)
    plt <- histogram(~ DV.1 | Therapy, data = dta,
      breaks = seq(min(dta$DV.1,-5)-1,max(dta$DV.1,5)+1,by=1), layout=c(3,1) )
    expect_output( str(plt), "List of 45" )

    dta <- GRD(
      BSFactors = "Therapy(CBT, Control, Exercice)",
      WSFactors = "Contrast(3)",
      SubjectsPerGroup = 200,
      Effects = list("Contrast" = extent(4) )
    )
    library(lsr)
    dta2 <- wideToLong(dta, within = c("Contrast"), sep = ".")
    plt <- histogram( ~DV | Contrast, data = dta2, 
      breaks = seq(min(dta$DV,-6)-1,max(dta$DV,6)+1, by=1), layout = c(3,1), aspect = 1,
      ylab="Percent total"
    )
    expect_output( str(plt), "List of 45" )
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 15", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(
      BSFactors = "Therapy(CBT, Control, Exercice)",
      WSFactors = "Contrast(3)",
      SubjectsPerGroup = 200,
      Effects = list("Therapy" = custom(0,0,2) )
    )
    expect_equal( dim(dta), c(600,5))
    dta <- GRD(
      BSFactors = "Therapy(CBT, Control, Exercice)",
      WSFactors = "Contrast(3)",
      SubjectsPerGroup = 200,
      Effects = list("Therapy*Contrast" = slope(10) )
    )
    expect_equal( dim(dta), c(600,5))
    dta <- GRD(
      BSFactors = "Therapy(CBT, Control, Exercice)",
      WSFactors = "Contrast(3)",
      SubjectsPerGroup = 10,
      Effects = list(
        "code1" = Rexpression("if(Therapy  == 'CBT') {-50} else {0}"),
        "code2" = Rexpression("if(Contrast == 3)     {+50} else {0}")
        )
    )
    expect_equal( dim(dta), c(30,5))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("# page 16", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(
      BSFactors = "Therapy(CBT, Control, Exercice)",
      WSFactors = "Contrast(3)",
      SubjectsPerGroup = 10,
      Effects = list(
        "code1" = Rexpression("if(Therapy  == 'CBT') {-50} else {0}"),
        "code2" = Rexpression("if(Contrast == 3)     {+50} else {0}")
        )
    )
    library(lsr)
    dta2 <- wideToLong(dta, within = c("Contrast"), sep = ".")
    library(lattice)
    histogram(~ DV | Contrast + Therapy, data=dta2,
      breaks = seq(min(dta2$DV)-5,max(dta2$DV)+5,by=2.5)
    )

    dta <- GRD(
      BSFactors = "Therapy(CBT, Control, Exercice)",
      WSFactors = "Contrast(3)",
      SubjectsPerGroup = 10,
      Effects = list("code1" = Rexpression("0")) # could be Rexpression("print(id);0") )
    )
    expect_equal( dim(dta), c(30,5))

    dta <- GRD(
      WSFactors = "Difficulty(2)",
      SubjectsPerGroup = 200,
      Population = list(Mean = 0, stddev = 20, rho = 0.5)
    )
    expect_equal( dim(dta), c(200,3))
    plot(dta$DV.1, dta$DV.2)
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 17", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(
      WSFactors = "Difficulty(2)",
      SubjectsPerGroup = 200,
      Population = list(Mean = c(10,2), stddev = c(1,0.2), rho = -0.85)
    )
    plot(dta$DV.1, dta$DV.2)
    expect_equal( dim(dta), c(200,3))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 18", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    library(fMultivar)
    # the parameters of the rmsn distribution are
    # xi, omega, alpha
    dta <- GRD(
      WSFactors = "Difficulty(2)",
      SubjectsPerGroup = 200,
      Population = list(rho = 99,
        scores = 'sn::rmsn(1, c(0,0), as.array(cbind(c(1,0.5),c(0.5,1))), c(2,-6))'
      )
    )
    plot(dta$DV.1, dta$DV.2)
    expect_equal( dim(dta), c(200,3))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 19", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(
      SubjectsPerGroup = 500,
      Population = list(mean = 100, stddev = 15 ),
      Contaminant = list(mean = 200, stddev = 15, proportion = 0.1) 
    )
    hist(dta$DV, breaks = seq(min(dta$DV,5)-5,max(dta$DV,260)+5,by=2.5))
    expect_equal( dim(dta), c(500,2))

    dta <- GRD(
      SubjectsPerGroup = 500,
      Population = list(mean = 100, stddev = 15 ),
      Contaminant = list(scores = 'rweibull(1,shape=1.5,scale=30)+1.5*GM', proportion = 0.1) 
    )
    hist(dta$DV, breaks = seq(min(dta$DV)-5,max(dta$DV)+5,by=2.5))
    expect_equal( dim(dta), c(500,2))

    dta <- GRD( BSFactors="grp(2)",WSFactors="M(2)",
      SubjectsPerGroup = 200,
      Effects = list("grp"=slope(100)),
      Population = list(mean = 0, stddev = 15, rho=-0.85 ),
      Contaminant = list(mean =100, stddev = 4, rho = -0.99, proportion = 0.1) 
    )
    oldpar <- par(mfrow=c(1,2))
    plot(dta[dta$grp ==1,]$DV.1,dta[dta$grp==1,]$DV.2,
      ylim = c(-150,150), xlim = c(-150,150))
    plot(dta[dta$grp ==2,]$DV.1,dta[dta$grp==2,]$DV.2,
      ylim = c(-150,150), xlim = c(-150,150))
    expect_equal( dim(dta), c(400,4))

    # restores parameters
    par(oldpar)
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("page 20", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    dta <- GRD(
      SubjectsPerGroup = 500,
      Population = list(mean = 100, stddev = 15 ),
      Contaminant = list(scores = 'NA', proportion = 0.1) 
    )
    expect_equal( dim(dta), c(500,2))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})



#################################################
##                                             ##
##  (3) Making the figures for the article     ##
##                                             ##
#################################################


test_that("figure 1", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    oldpar <- par(mfrow=c(1,3))
    # panel 1: unaffected (p. 4)
    dta <- GRD( SubjectsPerGroup = 200 )
    hist(dta$DV )
    # panel 2: IQ example (p. 10)
    dta <- GRD( 
      RenameDV = "IQ",
      Population=list(mean=100,stddev=15) 
    )
    hist(dta$IQ)
    # panel 3: weibull (p. 12)
    dta <- GRD(SubjectsPerGroup = 500, 
      RenameDV = "RT",
      Population=list(
        scores = "rweibull(1, shape=2, scale=40)+250"
      )
    )
    hist(dta$RT,breaks=seq(min(dta$DV,245)-5,max(dta$DV,410)+5,by=5))
    expect_equal( dim(dta), c(500,2))

    # restores parameters
    par(oldpar)
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("figure 2:", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    # panel 1: a slope of 2 on Therapy (p. 14)
    dta <- GRD(
      BSFactors = 'Surgery(yes, no) : Therapy(CBT, Control, Exercise)',
      WSFactors = 'Contrast(3)',
      SubjectsPerGroup = 100,
      Effects = list('Therapy' = slope(2)) 
    )
    library(lattice)
    p1 <- histogram(~ DV.1 | Therapy, data = dta, breaks=seq(min(dta$DV,-6)-1,max(dta$DV,6)+1,by=0.5),layout = c(3,1), aspect =1, ylab="Percent total" )
    # panel 2: an extent of 20
    dta <- GRD(
      BSFactors = 'Surgery(no, yes) : Therapy(CBT, Control, Exercise)',
      WSFactors = 'Contrast(3)',
      SubjectsPerGroup = 100,
      Effects = list('Contrast' = extent(4)) 
    )
    library(lsr)
    dta2 <- wideToLong(dta, within = c("Contrast"),sep=".")
    p2 <- histogram(~ DV | Contrast, data = dta2, breaks=seq(min(dta$DV,-6)-1,max(dta$DV,6)+1,by=0.5),layout = c(3,1), aspect =1, ylab="Percent total")

    # panel 3: a custom setting
    dta <- GRD(
      BSFactors = 'Surgery(yes, no) : Therapy(CBT, Control, Exercise)',
      WSFactors = 'Contrast(3)',
      SubjectsPerGroup = 100,
      Effects = list(
        "Therapy"=custom(0,0,2) 
      ) 
    )
    dta2 <- wideToLong(dta, within = c("Contrast"),sep=".")
    p3 <- histogram(~ DV | Therapy, data = dta2, breaks=seq(min(dta$DV,-6)-1,max(dta$DV,6)+1,by=0.5),layout = c(3,1), aspect =1, ylab="Percent total")

    print(p1, position=c(0.00, 0.50, 0.50, 1.00), more=TRUE)
    print(p2, position=c(0.50, 0.50, 1.00, 1.00), more=TRUE)
    print(p3, position=c(0.25, 0.00, 0.75, 0.50))
    expect_equal( dim(dta), c(600,6))
    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("figure 3:", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    oldpar <- par(mfrow=c(1,3))
    # panel 1: multivariate normal
    dta <- GRD( 
      WSFactors = 'Difficulty(1,2)',
      SubjectsPerGroup = 200,
      Population=list(mean=0,stddev=20,rho=0.5)
    )
    plot(dta$DV.1, dta$DV.2)
    # panel 2: multivariate normal
    dta <- GRD( 
      WSFactors = 'Difficulty(1,2)',
      SubjectsPerGroup = 200,
      Population=list(mean=c(10,2),stddev=c(1,0.2),rho=-0.85)
    )
    plot(dta$DV.1, dta$DV.2)
    # panel 3: multivariate skew normal 
    library(fMultivar)
    # the parameters of the rmsn distribution are
    # xi, omega, alpha

    dta <- GRD( 
      WSFactors = 'Difficulty(1, 2)',
      SubjectsPerGroup = 200,
      Population=list(rho=99,scores="sn::rmsn(1, c(0,0), as.array(cbind(c(1,0.5),c(0.5,1))), c(2,-6) )")
    )
    plot(dta$DV.1, dta$DV.2)
    expect_equal( dim(dta), c(200,3))

    # restores parameters
    par(oldpar)
    options("superb.feedback" = c('design','warnings','summary'))
})


test_that("figure 4:", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    oldpar <- par(mfrow=c(1,4))
    # panel 1
    dta <- GRD(SubjectsPerGroup = 500, 
      Population=list( mean=100, stddev = 15 ), 
      Contaminant=list( mean=200, stddev = 15, proportion = 0.10 )
    )
    hist(dta$DV,breaks=seq(min(dta$DV,5)-5,max(dta$DV,260)+5,by=2.5))
    #panel 2
    dta <- GRD(SubjectsPerGroup = 1000, 
      Population=list( mean=100, stddev = 15 ), 
      Contaminant=list(
        scores="rweibull(1,shape=1.5, scale=30)+2*GM", proportion = 0.10
      )
    )
    hist(dta$DV,breaks=seq(min(dta$DV,5)-5,max(dta$DV,360)+5,by=2.5))
    # panel 3
    dta <- GRD( BSFactors="grp(2)",WSFactors = "Moment (2)", 
       SubjectsPerGroup = 200,
       Effects = list("grp" = slope(100) ),
       Population=list(mean=0,stddev=20,rho=-0.85), 
       Contaminant=list(mean=100,stddev=4,rho=-0.99,proportion=0.2),
    )
    plot(dta[dta$grp == 1,]$DV.1,dta[dta$grp==1,]$DV.2, ylim=c(-150,150), xlim=c(-150,150))
    plot(dta[dta$grp == 2,]$DV.1,dta[dta$grp==2,]$DV.2, ylim=c(-150,150), xlim=c(-150,150)) 
    #done
    expect_equal( dim(dta), c(400,4))

    # restores parameters
    par(oldpar)
    options("superb.feedback" = c('design','warnings','summary'))
})


