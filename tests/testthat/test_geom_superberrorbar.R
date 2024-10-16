context("Testing geom_superberrorbar")


test_that("Simplest example", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    ## One easy question
    ## https://stackoverflow.com/q/27585776/5181513

    ## The package superb has a redefined geom_errorbar() 
    ## in which you can ask for error bars pointing "up", 
    ## "down" or (default) "both" directions.

    df <- data.frame(trt = factor(c(1, 1, 2, 2)), resp = c(1, 5, 3, 4),
                     group = factor(c(1, 2, 1, 2)), se = c(0.1, 0.3, 0.3, 0.2))

    #library(superb)
    p <- ggplot(df, aes(fill = group, y = resp, x = trt)) +
        geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
        geom_superberrorbar( aes(ymax = resp + se, ymin = resp - se), 
            position = position_dodge(width = 0.9), 
            width = 0.25, pointing="up")

    ### This solution is almost identical to the original code 
    ### except that the geom is changed to geom_superberrorbar() 
    ### and the argument pointing is added.

    expect_equal( "ggplot" %in% class(p), TRUE)
  
  # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})




test_that("Advanced example", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none')

    ### https://stackoverflow.com/q/29475701/5181513
    ### https://stackoverflow.com/questions/29475701
    library(superb)
    library("ggplot2")
    library(dplyr)

    set.seed(1)
    dat <- data.frame(Trial = c(rep("Pre",9),rep("Post",9)), 
                      Time = rep.int(seq(0,120,15),2), 
                      Insulin = c(rnorm(9,15,2),rnorm(9,22,2)),
                      Insulin_sd = c(rnorm(18,3,1)))
    dat <- mutate(dat, point = ifelse(Trial == "Pre","down","up"))

    ## bars pointing up             
    p1 <- ggplot(data = dat, 
                aes(x = Time, y = Insulin, group = Trial)) +
            geom_line(aes(linetype = Trial)) +  
            geom_point(aes(shape= Trial, fill = Trial), size=2) +
            geom_superberrorbar(aes(ymin=Insulin-Insulin_sd, 
                                    ymax=Insulin+Insulin_sd),
                                pointing = "up", 
                                width = 4)

    ## bars alternating
    p2 <- ggplot(data = dat, 
                aes(x = Time, y = Insulin, group = Trial)) +
            geom_line(aes(linetype = Trial)) +  
            geom_point(aes(shape= Trial, fill = Trial), size=2) +
            geom_superberrorbar(aes(ymin=Insulin-Insulin_sd, 
                            ymax=Insulin+Insulin_sd,
                            pointing = point), 
                width = 4)

    expect_equal( "ggplot" %in% class(p1), TRUE)
    expect_equal( "ggplot" %in% class(p2), TRUE)
  
  # restores default information
    options("superb.feedback" = c('design','warnings','summary'))
})