context("Testing deprecated arguments")


test_that("TESTS deprecate plotStyle", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none') #shut down all superb-generated messages

	suppressWarnings(library(superb))
    
	# Motivation data for 15 participants over three weeks in wide format:
	dta <- matrix( c(
	  45, 50,  59,
	  47, 58,  64,
	  53, 63,  72,
	  57, 64,  81,
	  58, 67,  86,
	  61, 70,  98,
	  61, 75, 104,
	  63, 79, 100,
	  63, 79,  84,
	  71, 81,  96,
	  72, 83,  82,
	  74, 84,  82,
	  76, 86,  93,
	  84, 90,  85,
	  90, 96,  89
	), ncol=3, byrow=T)
	colnames(dta) <- c("W1", "W2", "W3")

	# Superb throws a message with the deprecated plotStyle argument
	expect_message( plt <- superb(cbind(W1,W2,W3) ~ ., dta, 
			   WSFactors   = "Moment(3)",
			   adjustments = list(purpose = "difference"),
			   plotStyle   = "line"
	) )
    expect_equal( "ggplot" %in% class(plt), TRUE)

	expect_message( plt <- superbPlot(dta, 
			   WSFactors   = "Moment(3)",
			   variables   = c("W1", "W2", "W3"),
			   adjustments = list(purpose = "difference"),
			   plotStyle   = "line"
	) )
    expect_equal( "ggplot" %in% class(plt), TRUE)

	plt <- superb(cbind(W1,W2,W3) ~ ., dta, 
			   WSFactors   = "Moment(3)",
			   adjustments = list(purpose = "difference"),
			   plotLayout  = "line"
	)
    expect_equal( "ggplot" %in% class(plt), TRUE)


    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))

})