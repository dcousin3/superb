context("Testing compatibility of suberbPlot with other structures")


test_that("TESTS (1/1)", {
    options(superb.feedback = 'none') #shut down all superb-generated messages

	library(tibble)

	# Motivation data for 15 participants over three weeks in wide format:
	tib <- matrix( c(
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

	# put column names then convert to tibble:
	colnames(tib) <- c("Week 1", "Week 2", "Week 3")
	tib           <- as_tibble(tib)

	# Superb throws an error when the data frame is a tibble
	plt <- superbPlot(tib, 
			   WSFactors = "Moment(3)",
			   variables = c("Week 1", "Week 2", "Week 3"),
			   adjustments = list(purpose = "difference"),
			   plotStyle="line"
	)
    expect_equal( "ggplot" %in% class(plt), TRUE)

	dta <- superbData(as.data.frame(tib), 
			   WSFactors = "Moment(3)",
			   variables = c("Week 1", "Week 2", "Week 3"),
			   adjustments = list(purpose = "difference")
	)
    expect_equal( "list" %in% class(dta), TRUE)
    expect_output( str(dta), "List of 2")


    # restores default information
    options(superb.feedback = c('design','warnings','summary'))

})