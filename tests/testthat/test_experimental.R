context("Testing experimental features")


test_that("TESTS namespace::function in statistical functions", {
	old <- options() 
	on.exit(options(old)) 
    options("superb.feedback" = 'none') #shut down all superb-generated messages

	# whatever data set dta
	dta2 <- GRD( WSFactors = c("Moment(W1,W2,W3)") )

	# Superb throws a message if namespaces do not match
	expect_message( plt1 <- superb(cbind(DV.W1, DV.W2, DV.W3) ~ ., dta2, 
                WSFactors   = "Moment(3)",
                statistic   = "base::mean",
                errorbar    = "toto::CI"
	) )
    expect_equal( "ggplot" %in% class(plt1), TRUE)

    # throws an error if the function does not exists
	expect_error( plt2 <- superb(cbind(DV.W1, DV.W2, DV.W3) ~ ., dta2, 
                WSFactors   = "Moment(3)",
                statistic   = "calisson::mean",
                errorbar    = "CI"
	) )

    # using functions located in a namespace specifically
    require(rlang)
    rlang::env_unlock(asNamespace("superb"))
    assign('fct',      function(x) mean(x),                        asNamespace("superb") ) 
    assign('init.fct', function(x) return(0),                      asNamespace("superb") ) 
    assign('CI.fct',   function(x, gamma) CI.mean(x, gamma = .95), asNamespace("superb") ) 

	expect_message( plt3 <- superb(cbind(DV.W1, DV.W2, DV.W3) ~ ., dta2, 
                WSFactors   = "Moment(3)",
                statistic   = "superb::fct",
                errorbar    = "CI", 
                gamma       = 0.66
	) )
    expect_equal( "ggplot" %in% class(plt3), TRUE)


    # restores default information
    options("superb.feedback" = c('design','warnings','summary'))

})