#' @export WinerCompoundSymmetryTest

WinerCompoundSymmetryTest <- function(dta, cols) {
	# This function requires a data frame dta as input with the 
	# repeated-measure variables cols. It assesses the significance of the 
	# null hypothesis that the covariance matrix is compound symmetric. 
	# This test is given without demonstration in 
	# Winer, Browns, & Michels, 1991, p. 517.

    X <- dta[cols]
	# Get basic descriptive statistics
	p <- length(X)
	n <- dim(X)[1]
	S1  <- cov(X)

	# get H0 statistics
	vbar <- mean(diag(S1))
	cbar <- mean(S1[upper.tri(S1)])
	S0 <- vbar * diag(p) + (1-diag(p)) * cbar

	# the chi-square test corrected for small sample; 
	# M is a shortcut for the likelihood ratio
	# cf is a correction factor for small samples
	# df is the degree of freedom of the test distribution

    # M  <- -(n-1) * log( det(S1) / det(S0) )
    suppressWarnings(if(is.na(log(det(S1)/det(S0)))) 
        M  <- +1000
    else
        M  <- -(n-1) * log( det(S1) / det(S0) )
    )
	cf <- (p * (p+1)^2 * (2*p-3) )/(6 * (n-1) * (p-1) * (p^2 + p -4))
	df <- p*(p+1)/2-2
	W  <- M * (1 - cf)
	pW <- 1-pchisq(W, df )
	
	# cat("M =", M, ", W(",df,") = ", W, ", p = ", pW, "\n", sep = "")
    return( pW )
}
