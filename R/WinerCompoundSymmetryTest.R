######################################################################################
#' @title WinerCompoundSymmetryTest
#'
#' @description Run a test of compound symmetry.
#'   generates a data frame of random data suitable for analyses.
#'   It assesses the significance of the null hypothesis that
#'   the covariance matrix is compound symmetric. 
#'   This test is given without demonstration in 
#'   \insertCite{w91}{superb}, p. 517.
#'
#'
#' @param dta A data frame containing within-subject measures, one participant per line; 
#' @param cols A vector indicating the columns containing the measures. 
#'
#' @return p the p-value of the null hypothesis that the data are compound symmetric.
#'
#' @references
#'      \insertAllCited{}
#'
#' @examples
#' # creates a small data frames with 4 subject's scores for 5 measures:
#' dta <- data.frame(cbind(
#'         col1 <- c(3., 6., 2., 2., 5.),
#'         col2 <- c(4., 5., 4., 4., 3.),
#'         col3 <- c(2., 7., 7., 8., 6.),
#'         col4 <- c(6., 8., 4., 6., 5.)
#'     ))
#' # performs the test (here p = 0.6733)
#' WinerCompoundSymmetryTest(dta)
#' # 0.6733123
#'
#' @export WinerCompoundSymmetryTest

WinerCompoundSymmetryTest <- function(dta, cols) {
	# This function requires a data frame dta as input with the 
	# repeated-measure variables cols. It assesses the significance of the 
	# null hypothesis that the covariance matrix is compound symmetric. 
	# This test is given without demonstration in 
	# Winer, Browns, & Michels, 1991, p. 517.
    # It computes the statistic M, the test statistics W(df), its p value
    # Herein, only the p-value is returned.

    X  <- dta[cols]
	# Get basic descriptive statistics
	p  <- length(X)
	n  <- dim(X)[1]
	S1 <- cov(X)

	# get H0 statistics
	vbar <- mean(diag(S1))
	cbar <- mean(S1[upper.tri(S1)])
	S0   <- vbar * diag(p) + (1-diag(p)) * cbar
	# the chi-square test corrected for small sample; 
	# M is a shortcut for the likelihood ratio
	# cf is a correction factor for small samples
	# df is the degree of freedom of the test distribution

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
