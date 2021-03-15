######################################################################################
#' @title MauchlySphericityTest
#'
#' @description Performs a test of sphericity on a dataframe with
#'   multiple measures, one subject per line. It assesses the significance of the 
#'   null hypothesis that the covariance matrix is spherical. 
#'   This test is described in \insertCite{a10}{superb}
#'
#' @param dta A data frame containing within-subject measures, one participant per line; 
#' @param cols A vector indicating the columns containing the measures. 
#'
#' @return p the p-value of the null hypothesis that the data are spherical.
#'
#' @references
#'      \insertAllCited{}
#'
#'
#' @examples
#' # creates a small data frames with 4 subject's scores for 5 measures:
#' dta <- data.frame(cbind(
#'         col1 <- c(3., 6., 2., 2., 5.),
#'         col2 <- c(4., 5., 4., 4., 3.),
#'         col3 <- c(2., 7., 7., 8., 6.),
#'         col4 <- c(6., 8., 4., 6., 5.)
#'     ))
#' # performs the test (here p = 0.5824)
#' MauchlySphericityTest(dta)
#' # 0.582443
#'
#' @export MauchlySphericityTest

MauchlySphericityTest <- function(dta, cols) {
	# This function requires a data frame dta as input with the 
	# repeated-measure variables cols. It assesses the significance of the 
	# null hypothesis that the covariance matrix is spherical. 
	# This test is described in Abdi, The Greenhouse-Geisser Correction.
    # In Neil Salkind (Ed.), Encyclopedia of Research Design.
    # Thousand Oaks, CA: Sage. 2010
    X   <- dta[cols]

	# Get basic descriptive statistics
	p   <- length(X)
	n   <- dim(X)[1]
	S   <- cov(X)
    Sij <- t(t(S-rowMeans(S)) -colMeans(S)) + mean(rowMeans(S))
    lam <- eigen(Sij)$values

    W   <- prod(lam[1:p-1])/(1/(p-1) * sum(lam[1:p-1]))^(p-1)
    f   <- (2 * (p - 1)^2 + p + 1)/(6 * (p - 1) * (n - 1))  
    df  <- p * (p - 1)/2 - 1

    suppressWarnings(if(is.na(log(W)))
        chiW <- -(1 - f) * (n - 1) * log(abs(W))
    else
        chiW <- -(1 - f) * (n - 1) * log(W)
    )
    
    pW   <- 1 - pchisq(chiW, df)
  
    return( pW )
}