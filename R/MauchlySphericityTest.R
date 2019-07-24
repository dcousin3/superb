MauchlySphericityTest <- function(dta, cols) {
	# This function requires a data frame dta as input with the 
	# repeated-measure variables cols. It assesses the significance of the 
	# null hypothesis that the covariance matrix is spherical. 
	# This test is described in Abdi, 2010.
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
    chiW <- -(1 - f) * (n - 1) * log(W)
    
    pW   <- 1 - pchisq(chiW, df)
  
    return( pW )
}