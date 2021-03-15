##############################################################
#' @title epsilon
#' 
#' @description epsilon is a measure of sphericity created by 
#' Greenhouse and Geiser (1958). The original measure was biased
#' and therefore, Hyunh and Feldt (1976) produced a revised version
#' (note that the 1976 paper contained typos that were uncorrected
#' in SPSS; Lecoutre, 1991)
#' 
#' 
#' @param dta a data.frame
#' @param cols a vector of column names indicating the relevant
#' columns on which to compute epsilon. Any other columns are ignored.
#' 
#' @return returns the Hyunh-Feldt estimate of sphericity epsilon
#' 
#' 
#' @export epsilon
#' 

# from Chartier & Cousineau, TMJ, 2011
epsilon <- function(dta, cols) {
    # takes as input a data frame and column names of the measures
    X     <- dta[cols]

	# Get basic descriptive statistics
	c     <- length(X)
	n     <- dim(X)[1]
	S     <- cov(X)

    # collapse the matrix
    smjj  <- mean(diag(S))
    sm    <- mean(colMeans(S))
    sm2   <- sum(colMeans(S)^2)

    num   <- c^2*(smjj-sm)^2
    denom <- (c-1)*(sum(sum(S^2))-2*c*sm2+c^2*sm^2)
    epsGG <- num / denom
    epsGG
    
}
