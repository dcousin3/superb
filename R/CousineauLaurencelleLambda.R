######################################################################################
#' @title Cousineau and Laurencelle's lambda correction factor for cluster-randomized sampling
#'
#' @md
#'
#' @description The functions CousineauLaurencelleLambda() returns the correction factor
#'   for cluster-randomized sampling. This correction is then used
#'   in a variety of ways, for example, to get the effective number of
#'   participants (in a power study) or to correct a t-test.
#'   See \insertCite{cl16}{superb}.
#' 
#' @param paramvector A vector with, in that order, the intra-class correlation r, 
#'   the number of clusters, then the number of participants in all the clusters.
#'
#' @return lambda the correction factor for cluster-randomized sampling.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # Example from Cousineau & Laurencelle, 2017, p. 124:
#' CousineauLaurencelleLambda( c(0.2, 5, 20, 20, 20, 20, 20) )
#' # 2.234188  
#'
#' @export CousineauLaurencelleLambda
#


# compute the correction factor as per Cousineau & Laurencelle, 2016, Psyck Methods
CousineauLaurencelleLambda <- function(paramvector) {
    r  <- paramvector[1]          # i.e. ICC
    r  <- max(-0.2, r)
    k  <- paramvector[2]          # number of clusters
    ns <- paramvector[c(-1,-2)]   # drop r and k

    M <- sum(ns^2)
    N <- sum(ns)
    nbar <- mean(ns)
    return(sqrt((1+(M/N-1) * r) / (1 - (nbar-1)/(N-1) * r)))
}


