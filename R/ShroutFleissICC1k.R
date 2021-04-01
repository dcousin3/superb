######################################################################################
#' @title Shrout and Fleiss intra-class correlation functions
#'
#' @aliases ShroutFleissICC1 ShroutFleissICC11 ShroutFleissICC1k
#'
#' @md
#'
#' @description The functions ShroutFleissICC1, ShroutFleissICC11
#'   and ShroutFleissICC1k computes the intra-class correlation ICC
#'   for a given data frame containing repeated measures in columns cols
#'   when the measures are in distinct clusters, identified in column clustercol.
#'   See \insertCite{sf79}{superb}.
#' 
#' @param dta A data frame containing within-subject measures, one participant per line; 
#' @param clustercol is the column index where cluster belonging are given;
#' @param cols A vector indicating the columns containing the measures. 
#'
#' @return ICC the intra-class measure of association.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # creates a small data frames with 4 subject's scores for 5 measures:
#' dta <- data.frame(cbind(
#'         clus <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
#'         col1 <- c(2, 4, 4, 6, 4, 5, 8, 8, 5, 8, 9, 9)
#'     ))
#' 
#' ShroutFleissICC1(dta, 1, 2)
#' # 0.434343434 
#' ShroutFleissICC11(dta[, 1], dta[,2])
#' # 0.434343434 
#' 
#' dta2 <- data.frame(cbind(
#'         clus <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
#'         col1 <- c(1, 3, 3, 5, 3, 4, 7, 7, 4, 7, 8, 8),
#'         col1 <- c(2, 4, 4, 6, 4, 5, 8, 8, 5, 8, 9, 9),
#'         col1 <- c(3, 5, 5, 7, 5, 6, 9, 9, 6, 9, 10, 10)
#'     ))
#'  
#' ShroutFleissICC1(dta2, 1, 2:4)
#' # 0.7543859649 
#' ShroutFleissICC1k(dta2[, 1], dta2[,2:4])
#' # 0.7543859649 
#'   
#' @references
#' \insertAllCited{}
#'
#' @export ShroutFleissICC1
#' @export ShroutFleissICC11
#' @export ShroutFleissICC1k
#

ShroutFleissICC1 <- function(dta, clustercol, cols) {

    if (is.factor(dta[,clustercol])) {
      clusters = as.numeric(levels(dta[,clustercol]))[dta[,clustercol]]
    } else {
      clusters = as.numeric(as.vector(as.matrix(dta[clustercol])))
    }

    if (length(cols)==1)
        # computes ICC on raw data
        ShroutFleissICC11(clusters, as.vector(as.matrix(dta[cols])) )
    else
        # computes ICC on multiple columns that will be averaged
        ShroutFleissICC1k(clusters, as.matrix(dta[cols]))
}


ShroutFleissICC11 <- function(clusters, scores) {
    # The Shrout and Fleiss ICC(1,1)
    # computes the intra-class correlation
    # for a measurement.
    # clusters MUST be a vector
    # scores MUST be a vector
    SStotal <- SS(cbind(clusters, scores))
    SSerror <-  sum(
                    plyr::ddply(as.data.frame(cbind(clusters,scores)),
                    .variables = "clusters", .fun = SS
                )[,2])
    SSeffect <- SStotal-SSerror
    dleffect <- length(unique(clusters)) - 1
    dlerror  <- length(scores) - length(unique(clusters))
    MSeffect <- SSeffect / dleffect
    MSerror  <- SSerror / dlerror
    n <- length(scores)/ length(unique(clusters))
    ICC = (MSeffect - MSerror)/ (MSeffect + (n-1)*MSerror)
    ICC
}


ShroutFleissICC1k <- function(clusters, multiplescores) {
    # The Shrout and Fleiss ICC(1,k)
    # computes the intra-class correlation
    # for the mean measurement; it assumes that m == k.
    # clusters MUST be a vector
    # multiplescores MUST be a matrix

    temp        <- as.data.frame( cbind(clusters,rowMeans(multiplescores)) )
    names(temp) <- c("clus", "score")
    
    SStotal  <- SS(temp)
    SSerror  <-  sum(
                    plyr::ddply( temp, .variables = "clus", .fun = SS
                )[,2])
    SSeffect <- SStotal-SSerror
    dleffect <- length(unique(clusters)) - 1
    dlerror  <- length(clusters) - length(unique(clusters))
    MSeffect <- SSeffect / dleffect
    MSerror  <- SSerror / dlerror
    ICC      <- (MSeffect - MSerror)/ MSeffect 
    ICC
}


######################################################################
## subsidiary functions
######################################################################

# A quick sum of the squared differences to the mean function
SS <- function(v) {
    sum((v[,2] - mean(v[,2]))^2) 
}


# compute the correction factor as per Cousineau & Laurencelle, 2016, Psyck Methods
lambda <- function(paramvector) {
    r  <- paramvector[1]          # i.e. ICC
    r  <- max(-0.2, r)
    k  <- paramvector[2]          # number of clusters
    ns <- paramvector[c(-1,-2)]   # drop r and k

    M <- sum(ns^2)
    N <- sum(ns)
    nbar <- mean(ns)
    return(sqrt((1+(M/N-1) * r) / (1 - (nbar-1)/(N-1) * r)))
}


# extract the number of clusters k and the number of subjects per cluster ns.
getKNs<-function(dta, clustercol) {
    k  <- length(unique(dta[,clustercol]))
    ns <- plyr::ddply(as.data.frame(dta[clustercol]), .fun = dim, .variables = clustercol)$V1
    return( c(k, ns) )
}
