#' @export ShroutFleissICC1
#' @export ShroutFleissICC11
#' @export ShroutFleissICC1k
#' @export lambda
#' @export getKNs
#
#
# A quick sum of the squared differences to the mean function
SS <- function(v) { sum((v[,2] - mean(v[,2]))^2) }

ShroutFleissICC1 <- function(dta, clustercol, cols) {

    if (is.factor(dta[,clustercol])) {
      clusters = as.numeric(levels(dta[,clustercol]))[dta[,clustercol]]
    } else {
      clusters = as.vector(as.matrix(dta[clustercol]))
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
    SSerror <-  sum(ddply(as.data.frame(cbind(clusters,scores)),
                    .variables = "clusters",
                    .fun = SS
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
    # scores MUST be a matrix

    temp        <- as.data.frame(cbind(clusters,rowMeans(multiplescores)))
    names(temp) <- c("clus", "score")
    
    SStotal  <- SS(temp)
    SSerror  <-  sum(ddply(
                    temp,
                    .variables = "clus",
                    .fun = SS
                )[,2])
    SSeffect <- SStotal-SSerror
    dleffect <- length(unique(clusters)) - 1
    dlerror  <- length(clusters) - length(unique(clusters))
    MSeffect <- SSeffect / dleffect
    MSerror  <- SSerror / dlerror
    ICC      <- (MSeffect - MSerror)/ MSeffect 
    ICC
}

# ShroutFleissICC1(as.data.frame(Orange),"circ","Tree")

# compute the correction factor as per Cousineau & Laurencelle, 2015, Psyck Methods
lambda <- function(paramvector) {
    r  <- paramvector[1]          # i.e. ICC
    r  <- max(-0.2, r)
    k  <- paramvector[2]          # number of clusters
    ns <- paramvector[c(-1,-2)]  # drop r and k

    M <- sum(ns^2)
    N <- sum(ns)
    nbar <- mean(ns)
    return(sqrt((1+(M/N-1) * r) / (1 - (nbar-1)/(N-1) * r)))
}

# extract the number of clusters k and the number of subjects per cluster ns.
getKNs<-function(dta, clustercol) {
    k=length(unique(dta[,clustercol]))
    ns=ddply(as.data.frame(dta[clustercol]), .fun = dim, .variables = clustercol)$V1
    return(c(k,ns))
}
