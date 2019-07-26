#' @export ShroutFleissICC1k
#
#
#
ShroutFleissICC1k <- function(dta, cols, clustercol) {
    # The Shrout and Fleiss ICC(1,k)
    # computes the intra-class correlation
    # for the mean measurement; it assumes that m == k
    SS <- function(v) {
        sum((v[,2] - mean(v[,2]))^2)
    }

    if (is.factor(dta[clustercol])) {
      clu = as.numeric(levels(dta[,clustercol]))[dta[,clustercol]]
    } else {
      clu = dta[clustercol]
    }
    
    SStotal  <- SS(cbind(clu, rowMeans(dta[cols])))
    SSerror  <-  sum(ddply(
                    as.data.frame(cbind(clu,rowMeans(dta[cols]))),
                    .variables = clustercol,
                    .fun = SS
                )[,2])
    SSeffect <- SStotal-SSerror
    dleffect <- length(unique(clu[,1])) - 1
    dlerror  <- length(rowMeans(dta[cols])) - length(unique(clu[,1]))
    MSeffect <- SSeffect / dleffect
    MSerror  <- SSerror / dlerror
    ICC      <- (MSeffect - MSerror)/ MSeffect 
    ICC
}

#ShroutFleissICC1k(as.data.frame(Orange),"circ","Tree")
# ShroutFleissICC1k(as.data.frame(cbind(clus,scores)), "scores", "clus")


