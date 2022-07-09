######################################################################################
#' @name twoStepTransform
#'
#' @title two-step transform for subject centering and bias correction
#'
#' @md
#'
#' @description `twoStepTransform` is a transformation that can
#'  be applied to a matrix of data. The resulting matrix is both 
#'  subject-centered and bias corrected, a technique called
#'  the CM technique \insertCite{b12,c05,m08}{superb}
#'
#' @param dta a data.frame containing the data in wide format;
#' @param variables a vector of column names on which the transformation will be applied.
#'     the remaining columns will be left unchanged
#'
#' @return a data.frame of the same form as dta with the variables transformed.
#'
#' This function is useful when passed to the argument `preprocessfct` of `superbPlot()`
#' where it performs a modification of the data matrix.
#'
#' @references
#' \insertAllCited{}
#'
#' @export twoStepTransform
#'
twoStepTransform <- function(dta, variables) {
    # from O'Brien and Cousineau (2014) The Quantitative Methods for Psychology
    # It merges together subjectCenteringTransform and biasCorrectionTransform
    X <- dta[ variables ]
    C <- ncol(X)
    Y <- X - rowMeans(X) + mean(rowMeans(X))
    Z <- sqrt(C / (C - 1)) * (t(Y) - colMeans(Y)) + colMeans(Y)
    Z <- as.data.frame(t(Z))
    dta [ variables ] <- Z
    return(dta)
}


######################################################################################
#' @name subjectCenteringTransform
#' @title subject-centering transform  
#'
#' @md
#'
#' @description `subjectCenteringTransform` is a transformation that can
#'  be applied to a matrix of data. the resulting matrix have means
#'  that are centered on the grand mean, subject-wise \insertCite{c05}{superb}.
#'
#' @param dta a data.frame containing the data in wide format;
#' @param variables a vector of column names on which the transformation will be applied.
#'     the remaining columns will be left unchanged
#'
#' @return a data.frame of the same form as dta with the variables transformed.
#'
#' This function is useful when passed to the argument `preprocessfct` of `superbPlot()`
#' where it performs a modification of the data matrix.
#'
#' @references
#' \insertAllCited{}
#'
#' @export subjectCenteringTransform
#'
subjectCenteringTransform <- function(dta, variables) {
    # from O'Brien and Cousineau (2014) The Quantitative Methods for Psychology
    X <- dta[ variables ]
    C <- ncol(X)
    Y <- X - rowMeans(X) + mean(rowMeans(X))
    dta [ variables ] <- Y
    return(dta)
}


######################################################################################
#' @name biasCorrectionTransform
#'
#' @title bias-correction transform 
#'
#' @md
#'
#' @description `biasCorrectionTransform`  is a transformation that can
#'  be applied to a matrix of data. The resulting matrix's variance
#'  is corrected for bias \insertCite{m08}{superb}
#'
#' @param dta a data.frame containing the data in wide format;
#' @param variables a vector of column names on which the transformation will be applied.
#'     the remaining columns will be left unchanged
#'
#' @return a data.frame of the same form as dta with the variables transformed.
#'
#' This function is useful when passed to the argument `preprocessfct` of `superbPlot()`
#' where it performs a modification of the data matrix.
#'
#' @references
#' \insertAllCited{}
#'
#' @export biasCorrectionTransform 
#'
biasCorrectionTransform <- function(dta, variables) {
    # from O'Brien and Cousineau (2014) The Quantitative Methods for Psychology
    Y <- dta[ variables ]
    C <- ncol(Y)
    Z <- sqrt(C / (C - 1)) * (t(Y) - colMeans(Y)) + colMeans(Y)
    Z <- as.data.frame(t(Z))
    dta [ variables ] <- Z
    return(dta)
}



######################################################################################
#' @name poolSDTransform
#'
#' @title pooled standard deviation transform 
#'
#' @md
#'
#' @description `poolSDTransform`  is a transformations that can
#'  be applied to a matrix of data. The resulting matrix has the column-
#'  standard deviations equal to the pool standard deviations of the 
#'  individual columns, the solution adopted by \insertCite{lm94}{superb}.
#'
#' @param dta a data.frame containing the data in wide format;
#' @param variables a vector of column names on which the transformation will be applied.
#'     the remaining columns will be left unchanged
#'
#' @return a data.frame of the same form as dta with the variables transformed.
#'
#' This function is useful when passed to the argument `preprocessfct` of `superbPlot()`
#' where it performs a modification of the data matrix.
#'
#' @references
#' \insertAllCited{}
#'   
#' @export poolSDTransform 
#'
poolSDTransform <- function(dta, variables) {
    # from Cousineau, in prep.
    Z   <- dta[ variables ]
    sds <- colSDs(Z)
    sdp <- sqrt(mean(sds^2))
    W   <- sdp / sds * (t(Z) - colMeans(Z)) + colMeans(Z)
    W   <- as.data.frame(t(W))
    dta [ variables ] <- W
    return(dta)
}

