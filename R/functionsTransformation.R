######################################################################################
#' @name twoStepTransform
#'
#' @title twoStepTransform
#'
#' @description twoStepTransform, is a  
#' transformation that can be applied to a matrix of data.
#'
#' @param dta a data.frame containing the data in wide format;
#' @param variables a vector of column names on which the transformation will be applied.
#'     the remaining columns will be left unchanged
#'
#' @return a data.frame of the same form as dta with the variables transformed.
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
    dta [ variables ] = Z
    return(dta)
}


######################################################################################
#' @name subjectCenteringTransform
#' @title subjectCenteringTransform  
#'
#' @description subjectCenteringTransform is a 
#' transformations that can be applied to a matrix of data.
#'
#' @param dta a data.frame containing the data in wide format;
#' @param variables a vector of column names on which the transformation will be applied.
#'     the remaining columns will be left unchanged
#'
#' @return a data.frame of the same form as dta with the variables transformed.
#' @export subjectCenteringTransform
#'
subjectCenteringTransform <- function(dta, variables) {
    # from O'Brien and Cousineau (2014) The Quantitative Methods for Psychology
    X <- dta[ variables ]
    C <- ncol(X)
    Y <- X - rowMeans(X) + mean(rowMeans(X))
    dta [ variables ] = Y
    return(dta)
}


######################################################################################
#' @name biasCorrectionTransform
#'
#' @title biasCorrectionTransform 
#'
#' @description biasCorrectionTransform  is a 
#' transformations that can be applied to a matrix of data.
#'
#' @param dta a data.frame containing the data in wide format;
#' @param variables a vector of column names on which the transformation will be applied.
#'     the remaining columns will be left unchanged
#'
#' @return a data.frame of the same form as dta with the variables transformed.
#' @export biasCorrectionTransform 
#'
biasCorrectionTransform <- function(dta, variables) {
    # from O'Brien and Cousineau (2014) The Quantitative Methods for Psychology
    Y <- dta[ variables ]
    C <- ncol(Y)
    Z <- sqrt(C / (C - 1)) * (t(Y) - colMeans(Y)) + colMeans(Y)
    Z <- as.data.frame(t(Z))
    dta [ variables ] = Z
    return(dta)
}



######################################################################################
#' @name poolSDTransform
#'
#' @title poolSDTransform 
#'
#' @description poolSDTransform  is a 
#' transformations that can be applied to a matrix of data.
#'
#' @param dta a data.frame containing the data in wide format;
#' @param variables a vector of column names on which the transformation will be applied.
#'     the remaining columns will be left unchanged
#'
#' @return a data.frame of the same form as dta with the variables transformed.
#' @export poolSDTransform 
#'
poolSDTransform <- function(dta, variables) {
    # from Cousineau, in prep.
    Z   <- dta[ variables ]
    sds <- colSDs(Z)
    sdp <- sqrt(mean(sds^2))
    W   <- sdp / sds * (t(Z) - colMeans(Z)) + colMeans(Z)
    W <- as.data.frame(t(W))
    dta [ variables ] = W
    return(dta)
}

