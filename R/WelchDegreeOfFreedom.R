######################################################################################
#' @title Welch's rectified degree of freedom
#'
#' @md
#'
#' @description When variance across groups are heterogeneous,
#'   the Student ``t`` distribution with ``n - 1`` df is not the exact distribution. 
#'   However, \insertCite{w47}{superb}, using methods of moments, was able to find the 
#'   best-fitting ``t`` distribution. This distribution has degree of freedom reduced
#'   based on the sample sizes and the variances of the group tests. The present
#'   function returns the rectified degree of freedom
#'
#' @param dta A data frame containing within-subject measures, one participant per line; 
#' @param cols A vector indicating the columns containing the measures. 
#' @param groupingcols A vector indicating the columns containing the groups. 
#'
#' @return df the degrees of freedom rectified according to Welch (1947).
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # creates a small data frames with 4 subject's scores for 5 measures:
#' dta <- data.frame(cbind(
#'         DV.1 = c(3., 6., 2., 2., 5.),
#'         DV.2 = c(4., 5., 4., 4., 3.),
#'         DV.3 = c(2., 7., 7., 8., 6.),
#'         DV.4 = c(6., 8., 4., 6., 5.),
#'         grp  = c(1., 1., 2., 2., 2.)
#'     ))
#' # performs the test (here rectified df = 1.898876)
#' WelchDegreeOfFreedom(dta, "DV.1","grp")
#'
#' @export WelchDegreeOfFreedom
#'
WelchDegreeOfFreedom <- function(dta, cols, groupingcols) {
    # get standard deviations and sample sizes per group
    sds <- plyr::ddply(dta, .fun=colSDs, .variables = groupingcols)[,cols]
    ns  <- plyr::ddply(dta, .fun=nrow,   .variables = groupingcols)$V1
    # compute Welch rectified degree of freedom
    wdf <- (sum(sds^2/ns)^2) / (sum(sds^4/(ns^2*(ns-1))))
    wdf
}
