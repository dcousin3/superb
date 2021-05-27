######################################################################################
#' @title Obtain summary statistics with correct error bars.
#'
#' @name superbData
#'
#' @md
#'
#' @description The function `suberbData()` computes standard error or confidence interval for various descriptive 
#'      statistics under various designs, sampling schemes, population size and purposes,
#'      according to the `suberb` framework. See \insertCite{c17}{superb} for more.
#'
#' @param data Dataframe in wide format
#'
#' @param BSFactors The name of the columns containing the between-subject factor(s)
#' @param WSFactors The name of the within-subject factor(s)
#' @param WSDesign the within-subject design if not a full factorial design (default "fullfactorial")
#' @param variables The dependent variable(s)
#'
#' @param statistic The summary statistic function to use
#' @param errorbar The function that computes the error bar. Should be "CI" or "SE" or 
#'      any function name. Defaults to "CI"
#' @param gamma The coverage factor; necessary when errorbar == "CI". Default is 0.95.
#' @param factorOrder Order of factors as shown in the graph (x axis, groups, horizontal 
#'       panels, vertical panels)
#'
#' @param adjustments List of adjustments as described below.
#'   Default is ``adjustments = list(purpose = "single", popSize = Inf, decorrelation = "none",
#'              samplingDesign = "SRS")``
#' @param clusterColumn used in conjunction with samplingDesign = "CRS", indicates which column contains the cluster membership
#'
#' @param preprocessfct  is a transform (or vector of) to be performed first on data matrix of each group
#' @param postprocessfct is a transform (or vector of)
#'
#'
#' @return a list with (1) the summary statistics in summaryStatistics
#'      (2) the raw data in long format in rawData (using numeric levels for 
#'      repeated-measure variables).
#'
#' @details The possible adjustements are the following
#' * popsize: Size of the population under study. Defaults to Inf
#' * purpose: The purpose of the comparisons. Defaults to "single". 
#'      Can be "single", "difference", or "tryon".
#' * decorrelation: Decorrelation method for repeated measure designs. 
#'      Chooses among the methods "CM", "LM", "CA" or "none". Defaults to "none".
#' * samplingDesign: Sampling method to obtain the sample. implemented 
#'          sampling is "SRS" (Simple Randomize Sampling) and "CRS" (Cluster-Randomized Sampling).
#' @md
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # Basic example using a built-in dataframe as data; 
#' # by default, the mean is computed and the error bar are 95% confidence intervals
#' # (it also produces a $rawData dataframe, not shown here)
#' res <- superbData(ToothGrowth, BSFactors = c("dose", "supp"), 
#'   variables = "len") 
#' res$summaryStatistics
#'
#' # Example introducing adjustments for pairwise comparisons 
#' # and assuming that the whole population is limited to 200 persons
#' res <- superbData(ToothGrowth, BSFactors = c("dose", "supp"), 
#'   variables = "len",  
#'   statistic = "median", errorbar = "CI", gamma = .80,
#'   adjustments = list( purpose = "difference", popSize = 200) )
#' res$summaryStatistics
#'
#'
#' @export superbData
#
######################################################################################


superbData <- function(data, 
    BSFactors     = NULL,            # vector of the between-subject factor columns
    WSFactors     = NULL,            # vector of the names of the within-subject factors
    WSDesign      = "fullfactorial", # or ws levels of each variable if not a full factorial ws design
    factorOrder   = NULL,            # order of the factors for plots
    variables,                       # dependent variable name(s)
    statistic     = "mean",          # descriptive statistics
    errorbar      = "CI",            # content of the error bars
    gamma         = 0.95,            # coverage if confidence intervals
    adjustments   = list(
        purpose        = "single",   # is "single" or "difference"
        popSize        = Inf,        # is Inf or a specific positive integer
        decorrelation  = "none",     # is "CM", "LM", "CA" or "none"
        samplingDesign = "SRS"       # is "SRS" or "CRS" (in which case use clusterColumn)
    ),
    preprocessfct = NULL,            # run preprocessing on the matrix
    postprocessfct= NULL,            # run post-processing on the matrix
    clusterColumn = ""               # if samplineScheme = CRS
) {

    ##############################################################################
    # All DONE: just send this to the main function superbPlot with showPlot=FALSE
    ##############################################################################

    results <- superbPlot(data    = data, 
        BSFactors      = BSFactors,
        WSFactors      = WSFactors,
        WSDesign       = WSDesign,
        variables      = variables,  
        statistic      = statistic,  
        errorbar       = errorbar, 
        gamma          = gamma, 
        factorOrder    = factorOrder,
        adjustments    = adjustments,
        clusterColumn  = clusterColumn,
        preprocessfct  = preprocessfct,
        postprocessfct = postprocessfct,
        showPlot       = FALSE
    )    
    summaryStatistics = results[[1]]
    rawData = results[[2]]

#    if(missing(factorOrder))  {factorOrder <- c(WSFactors, BSFactors)}
#    widthfct <- paste(errorbar, statistic, sep = ".")

    # do some renaming of the columns for clearer results
#    verbosecol <- c(
#        statistic,
#        if (errorbar == "SE") c("- 1 * SE", "+ 1 * SE") 
#        else if (errorbar == "CI") c(paste("-", gamma* 100, "% CI width"), paste("+", gamma* 100, "% CI width") ) 
#        else if (errorbar == "PI") c(paste("-", gamma* 100, "% PI width"), paste("+", gamma* 100, "% PI width") ) 
#        else c(paste("-", widthfct), paste("+", widthfct) )
#    )
#    colnames(summaryStatistics)[(length(factorOrder)+1):(length(factorOrder)+3)] <- verbosecol

    return(list(summaryStatistics = summaryStatistics, rawData = rawData) )

}

##################################################################   
# End of suberbData.
##################################################################   
