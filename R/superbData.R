######################################################################################
#' @title superbData to obtain summary statistics with correct error bar intervals.
#'
#' @name superbData
#'
#' @description plotsuberb plots standard error or confidence interval for various descriptive 
#'      statistics under various designs, sampling schemes, population size and purposes,
#'      according to the suberb framework. See \insertCite{c17}{superb} for more.
#'
#' @param data Dataframe in wide format
#' @param BSFactor The name of the columns containing the between-subject factor(s)
#' @param WSFactor The name of the within-subject factor(s)
#' @param factorOrder Order of factors as shown in the graph (x axis, groups, horizontal 
#'       panels, vertical panels)
#' @param variables The dependent variable(s)
#' @param statistic The summary statistic function to use
#' @param errorbar The function that computes the error bar. Should be "CI" or "SE" or 
#'      any function name. Defaults to "SE"
#' @param gamma The coverage factor; necessary when errorbar == "CI". Default is 0.95.
#' @param adjustments List of adjustments as described below:
#'  popsize: Size of the population under study. Defaults to Inf
#'  purpose: The purpose of the comparisons. Defaults to "single". 
#'      Can be "single", "difference" or "tryon".
#' decorrelation: Decorrelation method for repeated measure designs. 
#'      Chooses among the methods ("CM", "LM", "CA" or "none"). Defaults to "none".
#' samplingDesign: Sampling method to obtain the sample. implemented 
#'          sampling is "SRS" (Simple Randomize Sampling) and "CRS" (Cluster-Randomized Sampling).
#' Default is adjustments = list(purpose = "single", popSize = Inf, decorrelation = "none",
#'              samplingDesign = "SRS")
#' @param clusterColumn used in conjunction with samplingDesign = "CRS", indicates which column contains the cluster membership
#' @param preprocessfct  is a transform (or vector of) to be performed first on data matrix of each group
#' @param postprocessfct is a transform (or vector of)
#'
#'
#' @return a list with (1) the summary statistics in summaryStatistics
#'         (2) the raw data in long format, with standardized levels in rawData.
#'
#' @references
#'      \insertAllCited{}
#'
#' @examples
#' # basic example using a built-in dataframe as data; 
#' # by default, the mean is computed and the error bar are 95% confidence intervals
#' superbData(ToothGrowth, BSFactor = c("dose", "supp"), 
#'   variables = "len") 
#'
#' # example changing the summary statistics to the median and
#' # the error bar to 90% confidence intervals
#' superbData(ToothGrowth, BSFactor = c("dose", "supp"), 
#'   variables = "len", statistic = "median", errorbar = "CI", gamma = .90) 
#'
#' # example introducing adjustments for pairwise comparisons 
#' # and assuming that the whole population is limited to 200 persons
#' superbData(ToothGrowth, BSFactor = c("dose", "supp"), 
#'   variables = "len",  
#'   adjustments = list( purpose = "difference", popSize = 200) )
#'
#' # This example is based on repeated measures
#' library(lsr)
#' options(superb.feedback = 'none') # shut down 'warnings' and 'design' interpretation messages
#' 
#' # define shorter column names...
#' names(Orange) <- c("Tree","age","circ")
#' # turn the data into a wide format
#' Orange.wide <- longToWide(Orange, circ ~ age)
#' superbData( Orange.wide, WSFactor = "age(7)",
#'   variables = c("circ_118","circ_484","circ_664","circ_1004","circ_1231","circ_1372","circ_1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "none")
#' )
#' superbData( Orange.wide, WSFactor = "age(7)",
#'   variables = c("circ_118","circ_484","circ_664","circ_1004","circ_1231","circ_1372","circ_1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "CM")
#' )
#'
#'
#' @export superbData
#
######################################################################################


superbData <- function(data, 
    BSFactor      = NULL,            # vector of the between-subject factor columns
    WSFactor      = NULL,            # vector of the names of the within-subject factors
    factorOrder,                     # order of the factors for plots
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
        BSFactor       = BSFactor,
        WSFactor       = WSFactor,
        factorOrder    = factorOrder,
        variables      = variables,  
        statistic      = statistic,  
        errorbar       = errorbar, 
        gamma          = gamma, 
        adjustments    = adjustments,
        preprocessfct  = preprocessfct,
        postprocessfct = postprocessfct,
        clusterColumn  = clusterColumn,
        showPlot       = FALSE
    )    
    summaryStatistics = results[[1]]
    rawData = results[[2]]

    if(missing(factorOrder))  {factorOrder <- c(WSFactor, BSFactor)}
    widthfct <- paste(errorbar, statistic, sep = ".")

    # do some renaming of the columns for clearer results
    verbosecol <- c(
        statistic,
        if (errorbar == "SE") c("- 1 * SE", "+ 1 * SE") 
        else if (errorbar == "CI") c(paste("-", gamma* 100, "% CI width"), paste("+", gamma* 100, "% CI width") ) 
        else if (errorbar == "PI") c(paste("-", gamma* 100, "% PI width"), paste("+", gamma* 100, "% PI width") ) 
        else c(paste("-", widthfct), paste("+", widthfct) )
    )
    colnames(summaryStatistics)[(length(factorOrder)+1):(length(factorOrder)+3)] <- verbosecol

    return(list(summaryStatistics = summaryStatistics, rawData = rawData) )

}

##################################################################   
# End of suberbData.
##################################################################   
