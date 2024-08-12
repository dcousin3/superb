######################################################################################
#' @title summary plot of any statistics with adjusted error bars.
#'
#' @md
#'
#' @description The function ``suberbPlot()`` plots standard error or confidence interval for various  
#'      descriptive statistics under various designs, sampling schemes, population size and purposes,
#'      according to the ``suberb`` framework. See \insertCite{cgh21}{superb} for more.
#'
#' @param data Dataframe in wide format
#'
#' @param BSFactors The name of the columns containing the between-subject factor(s)
#' @param WSFactors The name of the within-subject factor(s)
#' @param WSDesign the within-subject design if not a full factorial design (default "fullfactorial")
#' @param variables The dependent variable(s) as strings
#'
#' @param statistic The summary statistic function to use as a string
#' @param errorbar The function that computes the error bar. Should be "CI" or "SE" or 
#'      any function name if you defined a custom function. Default to "CI"
#' @param gamma The coverage factor; necessary when ``errorbar == "CI"``. Default is 0.95.
#' @param factorOrder Order of factors as shown in the graph (in that order: x axis,
#'       groups, horizontal panels, vertical panels)
#'
#' @param adjustments List of adjustments as described below.
#'      Default is ``adjustments = list(purpose = "single", popSize = Inf, decorrelation = "none",
#'              samplingDesign = "SRS")``
#' @param clusterColumn used in conjunction with samplingDesign = "CRS", indicates which column 
#'    contains the cluster membership
#'
#' @param showPlot Defaults to TRUE. Set to FALSE if you want the output to be the summary 
#'     statistics and intervals.
#' @param plotStyle The type of object to plot on the graph. See full list below.
#'      Defaults to "bar".
#'
#' @param preprocessfct  is a transform (or vector of) to be performed first on data matrix of each group
#' @param postprocessfct is a transform (or vector of)
#'
#' @param ...  In addition to the parameters above, superbPlot also accept a number of 
#'  optional arguments that will be transmitted to the plotting function, such as
#'  pointParams (a list of ggplot2 parameters to input inside geoms; see ?geom_bar2) and
#'  errorbarParams (a list of ggplot2 parameters for geom_errorbar; see ?geom_errorbar)
#'
#'
#' @return a plot with the correct error bars or a table of those summary statistics.
#'         The plot is a ggplot2 object with can be modified with additional declarations.
#'
#'
#' @details The possible adjustements are the following
#' * popsize: Size of the population under study. Defaults to Inf
#' * purpose: The purpose of the comparisons. Defaults to "single". 
#'      Can be "single", "difference", or "tryon".
#' * decorrelation: Decorrelation method for repeated measure designs. 
#'      Chooses among the methods "CM", "LM", "CA", "UA", "LDr" (with r an integer) or "none". Defaults to 
#'      "none". "CA" is correlation-adjusted \insertCite{c19}{superb};
#'      "UA" is based on the unitary Alpha method (derived from the Cronbach alpha;
#'      see \insertCite{lc22}{superb}).
#'      "LDr" is local decorrelation (useful for long time series with autoregressive 
#'      correlation structures; see \insertCite{cppf24}{superb});
#'     .
#' * samplingDesign: Sampling method to obtain the sample. implemented 
#'          sampling is "SRS" (Simple Randomize Sampling) and "CRS" (Cluster-Randomized Sampling).
#'
#' In version 0.97.5, the layouts for plots are the following:
#' * "bar" Shows the summary statistics with bars and error bars;
#' * "line" Shows the summary statistics with lines connecting the conditions over the first factor;
#' * "point" Shows the summary statistics with isolated points
#' * "pointjitter" Shows the summary statistics along with jittered points depicting the raw data;
#' * "pointjitterviolin" Also adds violin plots to the previous layout
#' * "pointindividualline" Connects the raw data with line along the first factor (which should be a repeated-measure factor)
#' * "raincloud" Illustrates the distribution with a cloud (half_violin_plot) and jittered dots next to it. Looks better when coordinates are flipped ``+coord_flip()``.
#' @md
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' ######################################################################
#'
#' # Basic example using a built-in dataframe as data. 
#' # By default, the mean is computed and the error bar are 95% confidence intervals
#' superbPlot(ToothGrowth, BSFactors = c("dose", "supp"), 
#'   variables = "len") 
#'
#' # Example changing the summary statistics to the median and
#' # the error bar to 80% confidence intervals
#' superbPlot(ToothGrowth, BSFactors = c("dose", "supp"), 
#'   variables = "len", statistic = "median", errorbar = "CI", gamma = .80) 
#'
#' # Example introducing adjustments for pairwise comparisons 
#' # and assuming that the whole population is limited to 200 persons
#' superbPlot(ToothGrowth, BSFactors = c("dose", "supp"), 
#'   variables = "len",  
#'   adjustments = list( purpose = "difference", popSize = 200) )
#'
#' # This example adds ggplot directives to the plot produced
#' library(ggplot2)
#' superbPlot(ToothGrowth, BSFactors = c("dose", "supp"), 
#'   variables = "len") + 
#' xlab("Dose") + ylab("Tooth Growth") +
#' theme_bw()
#'
#' ######################################################################
#'
#' # The following examples are based on repeated measures
#' library(gridExtra)
#' options(superb.feedback = 'none') # shut down 'warnings' and 'design' interpretation messages
#' 
#' # A simple example: The sleep data
#' # The sleep data are paired data showing the additional time of sleep with 
#' # the soporific drugn #1 (("group = 1") and with the soporific drug #2 ("group = 2"). 
#' # There is 10 participants with two measurements.
#' 
#' # sleep is available in long format so we transform it to the in wide format:
#' sleep2 <- reshape(sleep, direction = "wide", idvar = "ID", timevar = "group")
#' sleep2
#' 
#' # Makes the plots first without decorrelation:
#' superbPlot(sleep2, 
#'   WSFactors = "Times(2)", 
#'   variables = c("extra.1", "extra.2")
#' )
#' # As seen the error bar are very long. Lets take into consideration correlation...
#' # ...  with decorrelation (technique Correlation-adjusted CA):
#' superbPlot(sleep2, 
#'   WSFactors = "Times(2)", 
#'   variables = c("extra.1", "extra.2"), 
#'   # only difference:
#'   adjustments = list(purpose = "difference", decorrelation = "CA")
#' )
#' # The error bars shortened as the correlation is substantial (r = .795).
#' 
#' 
#' ######################################################################
#' 
#' # Another example: The Orange data
#' # Use the Orange example, but let's define shorter column names...
#' names(Orange) <- c("Tree","age","circ")
#' # ... and turn the data into a wide format using superbToWide:
#' Orange.wide <- superbToWide(Orange, id = "Tree", WSFactors = "age", variable = "circ") 
#'
#' # This example contains 5 trees whose diameter (in mm) has been measured at various age (in days):
#' Orange.wide
#'
#' # Makes the plots first without decorrelation:
#' p1 <- superbPlot( Orange.wide, WSFactors = "age(7)",
#'   variables = c("circ_118","circ_484","circ_664","circ_1004","circ_1231","circ_1372","circ_1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "none")
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="''Standalone'' confidence intervals")
#' # ... and then with decorrelation (technique Correlation-adjusted CA):
#' p2 <- superbPlot( Orange.wide, WSFactors = "age(7)",
#'   variables = c("circ_118","circ_484","circ_664","circ_1004","circ_1231","circ_1372","circ_1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "CA")
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="Decorrelated confidence intervals")
#'
#' # You can present both plots side-by-side
#' grid.arrange(p1, p2, ncol=2)
#'
#' ######################################################################
#' 
######################################################################################
#'
#' @export superbPlot
#' @importFrom lsr wideToLong
#' @importFrom plyr ddply
#' @import ggplot2
#' @importFrom utils capture.output
#
######################################################################################


superbPlot <- function(data, 
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
        decorrelation  = "none",     # is "CM", "LM", "CA", "UA", or "none"
        samplingDesign = "SRS"       # is "SRS" or "CRS" (in which case use clusterColumn)
    ),
    showPlot      = TRUE,            # show a plot or else summary statistics
    plotStyle     = "bar",           # type of plot (so far, bar, line, point, pointjitter and pointjitterviolin
    preprocessfct = NULL,            # run preprocessing on the matrix
    postprocessfct= NULL,            # run post-processing on the matrix
    clusterColumn = "",              # if samplineScheme = CRS
    ...
    # the following are optional list of graphic directives...
    # errorbarParams,                # merged into ggplot/geom_errorbar
    # pointParams,                   # merged into ggplot/geom_point
    # lineParams,                    # merged into ggplot/geom_line
    # barParams,                     # merged into ggplot/geom_bar
    # etc.
) {

    ##############################################################################
    # STEP 0: Load required libraries
    ##############################################################################
    #require(ggplot2)    # all of it
    #require(lsr)        # only function wideToLong is used
    #require(plyr)       # only function ddply is used
    
    
    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################
    # 1.0: is the data actually data!
	data <- as.data.frame(data) # coerce to data.frame if tibble or compatible
    if(!(is.data.frame(data)))
            stop("superb::ERROR: data is not a data.frame or similar data structure. Exiting...")

    # 1.1: missing adjustements
    if(is.null(adjustments$purpose))        {adjustments$purpose        <- "single"}
    if(is.null(adjustments$popSize))        {adjustments$popSize        <- Inf}
    if(is.null(adjustments$decorrelation))  {adjustments$decorrelation  <- "none"}
    if(is.null(adjustments$samplingDesign)) {adjustments$samplingDesign <- "SRS"}

    # 1.2: unknown adjustments listed
    if (!all(names(adjustments) %in% c("purpose","popSize","decorrelation","samplingDesign")))
            stop("superb::ERROR: one of the adjustment is unknown. Exiting...")

    # 1.3: invalid choice in a list of possible choices
    if (is.character(adjustments$popSize)||!(all(adjustments$popSize >0)))  
            stop("superb::ERROR: popSize should be a positive number or Inf (or a list of these). Exiting...")
    if (!(adjustments$purpose %in% c("single","difference","tryon"))) 
            stop("superb::ERROR: Invalid purpose. Did you mean 'difference'? Exiting...")
    if (!(substr(adjustments$decorrelation,1,2) %in% c("no","CM","LM","CA","UA","LD"))) 
            stop("superb::ERROR: Invalid decorrelation method. Did you mean 'CM'? Exiting...")
    if (substr(adjustments$decorrelation,1,2) == "LD") {
            radius <- suppressWarnings(as.integer(substr(adjustments$decorrelation, 3, 100)))
            if ((is.na(radius))||(radius<1))
                stop("superb::ERROR: radius given to LD not an integer or is smaller than 1. Exiting...")
    }
    if (!(adjustments$samplingDesign %in% c("SRS","CRS"))) 
            stop("superb::ERROR: Invalid samplingDesign. Did you mean 'SRS'? Exiting...")

    # 1.4a: innapropriate choice for between-subject specifications
    bsLevels <- dim(unique(data[BSFactors]))[1]
    if (!(length(adjustments$popSize) %in% c(1,bsLevels))) 
            stop("superb::ERROR: popSize is a list whose length does not match the number of groups. Exiting...")
    
    # 1.4b: invalid within-subject factors
    if (any(unlist(gregexpr("\\w\\((\\d+)\\)", WSFactors))== -1))
            stop("superb::ERROR: One of the repeated-measure factor not properly formed 'name(nlevel)'. Exiting...")
    wsMissing <- "DummyWithinSubjectFactor"
    wsLevels <- c(1)
    if (is.null(WSFactors)) {
        WSFactors <- wsMissing
    } else {
        for (i in 1:length(WSFactors)) {
            wsLevels[i] <- as.integer(unlist(strsplit(WSFactors[i], '[()]'))[2]) 
            WSFactors[i] <-            unlist(strsplit(WSFactors[i], '[()]'))[1]
        }
    }
    wslevel <- prod(wsLevels)

    # 1.4c: setting factorOrder in case it is null
    if(is.null(factorOrder))  {
        factorOrder <- c(WSFactors, BSFactors)
        if (('design' %in% getOption("superb.feedback") ) & (length(factorOrder[factorOrder != wsMissing])) > 1)  
                message(paste("superb::FYI: The variables will be plotted in that order: ",
                          paste(factorOrder[factorOrder != wsMissing],collapse=", "),
                          " (use factorOrder to change).", sep=""))
    }

    # 1.4d: checking WSFactors based on WSDesign
    if (all(WSDesign == "fullfactorial")) {
        if (!(length(variables) == wslevel)) 
                stop("superb::ERROR: The number of levels of the within-subject level(s) does not match the number of variables. Exiting...")
        if ((wslevel == 1)&&(!(adjustments$decorrelation == "none"))) 
                stop("superb::ERROR: Decorrelation is not to be used when there is no within-subject factors. Exiting...")
    } else {
        if (!is.list(WSDesign) )
                    stop("superb::ERROR: the WSdesign is not 'fullfactorial' (default) or a list. Exiting...")
        if (length(WSDesign) != length(variables))
                    stop("superb::ERROR: the WSDesign list is not of the same length as the variable vector. Exiting...")
        if (!all(lapply(WSDesign, length)==length(WSFactors)))
                    stop("superb::ERROR: the WSDesign does not contain vectors of factor levels, one per factor. Exiting...")
    }

    # 1.5: invalid column names where column names must be listed
    if (!(all(variables %in% names(data)))) 
            stop("superb::ERROR: One of the variable column is not found in data. Exiting...")
    if (!(all(BSFactors %in% names(data)))) 
            stop("superb::ERROR: One of the BSFactors column is not found in data. Exiting...")

    # 1.6: invalid inputs
    if (length(factorOrder[factorOrder != wsMissing] ) > 4)
            stop("superb::ERROR: Too many factors named on factorOrder. Maximum 4. Exiting...")
    if (length(factorOrder[factorOrder != wsMissing]) < length(WSFactors[WSFactors != wsMissing]) + length(BSFactors)) 
            stop("superb::ERROR: Too few factors named on factorOrder. Exiting...")
    if ((gamma[1] <0)||(gamma[1]>1))
            stop("superb::ERROR: gamma is not within 0 and 1. Exiting...")
    if (!is.logical(showPlot))
            stop("superb::ERROR: showPlot must be TRUE or FALSE. Exiting...")

    # 1.7: align levels and corresponding variables
    weird        <-"+!+" # to make sure that these characters are not in the column names
    if (all(WSDesign == "fullfactorial")) {
        combinaisons <- expand.grid(lapply(wsLevels,seq))
    } else {
        combinaisons <- data.frame(matrix(as.integer(unlist(WSDesign)),nrow=length(WSDesign), byrow = TRUE))
    }
    newnames     <- paste("DV", apply(combinaisons,1,paste,collapse=weird) ,sep=weird)
    design       <- cbind(combinaisons, variables, newnames)
    colnames(design)[1:length(WSFactors)] <- WSFactors
    colnames(design)[length(WSFactors)+1] <- "variable"
    colnames(design)[length(WSFactors)+2] <- "newvars"
    if ( (length(wsLevels)>1) & ('design' %in% getOption("superb.feedback") ) ) {
        message("superb::FYI: Here is how the within-subject variables are understood:")
        temp = paste0(capture.output(print(design[,c(WSFactors, "variable") ], row.names=FALSE)),collapse="\n")
        message(temp) 
    }

    # 1.8: invalid functions 
    widthfct <- paste(errorbar, statistic, sep = ".")
    if (errorbar == "none") { # create a fake function
        eval(parse(text=paste(widthfct, "<-function(X) 0",sep="")), envir = globalenv())
    }
    if ( !(is.stat.function(statistic)) )
            stop("superb::ERROR: The function ", statistic, " is not a known descriptive statistic function. Exiting...")
    if ( !(is.errorbar.function(widthfct)) )
            stop("superb::ERROR: The function ", widthfct, " is not a known function for error bars. Exiting...")
    pltfct <- paste("superbPlot", plotStyle, sep = ".")
    if ( !(is.superbPlot.function(pltfct)) )
            stop("superb::ERROR: The function ", pltfct, " is not a known function for making plots with superbPlot. Exiting...")

    # 1.9: if cluster randomized sampling, check that column cluster is set
    if (adjustments$samplingDesign == "CRS") {
        # make sure that column cluster is defined.
        if(!(clusterColumn %in% names(data))) 
            stop("superb::ERROR: With samplingDesign = \"CRS\", you must specify a valid column with ClusterColumn. Exiting...")
    }

    # 1.10: is there missing data in the scores?
    if (any(is.na(data[,variables])) ) {
        message("superb::WARNING: There are misssing data in your dependent variable(s).\n\tsuperb may show empty summaries... consult help(measuresWithMissingData)")
    }

    # We're clear to go!
    runDebug("superb.1", "End of Step 1: Input validation", 
        c("measure2","design2","BSFactors2","WSFactors2","wsLevels2","wslevel2","factorOrder2","adjustments2"), 
        list(variables, design, BSFactors, WSFactors, wsLevels, wslevel, factorOrder, adjustments) )


    ##############################################################################
    # STEP 2: Decorrelate repeated-measure variables if needed; apply transforms
    ##############################################################################

    complement <- function(x, U) {U[is.na(pmatch(U,x))]}
    x <- c(BSFactors, variables)
    U <- names(data)

    # keep a copy before transforming the data
    # 2022.11.17: sort the columns, bsfactors & wsvariables first
    data.untransformed <- data[ c(x, complement(x,U)) ]
    data.transformed   <- data[ c(x, complement(x,U)) ]

    # We do this step for each group and only on columns with repeated measures.
    if (adjustments$decorrelation == "CM" || adjustments$decorrelation == "LM") {
        data.transformed <- plyr::ddply(data.transformed, .fun = twoStepTransform, .variables= BSFactors, variables)    
    }
    # is LM (pooled standard error) needed?
    if (adjustments$decorrelation == "LM") {
        data.transformed <- plyr::ddply(data.transformed, .fun = poolSDTransform, .variables= BSFactors, variables) 
    }
    # other custom pre-processing of the data matrix per group
    if (!is.null(preprocessfct)) {
        for (fct in preprocessfct)
            data.transformed <- plyr::ddply(data.transformed, .fun = fct, .variables= BSFactors, variables) 
    }
    # other custom post-processing of the data matrix per group
    if (!is.null(postprocessfct)) {
        for (fct in postprocessfct)
            data.transformed <- plyr::ddply(data.transformed, .fun = fct, .variables= BSFactors, variables) 
    }
    
    runDebug("superb.2", "End of Step 2: Data post decorrelation", 
        c("data.transformed2", "newnames2"), list(data.transformed, newnames) )


    ##############################################################################
    # STEP 3: Put data into long format for conveniency
    ##############################################################################

    # replace variable names with names based on design...
    temp <- paste("^", variables, "$", collapse="|", sep="")
    colnames(data.untransformed)[grep(temp, names(data.untransformed))] = newnames
    colnames(data.transformed)[grep(temp, names(data.transformed))] = newnames

    # set data to long format using lsr (Navarro, 2015)
    data.untransformed.long <- suppressWarnings(lsr::wideToLong(data.untransformed, within = WSFactors, sep = weird))
    data.transformed.long   <- suppressWarnings(lsr::wideToLong(data.transformed, within = WSFactors, sep = weird))

    # New May 11th, 2022, version 0.95.1
    as.numeric.factor <- function(x) {strtoi(x)}

    data.untransformed.long[WSFactors] <- mapply(
        as.numeric.factor, data.untransformed.long[WSFactors])
    data.transformed.long[WSFactors] <- mapply(
        as.numeric.factor, data.transformed.long[WSFactors])


    # if there was no within-subject factor, a dummy had been added
    if (WSFactors[1]  == wsMissing) {
        # removing all traces of the dummy
        data.transformed.long[[wsMissing]]   <- NULL # remove the column 
        data.untransformed.long[[wsMissing]] <- NULL # remove the column 
        WSFactors    <- NULL # remove the dummy factor
        factorOrder <- factorOrder[ factorOrder != wsMissing]
    }

    # if the function has an initializer, run it on the long-format data
    if (has.init.function(statistic)) {
        iname = paste("init",statistic, sep=".")
        if (!("none" %in% getOption("superb.feedback")))
            message("superb::FYI: Running initializer ", iname)
        do.call(iname, list(data.untransformed.long) )
    }

    runDebug("superb.3", "End of Step 3: Reformat data frame into long format", 
        c("data.transformed.long2","factorOrder3"), list(data.transformed.long,factorOrder) )



    ##############################################################################
    # STEP 4: Get summary statistics (center, and lowerwidth + upperwidth)
    ##############################################################################

    aggregatefct <- function(subsetOfData) { 
        params1 <- list( subsetOfData$DV  )
        if (is.gamma.required(widthfct)) {
            paramsV <- list( subsetOfData$DV, gamma = gamma )
        } else {
            paramsV <- list( subsetOfData$DV  )
        }
        center <- do.call(statistic, params1)
        limits <- do.call(widthfct, paramsV)
        if (is.interval.function(widthfct)) {
            lowerwidth <- ( min(limits) - center)
            upperwidth <- ( max(limits) - center ) 
        } else {
            lowerwidth <- -limits
            upperwidth <- +limits
        }
        return( c(center=center, lowerwidth=lowerwidth, upperwidth=upperwidth) )
    }

    ### put into FACTORs
    summaryStatistics <- plyr::ddply( data.transformed.long, .fun = aggregatefct, .variables = factorOrder ) 

    summaryStatistics[factorOrder]       <- lapply(summaryStatistics[factorOrder], as.factor)
    data.untransformed.long[factorOrder] <- lapply(data.untransformed.long[factorOrder], as.factor)

    runDebug("superb.4", "End of Step 4: Statistics obtained", 
        c("summaryStatistics2"), list( summaryStatistics) )


    ##############################################################################
    # STEP 5: Get all the adjustments
    ##############################################################################

    # 5.1: Adjust for population size if not infinite 
    nadj <- if (min(adjustments$popSize) != Inf) {
        # Ns the number of subjects per group
        Ns  <- plyr::ddply(data, .fun = dim, .variables = BSFactors )$V1
        # the Ns must be expanded for each repeated measures
        Ns  <- rep(Ns, length(variables))
        sqrt(1 - Ns / adjustments$popSize )        
    } else {1}

    # 5.2: Adjust for purpose if "difference" or "tryon"
    padj <- if (adjustments$purpose == "difference") { 
        sqrt(2) 
    } else if  (adjustments$purpose == "tryon") {
        # extension of Tryon, 2001, for heterogeneous variances in between-group design
        Ns  <- plyr::ddply(data.transformed.long, .fun = dim, .variables = c(WSFactors,BSFactors) )$V1
        sds <- suppressWarnings(plyr::ddply(data.transformed.long, .fun = colSDs, .variables = c(WSFactors,BSFactors) )$DV)
        es <- sqrt(sum(sds^2/Ns)) / sum(sqrt(sds^2/Ns))
        # the es must be expanded for each repeated measures
        es  <- rep(es, length(variables))
        2 * es * sqrt(length(Ns)) / sqrt(2) # 2 because contrary to Tryon, 2001, superb does not want to avoid overlap
    } else {1}

    # 5.3: Adjust for cluster-randomized sampling
    sadj <- if (adjustments$samplingDesign == "CRS") {
        ICCs <- plyr::ddply(data, .fun = ShroutFleissICC1, .variables = BSFactors, clusterColumn, variables )
        KNSs <- plyr::ddply(data, .fun = getKNs, .variables = BSFactors, clusterColumn )
        ICCs <- ICCs[-1:-length(BSFactors)] # drop BSFactors columns
        KNSs <- KNSs[-1:-length(BSFactors)] # drop BSFactors columns

        ICCsKNNs <- cbind(ICCs, KNSs)
        lambdas  <- apply(ICCsKNNs, 1, CousineauLaurencelleLambda) # one lambda per group
        # the lambdas must be expanded for each repeated measures
        lambdas  <- rep(lambdas, length(variables))
        ICCs     <- ICCs$V1 # downcast to a vector for latter display
        lambdas
    } else {1}

    # 5.4: Adjust for correlation if decorrelation == "CA", "UA", ou "LDr"
    radj <- if (adjustments$decorrelation == "CA") {
        rs <- plyr::ddply(data, .fun = meanCorrelation, .variables = BSFactors, cols = variables)$V1
        # the rs must be expanded for each repeated measures
        rs  <- rep(rs, length(variables))
        sqrt(1- rs)
    } else if (adjustments$decorrelation == "UA") {
        rs <- plyr::ddply(data, .fun = unitaryAlpha, .variables = BSFactors, cols = variables)$V1
        # the rs must be expanded for each repeated measures
        rs  <- rep(rs, length(variables))
        sqrt(1- rs)
    } else if (substr(adjustments$decorrelation,1,2) == "LD") {
        rs <- plyr::ddply(data, .fun = meanLocalCorrelation, .variables = BSFactors, cols = variables, w = radius)
#        rs <- unlist(rs[,!is.na(rs)])
        rs <- suppressWarnings(as.numeric(unlist(rs))[!is.na(as.numeric(unlist(rs)))])
        # the rs is a vector containing one rLD for each measurement
        sqrt(1- rs)
    } else {1}

    # All done: apply the corrections to all the widths
    summaryStatistics$lowerwidth <- nadj*padj*sadj*radj*summaryStatistics$lowerwidth
    summaryStatistics$upperwidth <- nadj*padj*sadj*radj*summaryStatistics$upperwidth

    runDebug("superb.5", "End of Step 5: Getting adjustments", 
        c("nadj2","padj2","sadj2","radj2","summaryStatistics3"), list(nadj,padj,sadj,radj,summaryStatistics) )


    ##############################################################################
    # STEP 6: Issue feedback information
    ##############################################################################

    if (('warnings' %in% getOption("superb.feedback") ) | ('all' %in% getOption("superb.feedback")) )  {
        # 6.1: if option is difference, test heterogeneity of variances
        if ((adjustments$purpose == "difference") & (!is.null(BSFactors)) ) {
            crit <- data[,BSFactors]
            ps   <- c()
            for (var in variables ) {
                out <- split(data[,var], crit )
                # 2024.05.30: exlude empty conditions
                out <- out[lengths(out)>1]
                p   <- stats::bartlett.test(out)$p.value
                ps  <- c(ps, p)
            }
            if (any(p < .01)) 
                message("superb::ADVICE: Some of the groups' variances are heterogeneous. Consider using purpose=\"tryon\"." )
        }

        # 6.2: if deccorrelate is CA: show rbar, test Winer
        if ((adjustments$decorrelation == "CA")||(adjustments$decorrelation == "UA")||(substr(adjustments$decorrelation,1,2) == "LD")) {
            message(paste("superb::FYI: The average correlation per group is ", paste(unique(sprintf("%.4f",mean(round(rs,4)))), collapse=" ")) )

            winers <- suppressWarnings(plyr::ddply(data, .fun = "WinerCompoundSymmetryTest", .variables= BSFactors, variables)) 
            winers <- winers[,length(winers)]
            if (any(winers<.05, na.rm = TRUE))
                message("superb::ADVICE: Some of the groups' data are not compound symmetric. Consider using CM." )
        }
        
        # 6.3: if decorrelate is CM or LM: show epsilon, test Winer and Mauchly
        if (adjustments$decorrelation %in% c("CM","LM")) {
            epsGG <- suppressWarnings(plyr::ddply(data, .fun = "HyunhFeldtEpsilon", .variables= BSFactors, variables)) 
            epsGG <- epsGG[,length(epsGG)]
            message(paste("superb::FYI: The HyunhFeldtEpsilon measure of sphericity per group are ", paste(sprintf("%.3f",round(epsGG, 4)), collapse=" ")) )

            winers <- suppressWarnings(plyr::ddply(data, .fun = "WinerCompoundSymmetryTest", .variables= BSFactors, variables) )
            winers <- winers[,length(winers)]
            if (all(winers>.05, na.rm = TRUE))
                message("superb::FYI: All the groups' data are compound symmetric. Consider using CA or UA." )

            mauchlys <- plyr::ddply(data, .fun = "MauchlySphericityTest", .variables= BSFactors, variables) 
            mauchlys <- mauchlys[,length(mauchlys)]
            if (any(mauchlys<.05, na.rm = TRUE))
                message("superb::FYI: Some of the groups' data are not spherical. Use error bars with caution." )
        }
        
        # 6.4: if samplingDesign is CRS: print ICC, check that more than 8 clusters
        if (adjustments$samplingDesign == "CRS") {
            message(paste("superb::FYI: The ICC1 per group are ", paste(sprintf("%.3f",round(ICCs,3)), collapse=" ")) )
        }

        # 6.5: if objective is tryon: print tryon adjustment
        if (adjustments$purpose == "tryon") {
            message(paste("superb::FYI: The tryon adjustments per measures are ", paste("Measure ", 1:length(padj), ": ", sprintf("%.4f",round(padj,4)), sep="",collapse=", "), ", all compared to 1.4142.") )
        }
    }
    
    ##############################################################################
    # ALL DONE! Output the plot(s) or the summary data
    ##############################################################################

    runDebug("beforeplot", "Kit for testing plotting function", c("ss","factorOrder2","dl"),list(summaryStatistics, factorOrder, data.untransformed.long) )

    if (showPlot == TRUE) {
        # get the grouping factor
        groupingfactor = if(!is.na(factorOrder[2])) {factorOrder[2]} else { NULL}

        # get the facet factor(s)
        facets <- factorOrder[3:4][!is.na(factorOrder[3:4])]
        facets <- paste( c(facets, ".",".")[1:2], collapse="~")

        # produce the plot
        plot <- do.call( pltfct, list(
                    summarydata    = summaryStatistics,
                    xfactor        = factorOrder[1],
                    groupingfactor = groupingfactor,
                    addfactors     = facets,
                    rawdata        = data.untransformed.long,
                    ...
        ))
        return(plot)
    } else {
        # returns a list with summary statistsics as is and rawdata
        return( list(summaryStatistics = summaryStatistics, rawData = data.untransformed.long))
    }

    ##############################################################################
    # FINISHED! End of function superbPlot
    ##############################################################################
}


#################################################################################
# Statistics functions: colSDs; meanCorrelation; unitaryAlpha
#################################################################################

mycor <- function(X) {
    # a wrapper for cor() that checks if there are constant columns...
    rs <- suppressWarnings( cor(X, use = "pairwise.complete.obs") )
    if (any(is.na(rs))) {
        message("superb::FYI: Some of the measurements are constant. Use CM or use error bars with caution." )
        rs[is.na(rs)] <- 1
    }
    rs
}

meanCorrelation <- function(X, cols) {
    # the mean pair-wise correlations from many columns of the dataframe X
    rs   <- mycor( X[cols] )
    rbar <- mean(rs[upper.tri(rs)])
    rbar
}

unitaryAlpha <- function(X, cols) {
	m <- as.matrix(X[,cols])

	k <- dim(m)[2]
	V <- var(apply(m, 1, FUN=sum))
	S <- sum(apply(m, 2, FUN=var))
	(V-S)/((k-1)*S)
}

colSDs = function (x) {
    # the equivalent of colMeans for standard deviations
    if (is.vector(x))          sd(x)
    else if (is.matrix(x))     apply(x, 2, sd)
    else if (is.data.frame(x)) apply(x, 2, sd)
    else "what the fuck??"
}


##################################################################   
# Functions for local decorrelation
##################################################################   

# The gaussian kernel
gA <- function(w, mu, dim) {
    s1 <- exp(-((1:dim)-mu)^2/(2*w^2))/(w*sqrt(2*pi))
    s1[mu] <- 0 # Is a gaussian kernel with a hole a donut?
    s1 / sum(s1)
}

meanLocalCorrelation <- function(X, cols, w) {
    mat <- mycor( X[cols] )
    nrw <- dim(mat)[2]
    sapply( 1:nrw, 
            \(i) sum(mat[i,] * gA(w,i,nrw) )
    )
}
# that simple!



##################################################################   
# End of suberbPlot.
##################################################################   
