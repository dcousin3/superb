######################################################################################
#' @title superbPlot 
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
#' @param gamma The converage factor; necessary when errorbar == "CI". Default is 0.95.
#' @param adjustments List of adjustments as described below:
#'  popsize: Size of the population under study. Defaults to Inf
#'  purpose: The purpose of the comparisons. Defaults to "single". 
#'      Can be "single" or "difference".
#' decorrelation: Decorrelation method for repeated measure designs. 
#'      Chooses among the methods ("CM", "LM", "CA" or "none"). Defaults to "none".
#' samplingDesign: Sampling method to obtain the sample. implemented 
#'          sampling is "SRS" (Simple Randomize Sampling) and "CRS" (Cluster-Randomized Sampling).
#' Default is adjustments = list(purpose = "single", popSize = Inf, decorrelation = "none",
#'              samplingDesign = "SRS")
#' @param showPlot Defaults to TRUE. Set to FALSE if you want the output to be the summary statistics and intervals.
#' @param plotStyle The type of object to plot on the graph. Can be either "bar" or "line".
#'      Defaults to "bar".
#' @param Quiet Defaut to False, a boolean to inhibit showing additional information as warnings
#' @param clusterColumn used in conjunction with samplingDesign = "CRS", indicates which column contains the cluster membership
#' @param Debug export internal information into global environment. Default is FALSE
#' @param preprocessfct  is a transform (or vector of) to be performed first on data matrix of each group
#' @param postprocessfct is a transform (or vector of)
#' @param ...  In addition to the parameters above, superbPlot also accept a number of 
#'  optional arguments that will betransmitted to the plotting function, such as
#'  pointParams (a list of ggplot2 parameters to input inside geoms; see ?geom_bar2) and
#'  errorbarParams (a list of ggplot2 parameters for geom_errobar; see ?geom_errorbar)
#'
#'
#' @return a plot with the correct error bars or a table of those summary statistics.
#'         The plot is a ggplot2 object with can be modified with additional declarations.
#'
#' @references
#'      \insertAllCited{}
#'
#' @examples
#' # basic example using a built-in dataframe as data; 
#' # by default, the mean is computed and the error bar are 95% confidence intervals
#' superbPlot(ToothGrowth, BSFactor = c("dose", "supp"), 
#'   variables = "len") 
#'
#' # example changing the summary statistics to the median and
#' # the error bar to 90% confidence intervals
#' superbPlot(ToothGrowth, BSFactor = c("dose", "supp"), 
#'   variables = "len", statistic = "median", errorbar = "CI", gamma = .90) 
#'
#' # example introducing adjustments for pairwise comparisons 
#' # and assuming that the whole population is limited to 200 persons
#' superbPlot(ToothGrowth, BSFactor = c("dose", "supp"), 
#'   variables = "len",  
#'   adjustments = list( purpose = "difference", popSize = 200) )
#'
#' # This example add ggplot directives to the plot produced
#' library(ggplot2)
#' superbPlot(ToothGrowth, BSFactor = c("dose", "supp"), 
#'   variables = "len") + 
#'   xlab("Dose") + ylab("Tooth Growth") +
#'   theme_bw()
#'
#' # This example is based on repeated measures
#' library(lsr)
#' library(gridExtra)
#' # define shorter column names...
#' names(Orange) <- c("Tree","age","circ")
#' # turn the data into a wide format
#' Orange.wide <- longToWide(Orange, circ ~ age)
#' p1=superbPlot( Orange.wide, WSFactor = "age(7)",
#'   variables = c("circ_118","circ_484","circ_664","circ_1004","circ_1231","circ_1372","circ_1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "none"), Quiet = TRUE
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="Basic confidence intervals")
#' p2=superbPlot( Orange.wide, WSFactor = "age(7)",
#'   variables = c("circ_118","circ_484","circ_664","circ_1004","circ_1231","circ_1372","circ_1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "CM"), Quiet = TRUE
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="Decorrelated confidence intervals")
#' grid.arrange(p1,p2,ncol=2)
#'
#'
#' @export superbPlot
#' @export two_step_transform
#' @export subject_centering_transform 
#' @export bias_correction_transform 
#' @export pool_sd_transform
#' @importFrom lsr wideToLong
#' @importFrom plyr ddply
#' @import ggplot2
#
######################################################################################


superbPlot <- function(data, 
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
    showPlot      = TRUE,            # show a plot or else summary statistics
    plotStyle     = "bar",           # type of plot (so far, bar, line, point, pointjitter and pointjitterviolin
    Debug         = FALSE,           # dump named variables into global env for debugging
    Quiet         = FALSE,           # clarify the variables in within-subject design
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
    if(!(is.data.frame(data)))
            stop("ERROR: data is not a data.frame. Exiting...")

    # 1.1: missing adjustements
    if(is.null(adjustments$purpose))        {adjustments$purpose        <- "single"}
    if(is.null(adjustments$popSize))        {adjustments$popSize        <- Inf}
    if(is.null(adjustments$decorrelation))  {adjustments$decorrelation  <- "none"}
    if(is.null(adjustments$samplingDesign)) {adjustments$samplingDesign <- "SRS"}

    # 1.2: unknown adjustments listed
    if (!all(names(adjustments) %in% c("purpose","popSize","decorrelation","samplingDesign")))
            stop("ERROR: one of the adjustment is unknown. Exiting...")

    # 1.3: invalid choice in a list of possible choices
    if (is.character(adjustments$popSize)||!(all(adjustments$popSize >0)))  
            stop("ERROR: popSize should be a positive number or Inf (or a list of these). Exiting...")
    if (!(adjustments$purpose %in% c("single","difference"))) 
            stop("ERROR: Invalid purpose. Did you mean 'difference'? Exiting...")
    if (!(adjustments$decorrelation %in% c("none","CM","LM","CA"))) 
            stop("ERROR: Invalid decorrelation method. Did you mean 'CM'? Exiting...")
    if (!(adjustments$samplingDesign %in% c("SRS","CRS"))) 
            stop("ERROR: Invalid samplingDesign. Did you mean 'SRS'? Exiting...")

    # 1.4a: innapropriate choice for between-subject specifications
    bsLevels <- dim(unique(data[BSFactor]))[1]
    if (!(length(adjustments$popSize) %in% c(1,bsLevels))) 
            stop("ERROR: popSize is a list whose length does not match the number of groups. Exiting...")
    
    # 1.4b: invalid within-subject factors
    if (any(unlist(gregexpr("\\w\\((\\d+)\\)", WSFactor))== -1))
            stop("ERROR: One of the repeated-measure factor not properly formed 'name(nlevel)'. Exiting...")
    wsMissing <- "DummyWithinSubjectFactor"
    wsLevels <- c(1)
    if (is.null(WSFactor)) {
        WSFactor <- wsMissing
    } else {
        for (i in 1:length(WSFactor)) {
            wsLevels[i] <- as.integer(unlist(strsplit(WSFactor[i], '[()]'))[2]) 
            WSFactor[i] <-            unlist(strsplit(WSFactor[i], '[()]'))[1]
        }
    }

    wslevel <- prod(wsLevels)
    if (!(length(variables) == wslevel)) 
            stop("ERROR: The number of levels of the within-subject level(s) does not match the number of variables. Exiting...")
    if ((wslevel == 1)&&(!(adjustments$decorrelation == "none"))) 
            stop("ERROR: Decorrelation is not to be used when there is no within-subject factors. Exiting...")
    if(missing(factorOrder))  {factorOrder <- c(WSFactor, BSFactor)}

    # 1.5: invalid column names where column names must be listed
    if (!(all(variables %in% names(data)))) 
            stop("ERROR: One of the variable column is not found in data. Exiting...")
    if (!(all(BSFactor %in% names(data)))) 
            stop("ERROR: One of the BSFactor column is not found in data. Exiting...")

    # 1.6: invalid inputs
    if (length(factorOrder[factorOrder != wsMissing] ) > 4)
            stop("ERROR: Too many factors named on factorOrder. Maximum 4. Exiting...")
    if (length(factorOrder[factorOrder != wsMissing]) < length(WSFactor[WSFactor != wsMissing]) + length(BSFactor)) 
            stop("ERROR: Too few factors named on factorOrder. Exiting...")
    if ((gamma <0)||(gamma>1))
            stop("ERROR: gamma is not within 0 and 1. Exiting...")
    if (!is.logical(showPlot))
            stop("ERROR: showPlot must be TRUE or FALSE. Exiting...")

    # 1.7: align levels and corresponding variables
    weird        <-"+!+" # to make sure that these characters are not in the column names
    combinaisons <- expand.grid(lapply(wsLevels,seq))
    newnames     <- paste("DV", apply(combinaisons,1,paste,collapse=weird) ,sep=weird)
    design       <- cbind(combinaisons, variables, newnames)
    colnames(design)[1:length(WSFactor)] <- WSFactor
    colnames(design)[length(WSFactor)+1] <- "variable"
    colnames(design)[length(WSFactor)+2] <- "newvars"
    if ((length(wsLevels)>1)&&(!(Quiet)))  {
      cat("Here is how the within-subject variables are understood:\n")
      print( design[,c(WSFactor, "variable") ]) 
    }

    # 1.8: invalid functions 
    widthfct <- paste(errorbar, statistic, sep = ".")
    if (errorbar == "none") { # create a fake function
         eval(parse(text=paste(widthfct, "<-function(X) 0",sep="")), envir = globalenv())
    }
    if ( !(is.stat.function(statistic)) )
            stop("ERROR: The function ", statistic, " is not a known descriptive statistic function. Exiting...")
    if ( !(is.errorbar.function(widthfct)) )
            stop("ERROR: The function ", widthfct, " is not a known function for error bars. Exiting...")
    pltfct <- paste("superbPlot", plotStyle, sep = ".")
    if ( !(is.superbPlot.function(pltfct)) )
            stop("ERROR: The function ", pltfct, " is not a known function for making plots with superbPlot. Exiting...")

    # 1.9: if cluster randomized sampling, check that column cluster is set
    if (adjustments$samplingDesign == "CRS") {
        # make sure that column cluster is defined.
        if(!(clusterColumn %in% names(data))) 
            stop("ERROR: With samplingDesign = \"CRS\", you must specify a valid column with ClusterColumn. Exiting...")
    }

    # We're clear to go!
    runDebug(Debug, "End of Step 1: Input validation", 
        c("measure2","design2","BSFactor2","WSFactor2","wsLevels2","wslevel2","factorOrder2","adjustments2"), 
        list(variables, design, BSFactor, WSFactor, wsLevels, wslevel, factorOrder, adjustments) )


    ##############################################################################
    # STEP 2: Decorrelate repeated-measure variables if needed; apply transforms
    ##############################################################################

    data.unchanged <- data
    data.wide <- data
    # We do this step for each group and only on columns with repeated measures.
    if (adjustments$decorrelation == "CM" || adjustments$decorrelation == "LM") {
        data.wide <- plyr::ddply(data.wide, .fun = two_step_transform, .variables= BSFactor, variables)    
    }
    # is LM (pooled standard error) needed?
    if (adjustments$decorrelation == "LM") {
        data.wide <- plyr::ddply(data.wide, .fun = pool_sd_transform, .variables= BSFactor, variables) 
    }
    # other custom pre-processing of the data matrix per group
    if (!is.null(preprocessfct)) {
        for (fct in preprocessfct)
            data.wide <- plyr::ddply(data.wide, .fun = fct, .variables= BSFactor, variables) 
    }
    # other custom post-processing of the data matrix per group
    if (!is.null(postprocessfct)) {
        for (fct in postprocessfct)
            data.wide <- plyr::ddply(data.wide, .fun = fct, .variables= BSFactor, variables) 
    }
    
    runDebug(Debug, "End of Step 2: Data post decorrelation", 
        c("data.wide2"), list(data.wide) )


    ##############################################################################
    # STEP 3: Put data into long format for conveniency
    ##############################################################################

    # replace variable names with names based on design...
    colnames(data.unchanged)[grep(paste(variables,collapse="|"),names(data.unchanged))] = newnames
    colnames(data.wide)[grep(paste(variables,collapse="|"),names(data.wide))] = newnames
    # set data to long format using lsr (Navarro, 2015)
    # if no unique identifier is found, a column ".id" may be added; don't bother
    data.unchanged.long <- suppressWarnings(lsr::wideToLong(data.unchanged, within = WSFactor, sep = weird))
    data.long <- suppressWarnings(lsr::wideToLong(data.wide, within = WSFactor, sep = weird))

    # if there was no within-subject factor, a dummy had been added
    if (WSFactor[1]  == wsMissing) {
        # removing all traces of the dummy
        data.long[[wsMissing]] = NULL # remove the column 
        WSFactor = NULL # remove the dummy factor
        factorOrder = factorOrder[ factorOrder != wsMissing]
    }
    
    runDebug(Debug, "End of Step 3: Reformat data frame into long format", 
        c("data.long2","factorOrder3"), list(data.long,factorOrder) )


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

    summaryStatistics <- plyr::ddply( data.long, .fun = aggregatefct, .variables = factorOrder ) 
    summaryStatistics[factorOrder] <- lapply(summaryStatistics[factorOrder], as.factor)

    runDebug(Debug, "End of Step 4: Statistics obtained", 
        c("summaryStatistics2"), list( summaryStatistics) )


    ##############################################################################
    # STEP 5: Get all the adjustments
    ##############################################################################

    # 5.1: Adjust for population size if not infinite 
    nadj <- if (min(adjustments$popSize) != Inf) {
        # Ns the number of subjects per group
        Ns  <- plyr::ddply(data, .fun = dim, .variables = BSFactor )$V1
        # the Ns must be expanded for each repeated measures
        Ns  <- rep(Ns, wslevel)
        sqrt(1 - Ns / adjustments$popSize )        
    } else {1}

    # 5.2: Adjust for purpose if "difference"
    padj <- if (adjustments$purpose == "difference") { sqrt(2) } else {1}
    
    # 5.3: Adjust for cluster-randomized sampling
    sadj <- if (adjustments$samplingDesign == "CRS") {
        ICCs <- plyr::ddply(data, .fun = ShroutFleissICC1, .variables = BSFactor, clusterColumn, variables )
        KNSs <- plyr::ddply(data, .fun = getKNs, .variables = BSFactor, clusterColumn )
        ICCs <- ICCs[-1:-length(BSFactor)] # drop BSFactor columns
        KNSs <- KNSs[-1:-length(BSFactor)] # drop BSFactor columns

        ICCsKNNs <- cbind(ICCs, KNSs)
        lambdas  <- apply(ICCsKNNs, 1, lambda) # one lambda per group
        # the lambdas must be expanded for each repeated measures
        lambdas  <- rep(lambdas, wslevel)
        ICCs     <- ICCs$V1 # downcast to a vector for latter display
        lambdas

    } else {1}

    # 5.4: Adjust for correlation if decorrelation == "CA"
    radj <- if (adjustments$decorrelation == "CA") {
        rs <- plyr::ddply(data, .fun = meanCorrelation, .variables = BSFactor, cols = variables)$V1
        # the rs must be expanded for each repeated measures
        rs  <- rep(rs, wslevel)
        sqrt(1- rs)
    } else {1}

    # All done: apply the corrections to all the widths
    summaryStatistics$lowerwidth <- nadj*padj*sadj*radj*summaryStatistics$lowerwidth
    summaryStatistics$upperwidth <- nadj*padj*sadj*radj*summaryStatistics$upperwidth

    runDebug(Debug, "End of Step 5: Getting adjustments", 
        c("nadj2","padj2","sadj2","radj2","summaryStatistics3"), list(nadj,padj,sadj,radj,summaryStatistics) )


    ##############################################################################
    # STEP 6: Issue warnings
    ##############################################################################

    if (!Quiet) {
        # 6.1: if deccorrelate is CA: show rbar, test Winer
        if (adjustments$decorrelation == "CA") {
            warning(paste("FYI: The average correlation per group are ", paste(unique(rs), collapse=" ")), call. = FALSE)

            winers <- suppressWarnings(plyr::ddply(data, .fun = "WinerCompoundSymmetryTest", .variables= BSFactor, variables)) 
            winers <- winers[,length(winers)]
            if (any(winers<.05, na.rm = TRUE))
                warning("Some of the groups' data are not compound symmetric. Consider using CM.", call. = FALSE)
        }
        
        # 6.2: if decorrelate is CM or LM: show epsilon, test Winer and Mauchly
        if (adjustments$decorrelation %in% c("CM","LM")) {
            epsGG <- suppressWarnings(plyr::ddply(data, .fun = "epsilon", .variables= BSFactor, variables)) 
            epsGG <- epsGG[,length(epsGG)]
            warning(paste("FYI: The epsilon measure of sphericity per group are ", paste(epsGG, collapse=" ")), call. = FALSE)

            winers <- suppressWarnings(plyr::ddply(data, .fun = "WinerCompoundSymmetryTest", .variables= BSFactor, variables) )
            winers <- winers[,length(winers)]
            if (all(winers>.05, na.rm = TRUE))
                warning("FYI: All the groups' data are compound symmetric. Consider using CA.", call. = FALSE)

            mauchlys <- plyr::ddply(data, .fun = "MauchlySphericityTest", .variables= BSFactor, variables) 
            mauchlys <- mauchlys[,length(mauchlys)]
            if (any(mauchlys<.05, na.rm = TRUE))
                warning("FYI: Some of the groups' data are not spherical. Use error bars with caution.", call. = FALSE)
        }
        
        # 6.3: if samplingDesign is CRS: print ICC, check that more than 8 clusters
        if (adjustments$samplingDesign == "CRS") {
            warning(paste("FYI: The ICC1 per group are ", paste(ICCs, collapse=" ")), call. = FALSE)
        }
    }
    
    ##############################################################################
    # ALL DONE! Output the plot(s) or the summary data
    ##############################################################################

    if (showPlot == TRUE) {
        # generate the plot
        groupingfac = if(!is.na(factorOrder[2])) {factorOrder[2]}else{ NULL}
        # if present, make the grouping variable a factor
        if (!is.null(groupingfac)) {
            data.unchanged.long[[groupingfac]] = as.factor(data.unchanged.long[[groupingfac]])
        }
        runDebug(Debug, "Kit for testing plotting function", c("ss","factorOrder2","dl"),list(summaryStatistics, factorOrder, data.unchanged.long) )

        # first get the facets
        facets <- factorOrder[3:4][!is.na(factorOrder[3:4])]
        facets <- c(facets, ".",".")
        facets <- paste(facets[1:2], collapse="~")

        # produce the plot
        plot <- do.call( pltfct, list(
                    summarydata = summaryStatistics,
                    xvar        = factorOrder[1],
                    groupingfac = groupingfac,
                    addfactors  = facets,
                    Debug       = Debug,
                    rawdata     = data.unchanged.long,
                    ...
        ))
        return(plot)
    } else {
        # do some renaming of the columns for clearer results
        #verbosecol <- c(
        #    statistic,
        #    if (errorbar == "SE") c("- 1 * SE", "+ 1 * SE") 
        #    else if (errorbar == "CI") c(paste("-", gamma* 100, "% CI width"), paste("+", gamma* 100, "% CI width") ) 
        #    else if (errorbar == "PI") c(paste("-", gamma* 100, "% PI width"), paste("+", gamma* 100, "% PI width") ) 
        #    else c(paste("-", widthfct), paste("+", widthfct) )
        #)
        #colnames(summaryStatistics)[(length(factorOrder)+1):(length(factorOrder)+3)] <- verbosecol
        return(summaryStatistics)
    }

    ##############################################################################
    # FINISHED! End of function superbPlot
    ##############################################################################
}



#################################################################################
# logical functions:    is.interval.function; is.gamma.required; is.stat.function
#################################################################################

is.stat.function <- function(fctname) {
    # does the function provided by the user exists and compute from a list of data? 
    res <- tryCatch(
        {do.call(fctname, list( c(1,2,3) ) ); TRUE},
        error = function(cond) {return(FALSE)} 
    )
    res
}

is.errorbar.function <- function(fctname) {
    # does the function provided by the user exists and compute from a list of data? 
    if (is.gamma.required(fctname)) {
        TRUE
    } else {
        is.stat.function(fctname)
    }
}
    
is.interval.function <- function(fctname) {
    # is the function provided by the user an interval, i.e., two numbers (e.g., CI) 
    # or a single width (e.g., SE)?
    res <- do.call(fctname, list( c(1,2,3)) )
    if (length(res) == 2) TRUE else FALSE
}

is.width.function <- function(fctname) {
    # is the function provided by the user an interval, i.e., two numbers (e.g., CI) 
    # or a single width (e.g., SE)?
    res <- do.call(fctname, list( c(1,2,3)) )
    if (length(res) == 1) TRUE else FALSE
}

is.gamma.required <- function(fctname) {
    # is the function provided by the user requires a coverage factor
    # gamma (e.g., CI) or not (e.g., SE)?
    res <- tryCatch(
        {do.call(fctname, list( c(1,2,3), gamma = 0.95) ); TRUE},
        error = function(cond) {return(FALSE)}
    )
    res
}

is.superbPlot.function <- function(fctname) {
    # does the plot function provided by the user exists?
    res <- TRUE
    if (!exists(fctname)) {
        res <- FALSE
    } else {
        # if the symbol exists, run a fake call to see if it works...
        dta <- data.frame( 
                dose       = cbind(c(0.5,0.5,1,1,2,2)),
                supp       = cbind(c("A","B","A","B","A","B")),
                center     = cbind(c(13,8,22,17,26,26)),
                lowerwidth = cbind(c(-3,-2,-3,-2,-2,-4)),
                upperwidth = cbind(c(+3,+2,+3,+2,+2,+4))
        )
        fake = datasets::ToothGrowth;
        fake = cbind(fake, DV = fake$len)

        res <- tryCatch(
            {test <- suppressWarnings(do.call(fctname, 
                    list(dta,
                        "dose", 
                        "supp", ".~.", 
                        FALSE,
                        fake ) ) ); 
            "ggplot" %in% class(test)},
            error = function(cond) {return(FALSE)} 
        )
    }
    res
}


#################################################################################
# statistics functions: colSDs; meanCorrelation
#################################################################################

meanCorrelation <- function(X, cols) {
    rs   <- cor(X[cols])
    rbar <- mean(rs[upper.tri(rs)])
    rbar
}

colSDs = function (x) {
    # the equivalent of colMeans for standard deviations
    if (is.vector(x))          sd(x)
    else if (is.matrix(x))     apply(x, 2, sd)
    else if (is.data.frame(x)) apply(x, 2, sd)
    else "what the fuck??"
}


#################################################################################
# tranform functions:   two_step_transform; pool_sd_transform;
#                       bias_correction_transform; subject_centering_transform;
#################################################################################



######################################################################################
#' @title transformations
#'
#' @aliases two_step_transform subject_centering_transform bias_correction_transform pool_sd_transform
#'
#' @description two_step_transform, subject_centering_transform, 
#' bias_correction_transform and pool_sd_transform are four 
#' transformations that can be applied to a matrix of data.
#'
#' @param dta a data.frame containing the data in wide format;
#' @param variables a vector of column names on which the transformation will be applied.
#'     the remaining columns will be left unchanged
#'
#' @return a data.frame of the same form as dta with the variables transformed.
#'
#'




two_step_transform <- function(dta, variables) {
    # from O'Brien and Cousineau (2014) The Quantitative Methods for Psychology
    # It merges together subject_centering_transform and bias_correction_transform
    X <- dta[ variables ]
    C <- ncol(X)
    Y <- X - rowMeans(X) + mean(rowMeans(X))
    Z <- sqrt(C / (C - 1)) * (t(Y) - colMeans(Y)) + colMeans(Y)
    Z <- as.data.frame(t(Z))
    dta [ variables ] = Z
    return(dta)
}
subject_centering_transform <- function(dta, variables) {
    # from O'Brien and Cousineau (2014) The Quantitative Methods for Psychology
    X <- dta[ variables ]
    C <- ncol(X)
    Y <- X - rowMeans(X) + mean(rowMeans(X))
    dta [ variables ] = Y
    return(dta)
}
bias_correction_transform <- function(dta, variables) {
    # from O'Brien and Cousineau (2014) The Quantitative Methods for Psychology
    Y <- dta[ variables ]
    C <- ncol(Y)
    Z <- sqrt(C / (C - 1)) * (t(Y) - colMeans(Y)) + colMeans(Y)
    Z <- as.data.frame(t(Z))
    dta [ variables ] = Z
    return(dta)
}
pool_sd_transform <- function(dta, variables) {
    # from Cousineau, in prep.
    Z   <- dta[ variables ]
    sds <- colSDs(Z)
    sdp <- sqrt(mean(sds^2))
    W   <- sdp / sds * (t(Z) - colMeans(Z)) + colMeans(Z)
    W <- as.data.frame(t(W))
    dta [ variables ] = W
    return(dta)
}


##################################################################   
# End of suberbPlot.
##################################################################   
