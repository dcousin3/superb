######################################################################################
#' @title plotESP 
#'
#' @description Plots standard error or confidence interval for various descriptive 
#'      statistics under various designs, sampling schemes, population size and purposes,
#'      according to the ESP framework.
#'
#' @param data Dataframe in wide format
#' @param bsFactor The name of the columns containing the between-subject factor(s)
#' @param wsFactor The name of the within-subject factor(s)
#' @param factorOrder Order of factors as shown in the graph (x axis, groups, horizontal 
#'       panels, vertical panels)
#' @param variables The dependent variable(s)
#' @param statistic The summary statistic function to use
#' @param errorbar The function that computes the error bar. Should be "CI" or "SE" or 
#'      any function name. Defaults to "SE"
#' @param gamma The converage factor; necessary when errorbar == "CI". Default is 0.95.
#' @param adjustments List of adjustments as described below:
#' @param adjustments$popsize Size of the population under study. Defaults to Inf
#' @param adjustments$purpose The purpose of the comparisons. Defaults to "single". 
#'      Can be "single" or "difference".
#' @param adjustments$decorrelation Decorrelation method for repeated measure designs. 
#'      Chooses among the methods ("CM", "LM", "CA" or "none"). Defaults to "none".
#' Default is adjustments = list(purpose = "single", popSize = Inf, decorrelation = "none")
#' @param showPlot Defaults to TRUE. Set to FALSE if you want the output to be the summary statistics and intervals.
#' @param plotStyle The type of object to plot on the graph. Can be either "bar" or "line".
#'      Defaults to "bar".
#' @param pointParams a list of ggplot2 parameters to input inside geoms (see ?geom_bar2)
#' @param errorParams a list of ggplot2 parameters for geom_errobar (see ?geom_errorbar)
#' @param Debug export internal information into global environment. Default is FALSE
#' @param preprocessfct  is a transform (or vector of) to be performed first on data matrix of each group
#' @param postprocessfct is a transform (or vector of)
#'
#'
#' @return a plot with the correct error bars or a table of those summary statistics.
#'         The plot is a ggplot2 object with can be modified with additional declarations.
#'
#' @examples
#' # basic example using a built-in dataframe as data; 
#' # by default, the mean is computed and the error bar are 95% confidence intervals
#' plotESP(ToothGrowth, bsFactor = c("dose", "supp"), 
#'   variables = "len") 
#'
#' # example changing the summary statistics to the median and
#' # the error bar to 90% confidence intervals
#' plotESP(ToothGrowth, bsFactor = c("dose", "supp"), 
#'   variables = "len", statistic = "median", errorbar = "CI", gamma = .90) 
#'
#' # example introducing adjustments for pairwise comparisons 
#' # and assuming that the whole population is limited to 200 persons
#' plotESP(ToothGrowth, bsFactor = c("dose", "supp"), 
#'   variables = "len",  
#'   adjustments = list( purpose = "difference", popSize = 200) )
#'
#' # This example add ggplot directives to the plot produced
#' plotESP(ToothGrowth, bsFactor = c("dose", "supp"), 
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
#' p1=plotESP( Orange.wide, wsFactor = "age(7)",
#'   variables = c("circ_118","circ_484","circ_664","circ_1004","circ_1231","circ_1372","circ_1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "none")
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="Basic confidence intervals")
#' p2=plotESP( Orange.wide, wsFactor = "age(7)",
#'   variables = c("circ_118","circ_484","circ_664","circ_1004","circ_1231","circ_1372","circ_1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "CM")
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="Decorrelated confidence intervals")
#' grid.arrange(p1,p2,ncol=2)
#'
#'
#' @export plotESP
######################################################################################


plotESP <- function(data, 
    bsFactor      = NULL,              # vector of the between-subject factor columns
    wsFactor      = NULL,              # vector of the names of the within-subject factors
    factorOrder,                       # order of the factors for plots
    variables,                         # dependent variable name(s)
    statistic     = "mean",            # descriptive statistics
    errorbar      = "CI",              # content of the error bars
    gamma         = 0.95,              # coverage if confidence intervals
    adjustments   = list(
        purpose        = "single",     # is "single" or "difference"
        popSize        = Inf,          # is Inf or a specific positive integer
        decorrelation  = "none",       # is "CM", "LM", "CA" or "none"
        samplingDesign = "SRS"         # is "SRS" or "CRS" (in which case use clusterColumn)
    ),
    showPlot      = TRUE,              # show a plot or else statistics
    plotStyle      = "bar",            # type of plot
    errorParams   = list(width = .8),  # sent to ggplot/error bars
    pointParams   = list(),            # sent to ggplot/summary results
    Debug         = FALSE,             # dump named variables into global env for debugging
    preprocessfct = NULL,              # run preprocessing on the matrix
    postprocessfct= NULL,              # run post-processing on the matrix
    clusterColumn = ""                 # if samplineScheme = CRS
) {

    ##############################################################################
    # STEP 0: Load required libraries
    ##############################################################################
    require(ggplot2)
    require(lsr)        # only function wideToLong is used
    require(plyr)       # only function ddply is used
    
    
    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################
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
    bsLevels <- dim(unique(data[bsFactor]))[1]
    if (!(length(adjustments$popSize) %in% c(1,bsLevels))) 
            stop("ERROR: popSize is a list whose length does not match the number of groups. Exiting...")
    
    # 1.4b: invalid within-subject factors
    if (any(unlist(gregexpr("\\w\\((\\d+)\\)", wsFactor))== -1))
            stop("ERROR: One of the repeated-measure factor not properly formed 'name(nlevel)'. Exiting...")
    wsMissing <- "DummyWithinSubjectFactor"
    wsLevels <- c(1)
    if (is.null(wsFactor)) {
        wsFactor <- wsMissing
    } else {
        for (i in 1:length(wsFactor)) {
            wsLevels[i] <- as.integer(unlist(strsplit(wsFactor[i], '[()]'))[2])
            wsFactor[i] <-            unlist(strsplit(wsFactor[i], '[()]'))[1]
        }
    }

    wslevel <- prod(wsLevels)
    if (!(length(variables) == wslevel)) 
            stop("ERROR: The number of levels of the within-subject level(s) does not match the number of variables. Exiting...")
    if ((wslevel == 1)&&(!(adjustments$decorrelation == "none"))) 
            stop("ERROR: Decorrelation is not to be used when there is no within-subject factors. Exiting...")
    if(missing(factorOrder))  {factorOrder <- c(wsFactor, bsFactor)}

    # 1.5: invalid column names where column names must be listed
    if (!(all(variables %in% names(data)))) 
            stop("ERROR: One of the variable column is not found in data. Exiting...")
    if (!(all(bsFactor %in% names(data)))) 
            stop("ERROR: One of the bsFactor column is not found in data. Exiting...")

    # 1.6: invalid inputs
    if (length(factorOrder[factorOrder != wsMissing] ) > 4)
            stop("ERROR: Too many factors named on factorOrder. Maximum 4. Exiting...")
    if (length(factorOrder[factorOrder != wsMissing]) < length(wsFactor[wsFactor != wsMissing]) + length(bsFactor)) 
            stop("ERROR: Too few factors named on factorOrder. Exiting...")
    if ((gamma <0)||(gamma>1))
            stop("ERROR: gamma is not within 0 and 1. Exiting...")
    if (!(plotStyle %in% c("bar","line"))) 
            stop("ERROR: plotStyle must be 'bar' or 'line'. Exiting...")
    if (!is.logical(showPlot))
            stop("ERROR: showPlot must be TRUE or FALSE. Exiting...")

    # 1.7: align levels and corresponding variables
    weird        <-"+!+" # to make sure that these characters are not in the column names
    combinaisons <- expand.grid(lapply(wsLevels,seq))
    newnames     <- paste("DV", apply(combinaisons,1,paste,collapse=weird) ,sep=weird)
    design       <- cbind(combinaisons, variables, newnames)
    colnames(design)[1:length(wsFactor)] <- wsFactor
    colnames(design)[length(wsFactor)+1] <- "variable"
    colnames(design)[length(wsFactor)+2] <- "newvars"
    if (length(wsLevels)>1) {
      cat("Here is how the within-subject variables are understood:\n")
      print( design[,c(wsFactor, "variable") ]) 
    }

    # 1.8: invalid statistical functions 
    widthfct <- paste(errorbar, statistic, sep = ".")
    if ( !(is.exists.function(statistic)) )
            stop("ERROR: The function ", statistic, " is not a known descriptive statistic function. Exiting...")
    if ( !(is.exists.function(widthfct)) )
            stop("ERROR: The function ", widthfct, " is not a known function for error bars. Exiting...")

    # 1.9: if cluster randomized sampling, check that column cluster is set
    if (adjustments$samplingDesign == "CRS") {
        # make sure that column cluster is defined.
        if(!(clusterColumn %in% names(data))) 
            stop("ERROR: With samplingDesign = \"CRS\", you must specify a valid column with ClusterColumn. Exiting...")
    }

    # We're clear to go!
    runDebug(Debug, "End of Step 1: Input validation", 
        c("measure2","design2","bsFactor2","wsFactor2","wsLevels2","wslevel2","factorOrder2","adjustments2"), 
        list(variables, design, bsFactor, wsFactor, wsLevels, wslevel, factorOrder, adjustments) )


    ##############################################################################
    # STEP 2: Decorrelate repeated-measure variables if needed; apply transforms
    ##############################################################################

    data.wide <- data
    # We do this step for each group and only on columns with repeated measures.
    if (adjustments$decorrelation == "CM" || adjustments$decorrelation == "LM") {
        data.wide <- plyr::ddply(data.wide, .fun = two_step_transform, .variables= bsFactor, variables)    
    }
    # is LM (pooled standard error) needed?
    if (adjustments$decorrelation == "LM") {
        data.wide <- plyr::ddply(data.wide, .fun = pool_sd_transform, .variables= bsFactor, variables) 
    }
    # other custom pre-processing of the data matrix per group
    if (!is.null(preprocessfct)) {
        for (fct in preprocessfct)
            data.wide <- plyr::ddply(data.wide, .fun = fct, .variables= bsFactor, variables) 
    }
    # other custom post-processing of the data matrix per group
    if (!is.null(postprocessfct)) {
        for (fct in postprocessfct)
            data.wide <- plyr::ddply(data.wide, .fun = fct, .variables= bsFactor, variables) 
    }
    
    runDebug(Debug, "End of Step 2: Data post decorrelation", 
        c("data.wide2"), list(data.wide) )


    ##############################################################################
    # STEP 3: Put data into long format for conveniency
    ##############################################################################

    # replace variable names with names based on design...
    colnames(data.wide)[grep(paste(variables,collapse="|"),names(data.wide))] = newnames
    # set data to long format using lsr (Navarro, 2015)
    # if no unique identifier is found, a column ".id" may be added; don't bother
    data.long <- suppressWarnings(lsr::wideToLong(data.wide, within = wsFactor, sep = weird))

    # if there was no within-subject factor, a dummy had been added
    if (wsFactor[1]  == wsMissing) {
        # removing all traces of the dummy
        data.long[[wsMissing]] = NULL # remove the column 
        wsFactor = NULL # remove the dummy factor
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
        Ns  <- plyr::ddply(data, .fun = dim, .variables = bsFactor )$V1
        # the Ns must be expanded for each repeated measures
        Ns  <- rep(Ns, wslevel)
        sqrt(1 - Ns / adjustments$popSize )        
    } else {1}

    # 5.2: Adjust for purpose if "difference"
    padj <- if (adjustments$purpose == "difference") { sqrt(2) } else {1}
    
    # 5.3: Adjust for cluster-randomized sampling
    sadj <- if (adjustments$samplingDesign == "CRS") {
        ICCs <- plyr::ddply(data, .fun = ShroutFleissICC1, .variables = bsFactor, clusterColumn, variables )
        KNSs <- plyr::ddply(data, .fun = getKNs, .variables = bsFactor, clusterColumn )
        ICCs <- ICCs[-1:-length(bsFactor)] # drop bsFactor columns
        KNSs <- KNSs[-1:-length(bsFactor)] # drop bsFactor columns

        ICCsKNNs <- cbind(ICCs, KNSs)
        lambdas  <- apply(ICCsKNNs, 1, lambda) # one lambda per group
        # the lambdas must be expanded for each repeated measures
        lambdas  <- rep(lambdas, wslevel)
        ICCs     <- ICCs$V1 # downcast to a vector for latter display
        lambdas

    } else {1}

    # 5.4: Adjust for correlation if decorrelation == "CA"
    radj <- if (adjustments$decorrelation == "CA") {
        rs <- plyr::ddply(data, .fun = meanCorrelation, .variables = bsFactor, cols = variables)$V1
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

    # 6.1: if deccorrelate is CA: show rbar, test Winer
    if (adjustments$decorrelation == "CA") {
        warning(paste("FYI: The average correlation per group are ", paste(unique(rs), collapse=" ")), call. = FALSE)

        winers <- suppressWarnings(plyr::ddply(data, .fun = "WinerCompoundSymmetryTest", .variables= bsFactor, variables)) 
        winers <- winers[,length(winers)]
        if (any(winers<.05, na.rm = TRUE))
            warning("Some of the groups' data are not compound symmetric. Consider using CM.", call. = FALSE)
    }
    
    # 6.2: if decorrelate is CM or LM: show epsilon, test Winer and Mauchly
    if (adjustments$decorrelation %in% c("CM","LM")) {
        epsGG <- suppressWarnings(plyr::ddply(data, .fun = "epsilon", .variables= bsFactor, variables)) 
        epsGG <- epsGG[,length(epsGG)]
        warning(paste("FYI: The epsilon measure of sphericity per group are ", paste(epsGG, collapse=" ")), call. = FALSE)

        winers <- suppressWarnings(plyr::ddply(data, .fun = "WinerCompoundSymmetryTest", .variables= bsFactor, variables) )
        winers <- winers[,length(winers)]
        if (all(winers>.05, na.rm = TRUE))
            warning("FYI: All the groups' data are compound symmetric. Consider using CA.", call. = FALSE)

        mauchlys <- plyr::ddply(data, .fun = "MauchlySphericityTest", .variables= bsFactor, variables) 
        mauchlys <- mauchlys[,length(mauchlys)]
        if (any(mauchlys<.05, na.rm = TRUE))
            warning("FYI: Some of the groups' data are not spherical. Use error bars with caution.", call. = FALSE)
    }
    
    # 6.3: if samplingDesign is CRS: print ICC, check that more than 8 clusters
    if (adjustments$samplingDesign == "CRS") {
        warning(paste("FYI: The ICC1 per group are ", paste(ICCs, collapse=" ")), call. = FALSE)
    }
    
    
    ##############################################################################
    # ALL DONE! Output the plot(s) or the summary data
    ##############################################################################

    if (showPlot == TRUE) {
        # generate the plot
        plot <- make_plot(data = summaryStatistics, 
            type = plotStyle,
            x = factorOrder[1],
            y = "center",
            ymin = "center + lowerwidth",
            ymax = "center + upperwidth",
            groupingfac = ifelse(!is.na(factorOrder[2]), factorOrder[2], "1"),
            addfactors = factorOrder[3:4][!is.na(factorOrder[3:4])],
            pointParams = pointParams,
            errorParams = errorParams
        )
        return(plot)
    } else {
        # do some renaming of the columns for clearer results
        verbosecol <- c(
            statistic,
            if (errorbar == "SE") c("- 1 * SE", "+ 1 * SE") 
            else if (errorbar == "CI") c(paste("-", gamma* 100, "% CI width"), paste("+", gamma* 100, "% CI width") ) 
            else c(paste("-", widthfct), paste("+", widthfct) )
        )
        colnames(summaryStatistics)[(length(factorOrder)+1):(length(factorOrder)+3)] <- verbosecol
        return(summaryStatistics)
    }

    ##############################################################################
    # FINISHED! End of function plotESP
    ##############################################################################
}




#################################################################################
# logical functions:    is.interval.function; is.gamma.required; is.exists.function
#################################################################################

is.interval.function <- function(fctname) {
    # is the function provided by the user an interval (e.g., CI) 
    # or a single width (e.g., SE)?
    res <- do.call(fctname, list( c(1,2,3)) )
    if (length(res) == 2) TRUE else FALSE
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

is.exists.function <- function(fctname) {
    # does the function provided by the user exists?
    res <- tryCatch(
        {do.call(fctname, list( c(1,2,3) ) ); TRUE},
        error = function(cond) {return(FALSE)} 
    )
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
bais_correction_transform <- function(dta, variables) {
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


#################################################################################
# debugging function:   runDebug; 
#################################################################################

runDebug <- function(state, title, vars, vals) { 
    # runDebug provides traces of the vars and
    # reassign them in the globalenv so that we can try commands
    if (state) {
        cat(paste("==>",title,"<==\n"))
        for (i in 1:length(vars)) {
            cat(paste("-",vars[i],"-\n"))
            print(vals[[i]])
            assign(vars[i], vals[[i]], envir = globalenv())
        }
    }
}


#################################################################################
# plotting functions:    make_plot, build_graph
#################################################################################

make_plot <- function(data, type, 
    x, y, ymin, ymax, 
    groupingfac, addfactors,
    pointParams = list(), 
    errorParams = list()
) {
    # some prefer "1", others NULL
    groupingfac2 <- if(groupingfac=="1") NULL else groupingfac

    # make all components separately in a list, then reduce the list to a +    
    plot <- Reduce(`+`, list(
        # the global setup of the plot
        ggplot(
            data, 
            aes_string(
                x = x, y = y, ymin = ymin, ymax = ymax, 
                fill = groupingfac2, 
                shape = groupingfac2, 
                colour = groupingfac2
            )
        ),

        if(type=="bar") {
            # the histograms
            do.call(geom_bar, modifyList(
                list(position = position_dodge(width = .95),
                    stat = "identity" ),
                pointParams
            ))
        } else { list(
            # the points ...
            do.call(geom_point, modifyList(
                list(position = position_dodge(width = .25), 
                    stat = "identity", 
                    mapping = aes_string(group = groupingfac) ),
                pointParams
            )),
            # ... and the lines connecting the points
            do.call(geom_line, modifyList(
                list(position = position_dodge(width = .25), 
                    stat = "identity", 
                    mapping = aes_string(group = groupingfac) ),
                pointParams
            ))
        )},

        # the error bars
        do.call(geom_errorbar, modifyList(
             list(position = position_dodge(ifelse(type=="bar",.95,.25))),
             errorParams
        )),
    
        # the panels (rows or both rows and columns)
        if (!identical(addfactors,character(0))) {
            if (length(addfactors)==1) {
               do.call(facet_grid, list(paste("~ ",addfactors[1], sep="")))
            } else {
               do.call(facet_grid, list(paste(addfactors[2], "~ ",addfactors[1], sep="")))            
            }
        }
    
    ))
    
    return(plot)
}


