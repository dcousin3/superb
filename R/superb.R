###################################################################################
#' @title superb 
#'
#' @md
#'
#' @description The function ``superb()`` plots standard error or confidence interval for   
#'      various descriptive statistics under various designs, sampling schemes, 
#'      population size and purposes, according to the ``superb`` framework. 
#'      See \insertCite{cgh21}{superb} for more.
#'      The functions `superb()` is now the entry point
#'      to realize summary plots.
#'      Compared to the previously documented `superbPlot()`,
#'      `superb()` is based on formula and accept 
#'      long and wide format.
#'
#' @param formula a formula describing the design of the data frame
#' @param data Dataframe in wide or long format
#'
#' @param WSFactors The name of the within-subject factor(s)
#' @param WSDesign the within-subject design if not a full factorial design (default "fullfactorial")
#'
#' @param statistic The summary statistic function to use as a string
#' @param errorbar The function that computes the error bar. Should be "CI" or "SE" or 
#'      any function name if you defined a custom function (see 
#'      \code{vignette("Vignette4", package = "superb")}). 
#'      Default to "CI"
#' @param gamma The coverage factor; necessary when ``errorbar == "CI"``. Default is 0.95.
#' @param factorOrder Order of factors as shown in the graph (in that order: x axis,
#'       groups, horizontal panels, vertical panels)
#'
#' @param adjustments List of adjustments as described below.
#'      Default is ``adjustments = list(purpose = "single", 
#'                       decorrelation = "none",
#'                       samplingDesign = "SRS", 
#'                       popSize = Inf)``
#' @param clusterColumn used in conjunction with samplingDesign = "CRS", indicates which column 
#'    contains the cluster membership
#'
#' @param showPlot Defaults to TRUE. Set to FALSE if you want the output to be the summary 
#'     statistics and intervals.
#' @param plotStyle `r lifecycle::badge("deprecated")`
#' @param plotLayout The type of object to plot on the graph. See full list below.
#'      Defaults to "line".
#'
#' @param preprocessfct  is a transform (or vector of) to be performed first on data 
#'      matrix of each group
#' @param postprocessfct is a transform (or vector of)
#'
#' @param ...  In addition to the parameters above, superbPlot also accept a number of 
#'  optional arguments that will be transmitted to the plotting function, such as
#'  `pointParams` (a list of ggplot2 parameters to input inside geoms; see `?geom_bar2`) and
#'  `errorbarParams` (a list of ggplot2 parameters for geom_errorbar; see `?geom_errorbar`)
#'
#' @return a plot with the correct error bars or a table of those summary statistics.
#'         The plot is a ggplot2 object with can be modified with additional declarations.
#'
#'
#' @details The possible adjustements are the following
#' * `purpose`: The purpose of the comparisons. Defaults to "single". 
#'      Can be "single", "difference", or "tryon".
#' * `decorrelation`: Decorrelation method for repeated measure designs. 
#'      Chooses among the methods "CM", "LM", "CA", "UA", "LDr" (with _r_ a positive integer) or "none".  
#'      Defaults to "none". "CA" is correlation-adjusted \insertCite{c19}{superb};
#'      "CM" is Cousineau-Morey \insertCite{b12}{superb}; "LM" is Loftus and Masson
#'      \insertCite{lm94}{superb};
#'      "UA" is based on the unitary Alpha method (derived from the Cronbach alpha;
#'      see \insertCite{lc22}{superb}).
#'      "LDr" is local decorrelation (useful for long time series with autoregressive 
#'      correlation structures; see \insertCite{cppf24}{superb}).
#' * `popsize`: Size of the population under study. Defaults to Inf
#' * `samplingDesign`: Sampling method to obtain the sample. implemented 
#'          sampling is "SRS" (Simple Randomize Sampling) and "CRS" 
#'          (Cluster-Randomized Sampling).
#'
#' The formulas can be for long format data using | notation, e.g., 
#' * `superb( extra ~ group | ID, sleep )`
#'
#' or for wide format, using cbind() or crange() notation, e.g., 
#' * `superb( cbind(DV.1.1, DV.2.1,DV.1.2, DV.2.2,DV.1.3, DV.2.3) ~ . , dta, WSFactors = c("a(2)","b(3)"))`
#' * `superb( crange(DV.1.1, DV.2.3) ~ . , dta, WSFactors = c("a(2)","b(3)"))`
#'
#' The avaialble `plotLayout` are the following:
#' * These are basic plots:
#'     * "bar" Shows the summary statistics with bars and error bars;
#'     * "line" Shows the summary statistics with lines connecting the conditions 
#'           over the first factor;
#'     * "point" Shows the summary statistics with isolated points
#'     * "lineband" illustrates the confidence intervals as a band;
#' * These plots add distributional information in addition
#'     * "pointjitter" Shows the summary statistics along with jittered points depicting 
#'           the raw data;
#'     * "pointjitterviolin" Also adds violin plots to the previous layout
#'     * "pointindividualline" Connects the raw data with line along the first factor 
#'           (which should be a repeated-measure factor)
#'     * "raincloud" Illustrates the distribution with a cloud (half_violin_plot) and 
#'            jittered dots next to it. Looks better when coordinates are 
#'            flipped ``+coord_flip()``
#'     * "corset" illustrates within-subject designs with individual lines and clouds.
#' * Circular plots (aka radar plots) results from the following layouts:
#'     * "circularpoint" Shows the summary statistics with isolated points
#'     * "circularline" Shows the summary statistics with lines;
#'     * "circularlineband" Also adds error bands instead of error bars;
#'     * "circularpointjitter" Shows summary statistics and error bars but also jittered dots;
#'     * "circularpointlinejitter" Same as previous layout, but connect the points with lines.
#' New layouts are added from times to time.
#' Personalized layouts can also be created (see 
#'   \code{vignette("Vignette5", package = "superb")})
#'
#' @references
#'   \insertAllCited
#'
#' @examples
#' # Basic example using a built-in dataframe as data. 
#' # By default, the mean is computed and the error bar are 95% confidence intervals
#' superb(len ~ dose + supp, ToothGrowth) 
#'
#' # Example changing the summary statistics to the median and
#' # the error bar to 80% confidence intervals
#' superb(len ~ dose + supp, ToothGrowth,
#'        statistic = "median", errorbar = "CI", gamma = .80) 
#'
#' # Example introducing adjustments for pairwise comparisons 
#' # and assuming that the whole population is limited to 200 persons
#' superb(len ~ dose + supp, ToothGrowth,  
#'   adjustments = list( purpose = "difference", popSize = 200) )
#'
#' # This example adds ggplot directives to the plot produced
#' library(ggplot2)
#' superb(len ~ dose + supp, ToothGrowth) + 
#' xlab("Dose") + ylab("Tooth Growth") +
#' theme_bw()
#'
#' 
#'
#' # The following examples are based on repeated measures
#' library(gridExtra)
#' options(superb.feedback = 'none') # shut down 'warnings' and 'design' interpretation messages
#' 
#' # A simple example: The sleep data
#' # The sleep data are paired data showing the additional time of sleep with 
#' # the soporific drug #1 (("group = 1") and with the soporific drug #2 ("group = 2"). 
#' # There is 10 participants with two measurements.
#' # sleep is available in long format 
#' 
#' # Makes the plots first without decorrelation:
#' superb( extra ~ group | ID, sleep )
#' # As seen the error bar are very long. Lets take into consideration correlation...
#' # ...  with decorrelation (technique Correlation-adjusted CA):
#' superb(extra ~ group | ID, sleep, 
#'   # only difference:
#'   adjustments = list(purpose = "difference", decorrelation = "CA")
#' )
#' # The error bars shortened as the correlation is substantial (r = .795).
#' 
#' 
#' 
#' 
#' # Another example: The Orange data
#' # This example contains 5 trees whose diameter (in mm) has been measured at various age (in days):
#' data(Orange)
#'
#' # Makes the plots first without decorrelation:
#' p1 <- superb( circumference ~ age | Tree, Orange,
#'   adjustments = list(purpose = "difference", decorrelation = "none")
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="''Standalone'' confidence intervals")
#' # ... and then with decorrelation (technique Correlation-adjusted CA):
#' p2 <- superb( circumference ~ age | Tree, Orange,
#'   adjustments = list(purpose = "difference", decorrelation = "CA")
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="Decorrelated confidence intervals")
#'
#' # You can present both plots side-by-side
#' grid.arrange(p1, p2, ncol=2)
#'
#' 
#' 
#'
###################################################################################
#'
#' @importFrom Rdpack reprompt
#' @export superb
#'
###################################################################################

# superb() is actually just a proxy to superbPlot()
# Most argument validation is performed in superbPlot()

superb <- function(
    formula, 
    data, 
    WSFactors     = NULL,            # vector of the names of the within-subject factors
    WSDesign      = NULL,            # or ws levels of each variable if not a full factorial ws design
    factorOrder   = NULL,            # order of the factors for plots
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
    plotStyle     = NULL,            # deprecated; use plotLayout
    plotLayout    = "line",          # type of plot ("bar", "line", "point", "pointjitter", etc. many exists)
    preprocessfct = NULL,            # run preprocessing on the matrix
    postprocessfct= NULL,            # run post-processing on the matrix
    clusterColumn = NULL,            # if samplineScheme = CRS
    ...
    # the following are optional list of graphic directives...
    # errorbarParams,                # merged into ggplot/geom_superberrorbar
    # pointParams,                   # merged into ggplot/geom_point
    # lineParams,                    # merged into ggplot/geom_line
    # barParams,                     # merged into ggplot/geom_bar
    # etc.
) {

    ##############################################################################
    # STEP 0: preliminary preparations...
    ##############################################################################
	data <- as.data.frame(data) # coerce to data.frame if tibble or compatible

    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################
    ### if DVvars has crange (column range), replace crange() with cbind()
    if (has.crange.terms(formula)) {
        range <- sub.formulas(formula, "crange")[[1]]
        beg <- match(paste(range[[2]]), names(data))
        end <- match(paste(range[[3]]), names(data))
        tmp <- paste("cbind(", paste(names(data)[beg:end],collapse=","),")", sep="")
        formula[[2]] <- as.formula(paste(tmp," ~ whatever"))[[2]]
    }

    # 1.0: Are the data actually data?
    if(!(is.data.frame(data)))
            stop("superb::ERROR(50): Argument `data` is not a data.frame or similar data structure. Exiting...")

    # 1.1: Is the formula actually a formula?
    if (!is.formula(formula)) 
        stop("superb::ERROR(51): Argument `formula` is not a legitimate formula. Exiting...")

    # 1.2: has the formula 1 or more DV?
    if (is.one.sided( formula )) {
            stop("superb::ERROR(52): Argument `formula` has no DV. Exiting...")
    }

    # 1.3: are the columns named in the formula present in the data?
    ALLvars <- all.vars(formula)  # extract variables, cbind and nested alike
	ALLvars <- ALLvars[!(ALLvars == ".")] # remove .

    if (!(all(ALLvars %in% names(data)))) 
        stop("superb::ERROR(53): Variables in `formula` are not all in `data`. Exiting...")

	# 1.4: If wide format with repeated-measures, are the WSFactors given?
	if ((has.cbind.terms(formula)) && is.null(WSFactors)) 
		stop("superb::ERROR(54): Argument `WSFactors` must be defined in wide format with repeated measures Exiting...")
    if ((has.nested.terms(formula))&& !is.null(WSFactors))
        stop("superb::ERROR(56): If the format is long (as suggested by |), you must not specify argument `WSFactors`. Exiting...")

    # 1.6: Keep only the columns named
    data <- data[, names(data) %in% c(ALLvars,clusterColumn),  drop=FALSE]

	# 1.7: get dependent variable -or- cbind dependent variables
	if (has.cbind.terms(formula)) {
		# extract all vars from cbind
		DVvars <- c()
		for (i in 2:length(formula[[2]])) 
			DVvars <- c(DVvars, paste(formula[[2]][[i]]))
        #print(DVvars)
	} else {
        DVvars <- paste(formula[[2]])
    }

    ##############################################################################
    # STEP 2: Manage long format (i.e., having |)
    ##############################################################################
    if (has.nested.terms(formula)) {
        # a) must locate the unique symbol past |
        IDvar <- getAfterNested(formula)
        ALLfacts <- ALLvars[!(ALLvars == IDvar)&!(ALLvars %in% DVvars)]
        # b) discriminate wsfacts from bsfacts
        nsubjects = dim(unique(data[IDvar]))[1]
        BSfacts <- NULL
        WSfacts <- WSfacts2 <- NULL
        prodWSfacts <- 1

        for (fact in ALLfacts) {
            if (dim(unique(data[c(IDvar, fact)]))[1] == nsubjects ) {
                BSfacts = c(BSfacts, fact)
            } else {
                nlevels <- dim(unique(data[fact]))[1]
                WSfacts <- c(WSfacts, fact)
                prodWSfacts <- prodWSfacts * nlevels
                WSfacts2 <- c(WSfacts2, paste(fact,"(",nlevels,")",sep=""))
            }
        }

        # c) if WSDesign was not specified, check if it is a full-factorial design
        if (is.null(WSDesign)) {
            temp <- unique(data[c(WSfacts)])
            prodCondits <- dim( temp )[1]
            if (prodCondits == prodWSfacts) { 
                wsdesign <- "fullfactorial"
            } else {
                if ('design' %in% getOption("superb.feedback") )
                    message("superb::FYI: The design is not full factorial. It is infered from the data...")
                temp <- temp[do.call(order, temp[c(WSfacts)]),]
                wsdesign <- mapply( c , data.frame(t(temp)), SIMPLIFY=FALSE )
            }
        } else {
            wsdesign <- WSDesign
        }
    } else {
        BSfacts <- ALLvars[!(ALLvars %in% DVvars)]
        if (length(BSfacts) == 0) BSfacts = NULL #it could be character(0)...
        WSfacts <- WSfacts2 <- NULL
        if (is.null(WSDesign)) {
            wsdesign <- "fullfactorial"
        } else {
            wsdesign = WSDesign
        }
    }


    ##############################################################################
    # STEP 3: Harmonize the data format to wide if needed
    ##############################################################################

    if (has.nested.terms(formula)) {
        # Widen the data file
        data <- superbToWide( data, IDvar, BSfacts, WSfacts, DVvars)
        # ALLvars must be reactualized...
        BSfacts2 <- if(is.null(BSfacts)) "" else BSfacts
        DVvars <- names(data)[!(names(data)==IDvar)&!(names(data) %in% BSfacts2)]
        # print("File has been widened...")
    } else {
        # print("nothing to do...")
    }


    ##############################################################################
    # STEP 4: All done! transfer to superbPlot()
    ##############################################################################

    #cat(paste("   variables = ", paste(DVvars, collapse=','), "\n", sep=''))
    #cat(paste("   BSFactors = ", paste(BSfacts, collapse=','), "\n", sep=''))
    #cat(paste("   WSFactors = ", paste(WSfacts2, collapse=','), "\n", sep=''))
    #print(if (length(WSfacts2)==0 ) WSFactors else WSfacts2)
    #cat(">>>Transfering to superbPlot() now...\n")

    # we trigger the plot generation here
    superbPlot(data = data,
        variables   = DVvars,
        BSFactors   = BSfacts,
        WSFactors   = if (length(WSfacts2)==0 ) WSFactors else WSfacts2, 
        WSDesign    = wsdesign,
        statistic   = statistic, 
        errorbar    = errorbar, 
        gamma       = gamma,
        adjustments = adjustments,
        factorOrder = factorOrder,
        plotStyle   = plotStyle,
        plotLayout  = plotLayout,
        showPlot    = showPlot,
        preprocessfct  = preprocessfct, 
        postprocessfct = postprocessfct,
        clusterColumn  = clusterColumn,
        ...
    )
}


##############################################################################
# Subfunction
##############################################################################

getAfterNested <- function(frm) {
	# There are two possible cases?
	#   frm1 <- b ~ BSFactors + WSConditions | Id 
	#   frm2 <- b ~ WSConditions | Id 
	f <- sub.formulas(frm, "|")[[1]]
	if (length(f[[3]]) == 1) {
		v1 <- f[[3]]
	} else {
        stop("superb::ERROR(55): Only a single identifier allowed past |. Exiting...")
	}
	return( paste(v1)  )
}


