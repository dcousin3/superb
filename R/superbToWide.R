######################################################################################
#' @title superbToWide: Reshape long data frame to wide, suitable for superbPlot
#'
#' @md
#'
#' @description The function ``suberbToWide()`` is an extension to Navarro's WideToLong function
#'      with ample checks to make sure all is legit, so that the data 
#'      is suitably organized for ``suberb``. See \insertRef{cgh21}{superb} for more.
#'      Other techniques are available to transform long to wide, but many asked for 
#'      it within `superb`.
#'
#' @param data      Dataframe in long format
#' @param id        A column with unique identifiers per subject
#' @param BSFactors The name(s) of the between-subject factor(s) as string(s)
#' @param WSFactors The name(s) of the within-subject factor(s) as string(s)
#' @param variable  The dependent variable as string
#'
#' @return A wide-format data frame ready for `superbPlot()` or `superbData()`. All other 
#'      variables will be erased.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' library(ggplot2)
#' library(gridExtra)
#'
#' # Example using the built-in dataframe Orange. 
#' data(Orange)
#' superbToWide(Orange, id = "Tree", WSFactors = c("age"), variable = "circumference") 
#'
#' # Optional: change column names to shorten "circumference" to "DV"
#' names(Orange) <- c("Tree","age","DV")
#' # turn the data into a wide format
#' Orange.wide <- superbToWide(Orange, id = "Tree", WSFactors = c("age"), variable = "DV") 
#' 
#' # Makes the plots two different way:
#' p1=superbPlot( Orange.wide, WSFactors = "age(7)",
#'   variables = c("DV.118","DV.484","DV.664","DV.1004","DV.1231","DV.1372","DV.1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "none")
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="Basic confidence intervals")
#'
#' p2=superbPlot( Orange.wide, WSFactors = "age(7)",
#'   variables = c("DV.118","DV.484","DV.664","DV.1004","DV.1231","DV.1372","DV.1582"),
#'   adjustments = list(purpose = "difference", decorrelation = "CA")
#' ) + 
#'   xlab("Age level") + ylab("Trunk diameter (mm)") +
#'   coord_cartesian( ylim = c(0,250) ) + labs(title="Decorrelated confidence intervals")
#' grid.arrange(p1,p2,ncol=2)
#'
#' # Note that with superb(), there is no need to reformat
#' # into a wide format anymore:
#' superb( DV ~ age | Tree, Orange )
#'
#'
#' @export superbToWide
#' @importFrom utils capture.output
#  ####@importFrom stats reshape # not anymore, bugging with missings
#' @importFrom reshape2 dcast
#
######################################################################################


superbToWide <- function(data,       # long format
	id			  = NULL,			 # column name with unique identifier per subject
    BSFactors     = NULL,            # vector of the between-subject factor columns
    WSFactors     = NULL,            # vector of the names of the within-subject factors
	variable      = NULL  	         # dependent variable name; if NULL, must be the only unnamed variable
) {

    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################
    # 1.0: is the data actually data!
	data <- as.data.frame(data) # coerce to data.frame if tibble or compatible

    if(!(is.data.frame(data)))
        stop("superb::ERROR: data is not a data.frame or similar data structure. Exiting...")
	if (is.null(data))
		stop("superb::ERROR: No data, so do nothing. Exiting...")

    # 1.2: checking empty within-subject factors or id columns
    if (is.null(WSFactors) || length(WSFactors)==0 ) 
		stop("superb::ERROR: No within-subject factor so nothing to do. Exiting...")
    if (is.null(id) || length(id)==0 ) 
		stop("superb::ERROR: No identifier column provided. Exiting...")

	# 1.2: checking invalid factors
    if (!(all(id %in% names(data)))) 
        stop("superb::ERROR: The id column is not found in data. Exiting...")
    if (!(all(BSFactors %in% names(data)))) 
        stop("superb::ERROR: One of the BSFactors column is not found in data. Exiting...")
    if (!(all(WSFactors %in% names(data)))) 
        stop("superb::ERROR: One of the WSFactors column is not found in data. Exiting...")

	# 1.3: checking invalid dependent variable 
    complement <- function(x, U) {U[is.na(pmatch(U,x))]}
	if (is.null(variable)) {
		if (length(complement( WSFactors, names(data) ) !=1) ) {
			stop("suberb::ERROR: Unable to determine which is the dependent variable. Use 'variable' argument. Exiting...")
		} else {
			variable = complement( WSFactors, names(data) )
		}
	}
	if (!(variable %in% names(data))) 
		stop("superb::ERROR: The dependent variable column is not a column in data. Exiting...")

	# 1.4: preserving only the relevant variables...
	data <- data[ c(id, BSFactors, WSFactors, variable) ]

    # 1.5: Sorting on the id and the within-subject variables
    data <- data[do.call(order, data[c(id,WSFactors)]),]

    # 1.6: We're clear to go! Turn this on with: options(superb.feedback = "superb.tw")
    runDebug("superb.tw", "End of Step 0: superbToWide", 
        c("BSFactors0","WSFactors0","variable0","data0"), 
        list( BSFactors, WSFactors, variable, data ) )


    ##################################################################################
    # STEP 2: We're all good. Lets do the reshaping
    ##################################################################################

    frm <- paste(paste(c(id, BSFactors), collapse="+"), 
                  paste(WSFactors, collapse="+"), 
                  sep="~")
    res <- reshape2::dcast( data, frm, value.var = variable )

    # building the column names is the more complicated part...
    if (length(WSFactors) > 1) { # concatenate the factor levels in a single column 'within'
        collapsed.treatments <- apply(as.matrix(data[, WSFactors]), 1, paste, collapse = ".")
        data <- data[, setdiff(names(data), WSFactors)]
        data$within <- collapsed.treatments # put the concatenated levels in a new column
        WSFactors <- "within"               # use that new column from now on
    }
    times <- unique(data[, WSFactors])
    times <- times[order(times)] # sort them
    varying <- list()
    for (i in seq_along(variable)) varying[[i]] <- paste(variable[i], times, sep = ".")
    names(res) <- c(id, BSFactors, unlist(varying) )

    return(res)
}

