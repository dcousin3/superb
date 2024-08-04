#' @details
#'
#' `suberb` is a library to perform descriptive statistics plots
#' based on the superb framework. In a nutshell, the framework 
#' assert that confidence intervals must be devised according to 
#' all the relevant information that can be used to assess precision.
#' For example, confidence intervals should be informed of the presence
#' of within-subject design, of the fact that the sample is random or clustered,
#' of whether the population is finite or infinite, etc.
#' 
#' Would you do a t-test on independent groups when you know that the data
#' are paired? Of course, not! Why use the classic "stand-alone" confidence interval then?
#' These classic confidence intervals are oblivious to most relevant information.
#' 
#' The superb framework is based on the idea that correct, well-informed, confidence 
#' intervals can be obtained with a succession of simple corrections. I call these
#' "adjusted confidence intervals". 
#' 
#' The main function is
#' 
#'    \code{superbPlot(df,  ...)}  
#' 
#' where \code{df} is a dataframe.
#'
#' For more details on the underlying math, see
#' \insertCite{c05,c19,c17,cl16,m08,b12,lm94,gc19}{superb}
#' 
#' A second function inserted in this package is \insertCite{ch19}{superb}
#' 
#'    \code{GRD( ...)}
#' 
#' which generates random datasets. It easily generate ficticious dataset
#' so that superbPlot can be tested rapidly. This function is described in 
#' \insertCite{ch19}{superb}.
#' 
#' @references
#' \insertAllCited{}
#' 
#' The package includes additional, helper, functions: \itemize{
#'      \item{\code{ShroutFleissICC1}} to compute intra-class correlation;
#'      \item{\code{epsilon}} to compute the sphericity measure;
#'      \item{\code{lambda}} to compute the cluster-sampling adjustment;
#'      \item{\code{MauchlySphericityTest}} to perform a test of sphericity;
#'      \item{\code{WinerCompoundSymmetry}} to perform a test of compound symmetry;
#' }
#' and example datasets described in the paper:  \itemize{
#'      \item\code{dataFigure1} illustrate the paradox of using stand-alone CI in between-group design;
#'      \item{\code{dataFigure2}} illustrate the paradox of using stand-alone CI in within-subject design;
#'      \item{\code{dataFigure3}} illustrate the paradox of using stand-alone CI in cluster-randomized sampling study;
#'      \item{\code{dataFigure4}} illustrate the paradox of using stand-alone CI with population of finite size.
#' }
#' 
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"

.onLoad <- function(libname, pkgname) {    
    # Set the default feedback traces displayed to all:
    #   design:   in superbPlot, shows information on how the within-subject variables are understood
    #   warnings: in superbPlot, returns 'FYI' messages about the data to help decide if the appropriate error bars were used
    #   summary:  in GRD, shows a summary of the design;
    # You can use 'all' to see all the feedback informations.
    options( "superb.feedback" = c('design','warnings','summary') )
    # Set the default bootstrap number of iterations to 5000; this is a minimum, avoid reducing it.
    options( "superb.bootstrapIter" = 5000)
    # Set the default display of information in superbShiny()
    options( "superb.shiny" = "silent" ) # default should be "silent" or else "display"
}

.onDetach <- function(libpath) {
    # remove the options
    options( "superb.feedback"	= NULL )
    options( "superb.bootstrapIter" = NULL )
    options( "superb.shiny" = NULL )
}

# to inhibit "no visible binding for global variable" errors from :
# showSignificance :
globalVariables(c("yend","xend","label"))
# superbPlot.bar and all the superbPlot.xxx functions
globalVariables(c("center","lowerwidth","upperwidth","hwlowerwidth","hwupperwidth"))
# superbPlot.pointindividualline
globalVariables(c("increase","id"))
# superbPlot.boxplot
globalVariables(c("DV"))

