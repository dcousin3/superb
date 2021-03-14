######################################################################################
#' @title runDebug 
#'
#' @description runDebug is an internal function used by GRD and superbPlot
#'      to help in debugging the functions. It assigns in the global environment
#'      the variables that are local to a function so that they become visible.
#'
#' @param state boolean (TRUE to activate runDebug)
#' @param title string text to be displayed when this function is triggered
#' @param vars strings names of the variables to be placed in the globalenvironment
#' @param vals numeric values to be given to the variables.
#'
#' @return puts in the globalenvironment the variables named "vars"
#'
#' @export runDebug
######################################################################################

runDebug <- function(state, title, vars, vals) { 
    # runDebug provides traces of the vars and
    # reassign them in the globalenv so that I can test commands
    if (state) {
        cat(paste("==>",title,"<==\n"))
        for (i in 1:length(vars)) {
            cat(paste("-",vars[i],"- "))
            print(vals[[i]])
            envrt = globalenv() # done in two steps for CRAN
            assign(vars[i], vals[[i]], envir = envrt)
        }
    }
}

##################################################################   
# End of runDebug.
##################################################################   
