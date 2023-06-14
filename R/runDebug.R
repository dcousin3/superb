######################################################################################
#' @title runDebug 
#'
#' @description runDebug is an internal function used by GRD and superb
#'      to help in debugging the functions. It assigns in the global environment
#'      the variables that are local to a function so that they become visible.
#'
#' @md
#'
#' @param where indicates where in the program runDebug was called
#' @param title string text to be displayed when this function is triggered
#' @param vars strings names of the variables to be placed in the global environment
#' @param vals numeric values to be given to the variables.
#'
#' @return puts in the globalenvironment the variables named "vars"
#'
#' @export runDebug
######################################################################################

runDebug <- function(where, title, vars, vals) { 
    # runDebug provides traces of the vars and
    # reassign them in the globalenv so that I can test commands
    if ( (where %in% getOption("superb.feedback")) | ('all' %in% getOption("superb.feedback") ) ){
        cat(paste("==>",title,"<==\n"))
        envrt = globalenv() # done in two steps for CRAN
        if (length(vars) > 0) {
            for (i in 1:length(vars)) {
                assign(vars[i], vals[[i]], envir = envrt)
            }
            cat(paste("    variables dumped in: ", paste(vars ,collapse=", "), "\n", sep=""))
        }
    }
}

##################################################################   
# End of runDebug.
##################################################################   
