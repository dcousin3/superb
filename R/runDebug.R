######################################################################################
#' @title runDebug 
#'
#' @description Plots standard error or confidence interval for various descriptive 
#'      statistics under various designs, sampling schemes, population size and purposes,
#'      according to the ESP framework.
#'
#' @param state sd fasdf
#' @param title sadfasdf
#' @param vars asdfasdf
#' @param vals asdfasdf
#'
#' @return 
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
            assign(vars[i], vals[[i]], envir = globalenv())
        }
    }
}

##################################################################   
# End of runDebug.
##################################################################   
