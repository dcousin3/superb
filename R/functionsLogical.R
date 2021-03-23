
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
    runDebug("is.superbPlotfunction", "entering is.superbPlotfunction", c("fcttested"),list(fctname) )
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
                        fake ) ) ); 
            "ggplot" %in% class(test)},
            error = function(cond) {return(FALSE)} 
        )
    }
    runDebug("is.superbPlotfunction", "exiting is.superbPlotfunction", c(),list() )
    res
}


