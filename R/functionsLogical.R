#' @importFrom methods existsFunction

#################################################################################
# logical functions:    
#################################################################################


has.init.function <- function(fctname) {
    # does the function has a "init.fctname" initializer?
    
    parts <- strsplit(fctname, "::")[[1]]
    if (length(parts) > 1 ) {
        enviro = asNamespace(parts[1]) 
        iname <- paste("init", parts[2], sep=".")
    } else {
        enviro = parent.frame()
        iname <- paste("init", parts[1], sep=".")
    }
    #print(iname)
    #print(enviro)

    if (exists(iname, envir = enviro)) {
        existsFunction(iname, where = enviro)
    } else { FALSE }
}

is.stat.function <- function(fctname) {
    if (has.init.function(fctname) ) # we launch the initialization
        do.call(paste("init", fctname, sep="."), list(data.frame(DV = c(1,2,3) )))

    # does the function provided by the user exists and compute from a list of data? 
    res <- tryCatch(
        {suppressWarnings(do.call(fctname, list( c(1,2,3) ) ) ); TRUE},
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
    if (has.init.function(fctname)) # we launch the initialization
        do.call(paste("init", fctname, sep="."), list(data.frame(DV = c(1,2,3) )))

    # is the function provided by the user an interval, i.e., two numbers (e.g., CI) 
    # or a single width (e.g., SE)?
    res <- suppressWarnings( do.call(fctname, list( c(1,2,3)) ) )
    if (length(res) == 2) TRUE else FALSE
}

is.width.function <- function(fctname) {
    if (has.init.function(fctname)) # we launch the initialization
        do.call(paste("init", fctname, sep="."), list(data.frame(DV = c(1,2,3) )))

    # is the function provided by the user an interval, i.e., two numbers (e.g., CI) 
    # or a single width (e.g., SE)?
    res <- suppressWarnings(do.call(fctname, list( c(1,2,3)) ) )
    if (length(res) == 1) TRUE else FALSE
}

# do.call which accepts the :: notation in function name
# https://stackoverflow.com/a/79331590/5181513
do_call <- function(fun_string, l) {
    pkg_fun <- str2lang(fun_string)
    if (length(pkg_fun) == 1) {
        return(do.call(get(pkg_fun, envir = parent.frame()), l))
    } else {
        return(do.call(get(pkg_fun[[3]], envir = asNamespace(pkg_fun[[2]])), l) )
    }
}

is.gamma.required <- function(fctname) {
    if (has.init.function(fctname)) 
        do.call(paste("init", fctname, sep="."), list(data.frame(DV = c(1,2,3) )))

    # is the function provided by the user requires a coverage factor
    # gamma (e.g., CI) or not (e.g., SE)?
    res <- tryCatch(
        {suppressWarnings( do_call(fctname, list( c(1,2,3), gamma = 0.95) ) ); TRUE},
        error = function(cond) {return(FALSE)}
    )
    res
}

is.superbPlot.function <- function(layoutfctname) {
    # does the plot function provided by the user exists?
    runDebug("is.superbPlot.function", "Entering is.superbPlot.function", c("fcttested"),list(layoutfctname) )
    opts <- getOption("superb.feedback")
    options(superb.feedback = 'none')

    res <- TRUE
    if (!exists(layoutfctname)) {
        res <- FALSE
    } else {
        # if the symbol exists, run a fake call to see if it works...
        stg <- data.frame( 
                dose       = cbind(c(0.5,0.5,1,1,2,2)),
                supp       = cbind(c("VC","OJ","VC","OJ","VC","OJ")),
                center     = cbind(c(13,8,22,17,26,26)),
                lowerwidth = cbind(c(-3,-2,-3,-2,-2,-4)),
                upperwidth = cbind(c(+3,+2,+3,+2,+2,+4))
        )
        fake = datasets::ToothGrowth;
        fake = cbind(fake, DV = fake$len)

        res <- tryCatch(
            {test <- suppressWarnings(do.call(layoutfctname, 
                    list(stg,
                        "dose", 
                        "supp", ".~.", 
                        fake ) ) ); 
            "ggplot" %in% class(test)},
            error = function(cond) {return(FALSE)} 
        )
    }

    # restores feedback information
    options(superb.feedback = opts)
    runDebug("is.superbPlot.function", "Exiting is.superbPlot.function", c(),list() )
    res
}
