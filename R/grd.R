######################################################################################
#' @title Generate random data
#'
#' @md
#'
#' @description The function `GRD()` generates a data frame containing 
#' random data suitable for analyses.
#' The data can be from within-subject or between-group designs.
#' Within-subject designs are in wide format. The function was originally
#' presented in \insertCite{ch19;textual}{superb}.
#'
#' @param RenameDV provide a name for the dependent variable (default DV) 
#' @param SubjectsPerGroup indicates the number of simulated scores per 
#'      group (default 100 in each group) 
#' @param BSFactors a string indicating the between-subject factor(s) 
#'      with, between parenthesis, the number of levels or the list of 
#'      level names. Multiple factors are separated with a colon ":" or 
#'      enumerated in a vector of strings.
#' @param WSFactors a string indicating the within-subject factor(s) in 
#'      the same format as the between-subject factors
#' @param Effects a list detailing the effects to apply to the data. The effects
#'      can be given with a list of `"factorname" = effect_specification` 
#'      or `"factorname1*factorname2" = effect_specification` pairs,
#'      in which effect_specification can either be `slope()`, `extent()`, 
#'      `custom()` and `Rexpression()`. For slope and extent, provide a range,
#'      for custom, indicate the deviation from the grand mean for each cell,
#'      finally, for Rexpression, give between quote any R commands which 
#'      returns the deviation from the grand mean, using the factors.
#'      See the last example below.
#' @param Population a list providing the population characteristics 
#'      (default is a normal distribution with a mean of 0 and standard deviation of 1)
#' @param Contaminant a list providing the contaminant characteristics 
#'      and the proportion of contaminant (default 0)
#'
#'
#' @return a data.frame with the simulated scores.
#'
#' @note Note that the \code{range} effect specification has been renamed
#'    \code{extent} to avoid masking the base function \code{base::range}.
#'
#' @examples
#'  # Simplest example using all the default arguments: 
#'  dta <- GRD()
#'  head(dta)
#'  hist(dta$DV)
#'
#'  # Renaming the dependant variable and setting the group size:
#'  dta <- GRD( RenameDV = "score", SubjectsPerGroup = 200 )
#'  hist(dta$score )
#'
#'  # Examples for a between-subject design and for a within-subject design: 
#'  dta <- GRD( BSFactors = '3', SubjectsPerGroup = 20)
#'  dta <- GRD( WSFactors = "Moment (2)", SubjectsPerGroup = 20)
#'
#'  # A complex, 3 x 2 x (2) mixed design with a variable amount of participants in the 6 groups:
#'  dta <- GRD(BSFactors = "difficulty(3) : gender (2)", 
#'          WSFactors="day(2)",
#'          SubjectsPerGroup=c(20,24,12,13,28,29)
#'        )
#'
#'  # Defining population characteristics :
#'  dta <- GRD( 
#'          RenameDV = "IQ",
#'			SubjectsPerGroup = 20,
#'          Population=list(
#'                       mean=100,  # will set GM to 100
#'                       stddev=15  # will set STDDEV to 15
#'                     ) 
#'         )
#'  hist(dta$IQ)
#'
#'  # This example adds an effect along the "Difficulty" factor with a slope of 15
#'  dta <- GRD(BSFactors="Difficulty(5)", SubjectsPerGroup = 100,
#'      Population=list(mean=50,stddev=5), 
#'      Effects = list("Difficulty" = slope(15) )  )
#'  # show the mean performance as a function of difficulty:
#'  superb(DV ~ Difficulty, dta )
#' 
#'  # An example in which the moments are correlated
#'  dta <- GRD( BSFactors = "Difficulty(2)",WSFactors = "Moment (2)", 
#'      SubjectsPerGroup = 250,
#'      Effects = list("Difficulty" = slope(3), "Moment" = slope(1) ),
#'      Population=list(mean=50,stddev=20,rho=0.85)
#'  )
#'  # the mean plot on the raw data...
#'  superb(cbind(DV.1,DV.2) ~ Difficulty, dta, WSFactors = "Moment(2)", 
#'      plotStyle="line",
#'      adjustments = list (purpose="difference") )
#'  # ... and the mean plot on the decorrelated data; 
#'  # because of high correlation, the error bars are markedly different
#'  superb(cbind(DV.1,DV.2) ~ Difficulty, dta, WSFactors = "Moment(2)", 
#'      plotStyle="line",
#'      adjustments = list (purpose="difference", decorrelation = "CM") )
#'  
#'  # This example creates a dataset in a 3 x 2 design. It has various effects,
#'  # one effect of difficulty, with an overall effect of 10 more (+3.33 per level),
#'  # one effect of gender, whose slope is 10 points (+10 points for each additional gender),
#'  # and finally one interacting effect, which is 0 for the last three cells of the design:
#'  GRD(
#'      SubjectsPerGroup = 10,
#'      BSFactors  = c("difficulty(3)","gender(2)"), 
#'      Population = list(mean=100,stddev=15), 
#'      Effects    = list(
#'          "difficulty" = extent(10),
#'          "gender"=slope(10),
#'          "difficulty*gender"=custom(-300,+200,-100,0,0,0) 
#'      ) 
#'  )
#'  
#'  
#' @references
#' \insertAllCited{} 
#' 
#' @importFrom Rdpack reprompt
#' @export GRD
#' @import stats
#' @import MASS
#'
######################################################################################

GRD <- function(
    RenameDV = "DV",
    SubjectsPerGroup = 100,
    BSFactors = "",
    WSFactors = "",
    Effects = list(),
    Population  = list(mean = 0, stddev = 1, rho = 0, scores="rnorm(1, mean = GM, sd = STDDEV)"),
    Contaminant = list(mean = 0, stddev = 1, rho = 0, scores="rnorm(1, mean = CGM, sd = CSTDDEV)", proportion = 0)
){
    #####################################################
    # STEP 0: Load required library
    #####################################################
    #require(stats)      # all the statistics function
    #require(MASS)       # for mvrnorm

    #####################################################
    # STEP 1: Determining experimental design
    #####################################################

    # initialize variables
    BSList <- list()
    WSList <- list()
    # determining between-group factors
    if (!(all(BSFactors==""))) {
        BSList <- grdUnpacker(BSFactors, ':', 'BS')
    }
    ngroups <- prod(unlist(lapply(BSList,length)))
    runDebug("GRD:1.1", "GRD:1.1: Processing BSFactors", 
        c("BSList","ngroups"),list(BSList,ngroups))

    # determining repeated measures
    if (!(all(WSFactors==""))) {
        unpacked <- grdUnpacker(WSFactors, ':', 'WS')
        if (any(names(unpacked) %in% names(BSList))) 
            stop('GRD::error(1): Unique names across BSFactors and WSFactors list must be provided')
        WSList <- unpacked
    }
    nreplic <- prod(unlist(lapply(WSList,length)))
    runDebug("GRD:1.2", "GRD:1.2: Processing WSFactors", 
        c("WSList","nreplic"),list(WSList,nreplic))

    # validating groups sizes
    if ((length(SubjectsPerGroup)!= 1)&&(length(SubjectsPerGroup)!=ngroups)) 
        stop('GRD::error(2): There is ',ngroups, " groups to be created but only ",length(SubjectsPerGroup), " have been defined")

    # setting matrix size, Columns and Rows
    BSnames  <- names(BSList)
    WSnames  <- names(WSList)
    facnames <- c(BSnames, WSnames); cols <- 2+ length(facnames) # variable "id" et "DV" ajout?e
    subj <- if(length(SubjectsPerGroup)==1) {
        SubjectsPerGroup * ngroups
    } else {
        sum(SubjectsPerGroup) 
    }
    rows = subj * nreplic
    runDebug("GRD:1.3", "GRD:1.3: Processing matrix size", 
        c("BSnames","WSnames","facnames","cols","rows","subj"   ),
        list(BSnames,WSnames,facnames,cols,rows,subj))

    if ('summary' %in% getOption("superb.feedback") ) {
        grdShowDesign(BSList, WSList, ngroups, nreplic, subj, SubjectsPerGroup)
    }

  
    #####################################################
    # STEP 2: Generating the data matrix long
    #####################################################

    # an empty data.frame with all the columns
    data <- data.frame(matrix(0, 
        nrow=rows, ncol=cols, byrow = TRUE,
        dimnames=list(c(), c("id", BSnames,  WSnames, "DV") ))
    )

    # set ids
    data$id = rep(1:subj,nreplic)
    # set group identifiers
    if(length(BSList)>0) {
        design <- expand.grid(BSList)
   
        for (i in 1:length(BSList)) {
            toto = mapply(rep, design[,i], SubjectsPerGroup)
            toto <- if (length(SubjectsPerGroup)>1) 
                {unlist(toto)} else {as.vector(toto)}
            data[,1+i] = toto
        }
    }

    # set repeated measure identifiers
    if(length(WSList)>0) {
        design <- expand.grid(WSList)
        for (i in 1:length(WSList)) {
            data[,1+length(BSList)+i] = unlist(lapply(design[,i],rep,times=subj))
        }
    }

    runDebug("GRD:2", "GRD:2: Processing base matrix", 
        c("dataMatrix"),
        list(data))

  
    #####################################################
    # STEP 3: Getting population/Contaminant definitions
    #####################################################
    # getting the population parameters definition
    GM     <- if(!is.null(Population$mean))        Population$mean   else 0
    STDDEV <- if(!is.null(Population$stddev))      Population$stddev else 1
    RHO    <- if(!is.null(Population$rho))         Population$rho    else 0
    DIST   <- if(!is.null(Population$scores))      Population$scores else "rnorm(1, mean = GM, sd = STDDEV)"
    # getting the population parameters definition
    CGM    <- if(!is.null(Contaminant$mean))       Contaminant$mean       else 0
    CSTDDEV<- if(!is.null(Contaminant$stddev))     Contaminant$stddev     else 1
    CRHO   <- if(!is.null(Contaminant$rho))        Contaminant$rho        else 0
    CDIST  <- if(!is.null(Contaminant$scores))     Contaminant$scores     else "rnorm(1, mean = CGM, sd = CSTDDEV)"
    PROP   <- if(!is.null(Contaminant$proportion)) Contaminant$proportion else 0
    SIGMA  <- CSIGMA  <- diag(2)

    multivariate  = FALSE
    cmultivariate = FALSE
    if ((nreplic == 1)&&((RHO != 0)||(CRHO != 0))) {
        stop("GRD::error(3): Warning: the rho parameter is non-zero but there is no within-subject factors. Exiting...")
    }
    if ((nreplic >0)&&(RHO != 0)) { # turn multivariate mode on
        NREPLICATION = nreplic
        multivariate = TRUE
        if(length(GM)==1) { 
            GM = rep(GM, nreplic)
        } else if (length(GM) != nreplic) {
            stop(paste("GRD::error(4): The mean is a vector of inadequate length for",nreplic,"replications"))
        } 
        if(length(STDDEV)==1) {
            SIGMA = diag(nreplic) * STDDEV*STDDEV + (1-diag(nreplic))* STDDEV*STDDEV * RHO
        } else if (length(STDDEV)!=nreplic) {
            stop(paste("GRD::error(5): The standard deviation is a vector of inadequate length for",nreplic,"replications"))
        } else {
            SIGMA = diag(nreplic) * STDDEV*STDDEV + (1-diag(nreplic))* outer(STDDEV,STDDEV) * RHO        
        }
        if (is.null(Population$scores))   {
            DIST   <- "mvrnorm(1, mu = GM, Sigma = SIGMA)" }
    }
    if ((nreplic >0)&&(CRHO != 0)) { # turn multivariate mode on
        NREPLICATION = nreplic
        cmultivariate = TRUE
        if(length(CGM)==1) { 
            CGM = rep(CGM, nreplic)
        } else if (length(CGM) != nreplic) {
            stop(paste("GRD::error(6): The mean is a vector of inadequate length for",nreplic,"replications"))
        } 
        if(length(CSTDDEV)==1) {
            CSIGMA = diag(nreplic) * CSTDDEV*CSTDDEV + (1-diag(nreplic))* CSTDDEV*CSTDDEV * CRHO
        } else if (length(CSTDDEV)!=nreplic) {
            stop(paste("GRD::error(7): The standard deviation is a vector of inadequate length for",nreplic,"replications"))
        } else {
            CSIGMA = diag(nreplic) * CSTDDEV*CSTDDEV + (1-diag(nreplic))* outer(CSTDDEV,CSTDDEV) * CRHO
        }
        if(is.null(Contaminant$scores))  {
            CDIST   <- "mvrnorm(1, mu = CGM, Sigma = CSIGMA)" }
    }

    runDebug("GRD:3", "GRD:3: Processing population", 
        c("GM","STDDEV","RHO","DIST","CGM","CSTDDEV","CRHO","CDIST","PROP"),
        list(GM,STDDEV,RHO,DIST,CGM,CSTDDEV,CRHO,CDIST,PROP))
    if ((RHO != 0)||(CRHO != 0)) {
        runDebug("GRD.rho", "GRD.rho: Processing multivariate population", 
            c("SIGMA","CSIGMA"),
            list(SIGMA,CSIGMA))
    }


    #####################################################
    # STEP 4: Getting effect definitions
    #####################################################

    # generating a function F0 (population) and C0 (contaminant) from this
    neffects=0
    allfactors <- paste(c("id", BSnames, WSnames, "DV"), sep="", collapse = ", ")
    fctstra <- paste("F0 <- function(", allfactors, ") { ", DIST, " }", sep = "" )      
    eval(parse(text=fctstra))
    fctstrb <- paste("C0 <- function(", allfactors, ") { ", CDIST, " }", sep = "" )
    eval(parse(text=fctstrb))

    # make functions that when mapped to the data returns the effect  
    if (length(Effects) > 0 ) { 
        for (i in 1:length(Effects)) {
            neffects = neffects + 1
            # make effect and run it.
            fctstr = grdMakeEffect(neffects, names(Effects)[i], Effects[[i]], 
                c(BSList, WSList), data, allfactors
            )
            eval(parse(text=fctstr))
        }
    }
    runDebug("GRD:4", "GRD:4: Processing the effects", 
        c("fctstrF","fctstrC"),
        list(fctstra,fctstrb))
  
  
    ############################################
    # STEP 5: Applying all the effects to DV
    ############################################
    # runs F0 effect
    if (multivariate) {
        temp1    <- function(args) { do.call(what="F0", args=as.list(args), quote=FALSE)}
        deltaDV  <- as.vector( t(apply(data[1:subj,], 1, temp1)) ) 
    } else {
        temp1    <- function(args) { do.call(what="F0", args=as.list(args), quote=FALSE)}
        deltaDV  <- apply(data, 1, temp1)    
    }
    data$DV <- data$DV + deltaDV
  
    # runs all effects created in STEP 4
    if (neffects > 0) {
        for (effect in 1 : neffects) {
            f <- paste("F", effect, sep="")
            temp2 <- function(args) { do.call(what=f,args=as.list(args),quote=FALSE)}
            deltaDV  <- apply(data, 1, temp2)
            data$DV <- data$DV + deltaDV
        }
    } 

    # runs CO, the contaminant function
    if (cmultivariate) {
        temp3   <- function(args) { do.call(what="C0",args=as.list(args),quote=FALSE)}
        contam <- t(apply(data[1:subj,], 1, temp3) )
        popula <- matrix(data$DV, nrow = subj, byrow= FALSE)
        ps     <- runif(subj)
        temp4   <- function(de, a, b, p) {if(de < p) {a} else {b} }
        ttt    <- t(mapply(temp4, ps, contam, popula, p=PROP))
        data$DV <- as.vector(ttt)
    } else {
        temp3   <- function(args) { do.call(what="C0",args=as.list(args),quote=FALSE)}
        contam <- apply(data, 1, temp3)    
        ps     <- runif(rows)
        temp4   <- function(de, a, b, p) {if(de < p) {a} else {b} }
        data$DV <- mapply(temp4, ps, contam, data$DV, p=PROP)
    }

  
    #####################################################
    # STEP 6: Let's clean the place
    #####################################################
    # Rename DV with its name
    colnames(data)[colnames(data) == "DV"] <- gsub("[[:space:]]", "", RenameDV)

    # put into wide format
    if (length(WSList) > 0) {
        data <- grdL2W(data, WSnames, RenameDV )
    }
  
  
    #####################################################
    # STEP 7: All done
    #####################################################
    return(data)
}



######################################################################################
#' @title Effect description
#'
#' @aliases slope extent custom Rexpression
#'
#' @description There is four ways that effects can be defined
#' in GRD. `"factor" = slope(s)` will vary the means by an amount of s for 
#' each step of the factor; `"factor" = extent(s)` will vary the means
#' uniformly so that there is a difference of s between the first and
#' the last factor level; `"factor" = custom(a,b,c..)` will alter each
#' means by an amount of a for the first, b for the second, etc. Finally
#' `"factor" = Rexpression("R code")` will apply R code to all levels of 
#' the factors, altering the base mean.
#'
#' @usage slope(s)
#' @usage extent(s)
#' @usage custom(...)
#' @usage Rexpression(str)
#'
#' @param s the size of the effect 
#' @param ... a sequence with the sizes of the effects
#' @param str R code string
#'
#' @return These internal functions are not meant to be used in 
#' isolation in any meaningful way...
#'
#' @export slope
#' @export extent
#' @export custom
#' @export Rexpression
#'

##################################################################   
# Aesthetic functions
##################################################################   

# These provide nicer inputs to grd
slope       <- function(s)   { c(-97, s)   }
extent      <- function(s)   { c(-98, s)   }
custom      <- function(...) { c(-99, ...) }
Rexpression <- function(str)   { c("-96", paste(c(str),collapse="") ) }


##################################################################   
# Subsidiary private functions: grdMakeEffect; grdL2W; grdDepacker; grdUnpacker;
##################################################################   

grdMakeEffect <- function (fnumber, name, details, WBSList, data, allfactors ) {

    if (details[1] == "-96") {
        # this is an R expression
        pfaclevel = 0 # unused
        eff = details[2]
        fctstr= paste("F",fnumber," <- function(", allfactors, ") { ",
            eff, "}", sep = "" )
        
    } else {
        # get factors named in the effect, their number of levels
        fullnames = grdDepacker(name)
        temp <- function(onename){
            length(WBSList[which(names(WBSList)==onename)][[1]])
        }

        faclevels <- unlist(lapply(fullnames,temp))
        pfaclevel <- prod(faclevels)

        # get the effects for each level
        if (details[1] == -97) { # slope effect 
            eff = (0:(pfaclevel-1))*details[2]-details[2]*(pfaclevel-1)/2
        }
        if (details[1] == -98) { # extent effect 
            eff = (0:(pfaclevel-1))*details[2]/(pfaclevel-1)-details[2]/2
        }
        if (details[1] == -99) { # custom effect 
            if (pfaclevel != length(details)-1 ) stop("GRD::error(8): The number of custom number does not match ",pfaclevel)
              eff = details[2:length(details)]
        }

        # get all the observed combination of levels
        comb=apply(expand.grid(WBSList[fullnames]),1,paste,collapse="")
        swit=paste("paste(",paste("paste(trimws(c(",fullnames,")),collapse='')",sep="",collapse=","),",sep=''), ")
        subs=paste("'",comb,"'=",eff, sep="",collapse=",")
        fctstr= paste("F",fnumber," <- function(", allfactors, ") { ",
                 "switch( ",swit,subs,") }", sep = "" )
    }

    runDebug("GRD.Effect", paste("GRD.Effect: Processing effect declaration ", fnumber), 
        c("eff","pfaclevel","fctstr"),
        list(eff,pfaclevel,fctstr))

    fctstr  
}


grdL2W <- function (data, within, dv = "DV") {
    # the function grdL2W taken from Navarro, "lsr" library longToWide.
    # The original function is a bit different so I made minor changes.
    idvar <- setdiff(names(data), c(within, dv))
    if (length(within) > 1) {
        collapsed.treatments <- apply(as.matrix(data[, within]), 
            1, paste, collapse = ".")
        data <- data[, setdiff(names(data), within)]
        data$within <- collapsed.treatments
        within <- "within"
    }
    times <- unique(data[, within])
    varying <- list()
    for (i in seq_along(dv)) varying[[i]] <- paste(dv[i], times, sep = ".")

    res <- reshape(data, direction = "wide", 
        idvar = idvar, varying = varying, 
        times = times, v.names = dv, 
        timevar = within)
    rownames(res) <- NULL

    return(res)
}


grdDepacker = function(string, sep_across) {
    # split factor names for effect sizes
    s <- unlist(strsplit(string, split="[*]"))
    s <- gsub("[[:space:]]", "", s)
    return(s)
}


grdUnpacker = function(string, sep_across, defaultfactorname) {
    # split factor names and levels
    # Initialize necessary variables
    out <- list()

    # Split string into its different variables and their levels
    varsnlvls <- gsub("\\s", "", string)
    varsnlvls <- unlist(strsplit(varsnlvls, sep_across))

    for (n in 1:length(varsnlvls))  {
        # Iterate through all the varsnlvls identified
        varnlvls <- varsnlvls[n]

        # Identify variable Names if available, attribute number otherwise
        varname <- unlist(strsplit(varnlvls, '\\('))[1]
        if(varname == '' || suppressWarnings(!is.na(as.numeric(varname)))) {
            varname <- paste(defaultfactorname, n, sep='')
        }
        # If a list of levels is not specified
        if(!grepl('\\(', varnlvls))  {
            if (length(varnlvls) == 1 && suppressWarnings(!is.na(as.numeric(varnlvls))))  {
                #if input is a number, add those levels
                lvls <- 1:varnlvls
            }  else  {
                # if it is just a name
                lvls <- ''
            }
            if (varname %in% names(out)) 
                stop('GRD::error(9): Unique names must be provided')
            out[[varname]] <- lvls
        }
        # if a list of levels is specified
        else  {
            # Identify levels
            lvls <- gsub(".*\\((.*)\\).*", "\\1", varnlvls)
            lvls <- unlist(strsplit(lvls,","))
                
            # If the two levels are numeric, e.g., (1,4) convert to numeric
            if ((length(lvls) == 2) & all(suppressWarnings(!is.na(as.numeric(lvls)))))
                lvls <- as.numeric(lvls[1]):as.numeric(lvls[2])

            # If it is a single number, assume it is the number of levels, not the level name
            if (length(lvls) == 1 && suppressWarnings(!is.na(as.numeric(lvls))))  {
                lvls <- 1:lvls
            }
            if (varname %in% names(out)) 
                stop('GRD::error(10): Unique names must be provided')
            out[[varname]] <- lvls
        }
    }
    return(out)
}


##################################################################   
# small private functions: grdShowDesign;
##################################################################   

grdShowDesign <- function(BSList,WSList,ngroups,nreplic,subj,SubjectsPerGroup) {
    cat(paste(rep('-',60),collapse=""), '\nDesign is: ')
    if (length(BSList)>0) {
        cat( paste(unlist(lapply(BSList,length)),collapse=" x "))
    }
    if ((length(WSList)>0)&&(length(BSList)>0)) { cat(" x ") }
    if (length(WSList)>0) {
        cat("(", paste(unlist(lapply(WSList,length)),collapse=" x "),")")
    }
    cat(paste(" with",ngroups,"independent groups."))
    cat("\n", paste(rep('-',60),collapse=""),sep="")
    #BS factors
    cat('\n1.Between-Subject Factors (', ngroups, 'groups ) :')
    for (name in names(BSList)) cat('\n\t', name, '; levels: ', paste(BSList[[name]], collapse=', '), sep='')
   
    #WS factors
    cat('\n2.Within-Subject Factors (',nreplic, 'repeated measures ):')
    for (name in names(WSList)) cat('\n\t', name, '; levels : ', paste(WSList[[name]], collapse=', '), sep='')
    #SpG
    cat('\n3.Subjects per group (', subj, 'total subjects ):')
    cat('\n\t', SubjectsPerGroup)
    cat("\n",paste(rep('-',60),collapse=""),"\n")
}

    
##################################################################   
# End of grd.
##################################################################   
