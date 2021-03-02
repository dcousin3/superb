######################################################################################
#' @title GRD
#'
#' @description Generate Random Data (GRD) is a function that
#' generates a data frame of random data suitable for analyses.
#'
#'
#'
#' @param asdf asf 
#' @param asfd adsf 
#' @param asdf adf 
#' @param adsff adf asdf
#' @param asdf asfdasdf
#' @param asdf adfsasdf
#' @param adsf adfsasdf
#' @param adsf asdfasdf
#'
#'
#'
#' @return 
#'
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
#' @export GRD
######################################################################################

GRD <- function(
    RenameDV = "DV",
    SubjectsPerGroup = 100,
    BSFactors = "",
    WSFactors = "",
    Effects = list(),
    Population  = list(mean = 0, stddev = 1, rho = 0, scores="rnorm(1, mean = GM, sd = STDDEV)"),
    Contaminant = list(mean = 0, stddev = 1, rho = 0, scores="rnorm(1, mean = CGM, sd = CSTDDEV)", proportion = 0),
    Debug = FALSE, 
    Summary = FALSE
){
    #####################################################
    # STEP 0: Load required library
    #####################################################
    require(stats)
    require(MASS)
    require(matrixcalc)
    
    #####################################################
    # STEP 1: Determining experimental design
    #####################################################
  
    # initialize variables
    BSList <- list()
    WSList <- list()
    # determining between-group factors
    if (!(BSFactors=="")) {
        BSList <- grdUnpacker(BSFactors, ':', 'BS')
    }
    ngroups <- prod(unlist(lapply(BSList,length)))
    runDebug(Debug, "processing BSFactors", 
        c("BSList","ngroups"),list(BSList,ngroups))

    # determining repeated measures
    if (!(WSFactors=="")) {
        unpacked <- grdUnpacker(WSFactors, ':', 'WS')
        if (any(names(unpacked) %in% names(BSList))) 
            stop('Unique names across BSFactors and WSFactors list must be provided')
        WSList <- unpacked
    }
    nreplic <- prod(unlist(lapply(WSList,length)))
    runDebug(Debug, "processing WSFactors", 
        c("WSList","nreplic"),list(WSList,nreplic))

    # validating groups sizes
    if ((length(SubjectsPerGroup)!= 1)&&(length(SubjectsPerGroup)!=ngroups)) 
        stop('There is ',ngroups, " groups to be created but only ",length(SubjectsPerGroup), " have been defined")
  
    # setting matrix size, Columns and Rows
    BSnames <- names(BSList)
    WSnames <- names(WSList)
    facnames <- c(BSnames, WSnames)
    cols <- 2 + length(facnames) # variable "id" et "DV" ajoutée
    subj <- if(length(SubjectsPerGroup)==1) {
        SubjectsPerGroup * ngroups
    } else {
        sum(SubjectsPerGroup) 
    }
    rows = subj * nreplic
    runDebug(Debug, "processing matrix size", 
        c("BSnames","WSnames","facnames","cols","rows","subj"   ),
        list(BSnames,WSnames,facnames,cols,rows,subj))

    if (Summary) {
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
        stop("Warning: the rho parameter is non-zero but there is no within-subject factors. Exiting...")
    }
    if ((nreplic >0)&&(RHO != 0)) { # turn multivariate mode on
        NREPLICATION = nreplic
        multivariate = TRUE
        if(length(GM)==1) { 
            GM = rep(GM, nreplic)
        } else if (length(GM) != nreplic) {
            stop(paste("The mean is a vector of inadequate length for",nreplic,"replications"))
        } 
        if(length(STDDEV)==1) {
            SIGMA = diag(nreplic) * STDDEV*STDDEV + (1-diag(nreplic))* STDDEV*STDDEV * RHO
        } else if (length(STDDEV)!=nreplic) {
            stop(paste("The standard deviation is a vector of inadequate length for",nreplic,"replications"))
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
            stop(paste("The mean is a vector of inadequate length for",nreplic,"replications"))
        } 
        if(length(CSTDDEV)==1) {
            CSIGMA = diag(nreplic) * CSTDDEV*CSTDDEV + (1-diag(nreplic))* CSTDDEV*CSTDDEV * CRHO
        } else if (length(CSTDDEV)!=nreplic) {
            stop(paste("The standard deviation is a vector of inadequate length for",nreplic,"replications"))
        } else {
            CSIGMA = diag(nreplic) * CSTDDEV*CSTDDEV + (1-diag(nreplic))* outer(CSTDDEV,CSTDDEV) * CRHO
        }
        if(is.null(Contaminant$scores))  {
            CDIST   <- "mvrnorm(1, mu = CGM, Sigma = CSIGMA)" }
    }


    # generating a function F0 (population) and C0 (contaminant) from this
    neffects=0
    allfactors <- paste(c("id", BSnames, WSnames, "DV"), sep="", collapse = ", ")
    fctstra <- paste("F0 <- function(", allfactors, ") { ", DIST, " }", sep = "" )      
    eval(parse(text=fctstra))
    fctstrb <- paste("C0 <- function(", allfactors, ") { ", CDIST, " }", sep = "" )
    eval(parse(text=fctstrb))

    runDebug(Debug, "processing population", 
        c("GM","STDDEV","RHO","DIST","fctstra","CGM","CSTDDEV","CRHO","CDIST","PROP","fctstrb"),
        list(GM,STDDEV,RHO,DIST,fctstra,CGM,CSTDDEV,CRHO,CDIST,PROP,fctstrb))
    if ((RHO != 0)||(CRHO != 0)) {
        runDebug(Debug, "processing multivariate population", 
            c("SIGMA","CSIGMA"),
            list(SIGMA,CSIGMA))
    }


    #####################################################
    # STEP 4: Getting effect definitions
    #####################################################
    # make functions that mapped to the data returns the effect  
    if (length(Effects) > 0 ) { 
        for (i in 1:length(Effects)) {
            neffects = neffects + 1
            # make effect and run it.
            fctstr = grdMakeEffect(neffects, names(Effects)[i], Effects[[i]], 
                c(BSList, WSList), data, allfactors, Debug
            )
            eval(parse(text=fctstr))
        }
    }
  
  
    ############################################
    # STEP 5: Applying all the effects to DV
    ############################################
    # runs F0 effect
    if (multivariate) {
        temp <- function(args) { do.call(what="F0",args=as.list(args),quote=FALSE)}
        deltaDV  <- as.vector( t(apply(data[1:subj,], 1, temp)) ) 
    } else {
        temp <- function(args) { do.call(what="F0",args=as.list(args),quote=FALSE)}
        deltaDV  <- apply(data, 1, temp)    
    }
    data$DV <- data$DV + deltaDV
    
    # runs all effects created in STEP 4
    if (neffects > 0) {
        for (effect in 1 : neffects) {
            f <- paste("F", effect, sep="")
            temp <- function(args) { do.call(what=f,args=as.list(args),quote=FALSE)}
            deltaDV  <- apply(data, 1, temp)    
            data$DV <- data$DV + deltaDV
        }
    } 

    # runs CO, the contaminant function
    if (cmultivariate) {
        temp   <- function(args) { do.call(what="C0",args=as.list(args),quote=FALSE)}
        contam <- t(apply(data[1:subj,], 1, temp) )
        popula <- matrix(data$DV, nrow = subj, byrow= FALSE)
        ps     <- runif(subj)
        temp   <- function(de, a, b, p) {if(de < p) {a} else {b} }
        ttt    <- t(mapply(temp, ps, contam, popula, p=PROP))
        data$DV <- as.vector(ttt)
    } else {
        temp   <- function(args) { do.call(what="C0",args=as.list(args),quote=FALSE)}
        contam <- apply(data, 1, temp)    
        ps     <- runif(rows)
        temp   <- function(de, a, b, p) {if(de < p) {a} else {b} }
        data$DV <- mapply(temp, ps, contam, data$DV, p=PROP)
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


##################################################################   
# Aesthetic private functions
##################################################################   

# These provide nicer inputs to grd
slope       <- function(s)   { c(-97, s)   }
range       <- function(s)   { c(-98, s)   }
custom      <- function(...) { c(-99,...)  }
Rexpression <- function(s)   { c("-96", paste(c(s),collapse="") ) }


##################################################################   
# Subsidiary private functions: grdMakeEffect; grdL2W; grdDepacker; grdUnpacker;
##################################################################   

grdMakeEffect <- function (fnumber, name, details, WBSList, data, allfactors, Debug) {

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
        if (details[1] == -98) { # range effect 
            eff = (0:(pfaclevel-1))*details[2]/(pfaclevel-1)-details[2]/2
        }
        if (details[1] == -99) { # custom effect 
            if (pfaclevel != length(details)-1 ) stop("The number of custom number does not match ",pfaclevel)
              eff = details[2:length(details)]
        }

        # get all the observed combination of levels
        comb=apply(expand.grid(WBSList[fullnames]),1,paste,collapse="")
        swit=paste("paste(",paste("paste(c(",fullnames,"),collapse='')",sep="",collapse=","),",sep=''), ")
        subs=paste("'",comb,"'=",eff, sep="",collapse=",")
        fctstr= paste("F",fnumber," <- function(", allfactors, ") { ",
                 "switch( ",swit,subs,") }", sep = "" )
    }
    
    runDebug(Debug, paste("Effect functions",fnumber), 
        c("eff","pfaclevel","fctstr"),
        list(eff,pfaclevel,fctstr))

    fctstr  
}


grdL2W <- function (data, within, dv = "DV") {
    # the function grdL2W taken from Navarro, "lsr" library longToWide.
    # The original function is a bit different so I made a few minor changes
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
                stop('Unique names must be provided')
            out[[varname]] <- lvls
        }
        # if a list of levels is specified
        else  {
            # Identify levels
            lvls <- gsub(".*\\((.*)\\).*", "\\1", varnlvls)
            lvls <- unlist(strsplit(lvls,","))
              
            #If it is a single number, assume it is the number of levels, not its name
            if (length(lvls) == 1 && suppressWarnings(!is.na(as.numeric(lvls))))  {
                lvls <- 1:lvls
            }
            if (varname %in% names(out)) 
                stop('Unique names must be provided')
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
