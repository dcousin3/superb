###################################################################################
#' @title logical functions for formulas
#'
#' @aliases is.formula is.one.sided has.nested.terms has.cbind.terms has.crange.terms in.formula sub.formulas
#'
#' @md
#'
#' @description The functions `is.formula()`,
#'      `is.one.sided()`, `has.nested.terms()`, 
#'      `has.cbind.terms()`, `has.crange.terms()`, 
#'      `in.formula()` and `sub.formulas()`
#'      performs checks or extract sub-formulas from a given formula.
#'
#' @usage is.formula(frm)
#' @usage is.one.sided(frm)
#' @usage has.nested.terms(frm)
#' @usage has.cbind.terms(frm)
#' @usage has.crange.terms(frm)
#' @usage in.formula(frm, whatsym)
#' @usage sub.formulas(frm, head)
#'
#' @param frm a formula;
#' @param whatsym a symbol to search in the formula;
#' @param head the beginning of a sub-formula to extract
#'
#' @return `is.formula(frm)`, `has.nested.terms(frm)`, 
#'      `has.crange.terms(frm)` and `has.cbind.terms(frm)`
#'      returns TRUE if frm is a formula, contains a `|` or a 
#'      `crange` or a `cbind` respectively;
#'      `in.formula(frm, whatsym)` returns TRUE if the symbol `whatsym` is somewhere in 'frm';
#'      `sub.formulas(frm, head)` returns a list of all the sub-formulas which contains `head`.
#'
#' @details These formulas are for internal use.
#'
#' @examples
#' is.formula( Frequency ~ Intensity * Pitch )
#'  
#' has.nested.terms( Level ~ Factor | Level )
#'  
#' has.cbind.terms( Frequency ~ cbind(Low,Medium,High) * cbind(Soft, Hard) )
#'  
#' has.crange.terms( Frequency ~ crange(Low,High) * cbind(Soft, Hard) )
#'  
#' in.formula( Frequency ~ Intensity * Pitch, "Pitch" )
#'  
#' sub.formulas( Frequency ~ cbind(Low,Medium,High) * cbind(Soft, Hard), "cbind" )
#'  
#'
###################################################################################
#'
#' @export is.formula
#' @export is.one.sided
#' @export has.nested.terms
#' @export has.cbind.terms
#' @export has.crange.terms
#' @export in.formula
#' @export sub.formulas
#
###################################################################################

#################################################################################
# logical functions:    
#################################################################################

is.formula <- function( frm ) inherits( frm, "formula")

is.one.sided <- function( frm ) 
    is.formula(frm)  && 
    length(frm) == 2 

has.nested.terms <- function( frm ) {
    if(!is.formula(frm)) return(FALSE)
    return ( in.formula(frm, "|"))
}

has.cbind.terms <- function( frm ) {
    if(!is.formula(frm)) return(FALSE)
    return ( in.formula(frm, "cbind"))
}
has.crange.terms <- function( frm ) {
    if(!is.formula(frm)) return(FALSE)
    return ( in.formula(frm, "crange"))
}


# performs a depth-first search in the language structure.
in.formula <- function( frm, whatsym) {
    # in case a string was in there, convert it to symbol first
    if ((length(frm)==1)) 
        if (is.character(frm)) 
            frm <- as.symbol(frm)

    if ((is.symbol(frm))&&(frm == whatsym)) 
        return(TRUE)

    if (!is.symbol(frm)) { # branches
        for (i in (1:length(frm)) ) {
            if (in.formula( frm[[i]], whatsym) )
                return(TRUE)
        }
    }
    
    return(FALSE)        
}


## Lists all the locations of head of a subformula in formula
sub.formulas <- function( frm, head ) {
    if (!in.formula( frm, head)) stop("error! head not in frm")
    res <- rrapply::rrapply( frm,
            condition = function(x) x == head,
            f = function(x, .xpos) .xpos,
            how = "flatten"
    )
    # grab the terms, removing last index to have the subformula
    lapply(res, function(i) frm[[i[1:(length(i)-1)]]])

}


