## ---- echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
library(ggplot2)
options(superb.feedback = c('design','warnings') )

## ---- eval=FALSE, message=FALSE, warning=FALSE--------------------------------
#  library(ggplot2)            # for the graphing commands
#  library(superb)             # for superbPlot and GRD
#  library(referenceIntervals) # for computing reference intervals

## ---- eval=TRUE, message=FALSE, warning=FALSE, echo=FALSE---------------------
# the above pretend that referenceInterval was installed, but it is not installed
# because its dependencies (gWidgets2tcltk, extremeValues) crashes travis-CI.com and shinyapps.io...
# I reproduce the relevant code here as is
library(ggplot2)            # for the graphing commands
library(superb)             # for superbPlot and GRD
library(boot)
#library(car)               # not working on Travis anymore; this is a nightmare...
library(stats)


### Power families:
basicPower <<- function(U,lambda, gamma=NULL) {
 if(!is.null(gamma)) basicPower(t(t(as.matrix(U) + gamma)), lambda) else{
 bp1 <- function(U,lambda){
  if(any(U[!is.na(U)] <= 0)) stop("First argument must be strictly positive.")
  if (abs(lambda) <= 1.e-6) log(U) else (U^lambda)
  }
  out <- U
  out <- if(is.matrix(out) | is.data.frame(out)){
    if(is.null(colnames(out))) colnames(out) <-
        paste("Z", 1:dim(out)[2],sep="")
    for (j in 1:ncol(out)) {out[, j] <- bp1(out[, j],lambda[j])
       colnames(out)[j] <- if(abs(lambda[j]) <= 1.e-6)
           paste("log(", colnames(out)[j],")", sep="") else
           paste(colnames(out)[j], round(lambda[j], 2), sep="^")}
    out}  else
    bp1(out, lambda)
  out}}

bcPower <<- function(U, lambda, jacobian.adjusted=FALSE, gamma=NULL) {
 if(!is.null(gamma)) bcPower(t(t(as.matrix(U) + gamma)), lambda, jacobian.adjusted) else{
 bc1 <- function(U, lambda){
  if(any(U[!is.na(U)] <= 0)) stop("First argument must be strictly positive.")
  z <- if (abs(lambda) <= 1.e-6) log(U) else ((U^lambda) - 1)/lambda
  if (jacobian.adjusted == TRUE) {
    z * (exp(mean(log(U), na.rm=TRUE)))^(1-lambda)} else z
  }
  out <- U
  out <- if(is.matrix(out) | is.data.frame(out)){
    if(is.null(colnames(out))) colnames(out) <-
        paste("Z", 1:dim(out)[2], sep="")
    for (j in 1:ncol(out)) {out[, j] <- bc1(out[, j], lambda[j]) }
    colnames(out) <- paste(colnames(out), round(lambda, 2), sep="^")
    out}  else
    bc1(out, lambda)
  out}}

yjPower <<- function(U, lambda, jacobian.adjusted=FALSE) {
 yj1 <- function(U, lambda){
  nonnegs <- U >= 0
  z <- rep(NA, length(U))
  z[which(nonnegs)] <- bcPower(U[which(nonnegs)]+1, lambda, jacobian.adjusted=FALSE)
  z[which(!nonnegs)] <- -bcPower(-U[which(!nonnegs)]+1, 2-lambda, jacobian.adjusted=FALSE)
  if (jacobian.adjusted == TRUE)
        z * (exp(mean(log((1 + abs(U))^(2 * nonnegs - 1)), na.rm=TRUE)))^(1 -
            lambda)
    else z
  }
  out <- U
  out <- if(is.matrix(out) | is.data.frame(out)){
    if(is.null(colnames(out))) colnames(out) <-
        paste("Z", 1:dim(out)[2], sep="")
    for (j in 1:ncol(out)) {out[, j] <- yj1(out[, j], lambda[j]) }
    colnames(out) <- paste(colnames(out), round(lambda, 2), sep="^")
    out}  else
    yj1(out, lambda)
  out}

powerTransform <<- function(object, ...) UseMethod("powerTransform")

powerTransform.default <<- function(object, family="bcPower", ...) {
   y <- object
   if(!inherits(y, "matrix") & !inherits(y, "data.frame")) {
       y <- matrix(y,ncol=1)
       colnames(y) <- c(paste(deparse(substitute(object))))}
   y <- na.omit(y)
   x <- rep(1, dim(y)[1])
   estimateTransform(x, y, NULL, family=family, ...)
   }

powerTransform.lm <<- function(object, family="bcPower", ...) {
    mf <- if(is.null(object$model))
            update(object, model=TRUE, method="model.frame")$model
            else object$model
    mt <- attr(mf, "terms")
        y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    if (is.null(w)) w <- rep(1, dim(mf)[1])
    if (is.empty.model(mt)) {
        x <- matrix(rep(1,dim(mf)[1]), ncol=1) }
    else {
        x <- model.matrix(mt, mf)   }
  estimateTransform(x, y, w, family=family, ...)
  }

powerTransform.formula <<- function(object, data, subset, weights,
              na.action, family="bcPower", ...) {
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("object", "data", "subset", "weights", "na.action"),
         names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    names(mf)[which(names(mf)=="object")] <- "formula"
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    if (is.null(w)) w <- rep(1, dim(mf)[1])
    if (is.empty.model(mt)) {
        x <- matrix(rep(1, dim(mf)[1]), ncol=1) }
    else {
        x <- model.matrix(mt, mf)   }
  estimateTransform(x, y, w, family=family, ...)
  }

estimateTransform <<- function(X, Y, weights=NULL, family="bcPower", ...) {
  Y <- as.matrix(Y)
  switch(family,
         bcnPower = estimateTransform.bcnPower(X, Y, weights,  ...),
         estimateTransform.default(X, Y, weights, family, ...)
  )
}

# estimateTransform.default is renamed 'estimateTransform
estimateTransform.default <<- function(X, Y, weights=NULL,
                    family="bcPower", start=NULL, method="L-BFGS-B", ...) { 
  fam <- function (U, lambda, jacobian.adjusted = FALSE, gamma = NULL) {
    if (!is.null(gamma)) 
        bcPower(t(t(as.matrix(U) + gamma)), lambda, jacobian.adjusted)
    else {
        bc1 <- function(U, lambda) {
            if (any(U[!is.na(U)] <= 0)) 
                stop("First argument must be strictly positive.")
            z <- if (abs(lambda) <= 1e-06) 
                log(U)
            else ((U^lambda) - 1)/lambda
            if (jacobian.adjusted == TRUE) {
                z * (exp(mean(log(U), na.rm = TRUE)))^(1 - lambda)
            }
            else z
        }
        out <- U
        out <- if (is.matrix(out) | is.data.frame(out)) {
            if (is.null(colnames(out))) 
                colnames(out) <- paste("Z", 1:dim(out)[2], 
                  sep = "")
            for (j in 1:ncol(out)) {
                out[, j] <- bc1(out[, j], lambda[j])
            }
            colnames(out) <- paste(colnames(out), round(lambda, 
                2), sep = "^")
            out
        }
        else bc1(out, lambda)
        out
    }
  }
  Y <- as.matrix(Y) # coerces Y to be a matrix.
  X <- as.matrix(X) # coerces X to be a matrix.
  w <- if(is.null(weights)) 1 else sqrt(weights)
  nc <- dim(Y)[2]
  nr <- nrow(Y)
  xqr <- qr(w * X)
  llik <- function(lambda){
    (nr/2)*log(((nr - 1)/nr) *
                 det(var(qr.resid(xqr, w*fam(Y, lambda, j=TRUE, ...)))))
  }
  llik1d <- function(lambda,Y){
    (nr/2)*log(((nr - 1)/nr) * var(qr.resid(xqr, w*fam(Y, lambda, j=TRUE, ...))))
  }
  if (is.null(start)) {
    start <- rep(1, nc)
    for (j in 1:nc){
      res<- suppressWarnings(optimize(
        f = function(lambda) llik1d(lambda,Y[ , j, drop=FALSE]),
        lower=-3, upper=+3))
      start[j] <- res$minimum
    }
  }
  res <- optim(start, llik, hessian=TRUE, method=method,  ...)
  if(res$convergence != 0)
    warning(paste("Convergence failure: return code =", res$convergence))
  res$start<-start
  res$lambda <- res$par
  names(res$lambda) <-
    if (is.null(colnames(Y))) paste("Y", 1:dim(Y)[2], sep="")
  else colnames(Y)
  roundlam <- res$lambda
  stderr <- sqrt(diag(solve(res$hessian)))
  lamL <- roundlam - 1.96 * stderr
  lamU <- roundlam + 1.96 * stderr
  for (val in rev(c(1, 0, -1, .5, .33, -.5, -.33, 2, -2))) {
    sel <- lamL <= val & val <= lamU
    roundlam[sel] <- val
  }
  res$roundlam <- roundlam
  res$invHess <- solve(res$hessian)
  res$llik <- res$value
  res$par <- NULL
  res$family<-family
  res$xqr <- xqr
  res$y <- Y
  res$x <- as.matrix(X)
  res$weights <- weights
  res$family<-family
  class(res) <- "powerTransform"
  res
}

print.powerTransform <<- function(x, ...) {
   lambda <- x$lambda
   if (length(lambda) > 1) cat("Estimated transformation parameters \n")
   else cat("Estimated transformation parameter \n")
   print(x$lambda)
   invisible(x)}

summary.powerTransform <<- function(object,...){
    one <- 1==length(object$lambda)
    label <- paste(object$family,
       (if(one) "Transformation to Normality" else
                "Transformations to Multinormality"), "\n")
    lambda<-object$lambda
    roundlam <- round(object$roundlam, 2)
    stderr<-sqrt(diag(object$invHess))
    df<-length(lambda)
#    result <- cbind(lambda, roundlam, stderr, lambda - 1.96*stderr, lambda + 1.96*stderr)
    result <- cbind(lambda, roundlam, lambda - 1.96*stderr, lambda + 1.96*stderr)
    rownames(result)<-names(object$lambda)
#    colnames(result)<-c("Est Power", "Rnd Pwr", "Std Err", "Lwr bnd", "Upr Bnd")
    colnames(result)<-c("Est Power", "Rounded Pwr", "Wald Lwr Bnd", "Wald Upr Bnd")
    tests <- testTransform(object, 0)
    tests <- rbind(tests, testTransform(object, 1))
#    if ( !(all(object$roundlam==0) | all(object$roundlam==1) |
#        length(object$roundlam)==1 ))
#           tests <- rbind(tests, testTransform(object, object$roundlam))
    family<-object$family
    out <-  list(label=label, result=result, tests=tests,family=family)
    class(out) <- "summary.powerTransform"
    out
    }

print.summary.powerTransform <<- function(x, digits=4, ...) {
    n.trans <- nrow(x$result)
    cat(x$label)
    print(round(x$result, digits))
   if(!is.null(x$family)){
    if(x$family=="bcPower" || x$family=="bcnPower"){
      if (n.trans > 1) cat("\nLikelihood ratio test that transformation parameters are equal to 0\n (all log transformations)\n")
      else cat("\nLikelihood ratio test that transformation parameter is equal to 0\n (log transformation)\n")
      print(x$tests[1,])
      if (n.trans > 1) cat("\nLikelihood ratio test that no transformations are needed\n")
      else cat("\nLikelihood ratio test that no transformation is needed\n")
      print(x$tests[2,])
    }
     if(x$family=="yjPower"){
         if (n.trans > 1) cat("\n Likelihood ratio test that all transformation parameters are equal to 0\n")
         else cat("\n Likelihood ratio test that transformation parameter is equal to 0\n")
       print(x$tests[1,])
     }

   }else{
       if (n.trans > 1) cat("\nLikelihood ratio tests about transformation parameters \n")
       else cat("\nLikelihood ratio test about transformation parameter \n")
     print(x$tests)
   }
}

coef.powerTransform <<- function(object, round=FALSE, ...)
  if(round==TRUE) object$roundlam else object$lambda

vcov.powerTransform <<- function(object,...) {
  ans <- object$invHess
  rownames(ans) <- names(coef(object))
  colnames(ans) <- names(coef(object))
  ans}



horn.outliers <<- function (data)
{
#   This function implements Horn's algorithm for outlier detection using
#   Tukey's interquartile fences.

	boxcox = powerTransform(data);
	lambda = boxcox$lambda;
	transData = data^lambda;
    descriptives = summary(transData);
    Q1 = descriptives[[2]];
    Q3 = descriptives[[5]];
    IQR = Q3 - Q1;

	out = transData[transData <= (Q1 - 1.5*IQR) | transData >= (Q3 + 1.5*IQR)];
	sub = transData[transData > (Q1 - 1.5*IQR) & transData < (Q3 + 1.5*IQR)];

    return(list(outliers = out^(1/lambda), subset = sub^(1/lambda)));
}
refLimit <<- function(data, out.method = "horn", out.rm = FALSE, RI = "p", CI = "p",
					refConf = 0.95, limitConf = 0.90, bootStat = "basic"){

	cl = class(data);
	if(cl == "data.frame"){
		frameLabels = colnames(data);
		dname = deparse(substitute(data));
		result = lapply(data, singleRefLimit, dname, out.method, out.rm, RI, CI, refConf, limitConf, bootStat);
		for(i in 1:length(data)){
			result[[i]]$dname = frameLabels[i];
		}
		class(result) = "interval";
	}
	else{
		frameLabels = NULL;
		dname = deparse(substitute(data));
		result = singleRefLimit(data, dname, out.method, out.rm, RI, CI, refConf, limitConf, bootStat);
	}

	return(result);
}
singleRefLimit <<- function(data, dname = "default", out.method = "horn", out.rm = FALSE,
						RI = "p", CI = "p", refConf = 0.95, limitConf = 0.90, bootStat = "basic")
{
#	This function determines a reference interval from a vector of data samples.
#	The default is a parametric calculation, but other options include a non-parametric
#	calculation of reference interval with bootstrapped confidence intervals around the
#	limits, and also the robust algorithm for calculating the reference interval with
#	bootstrapped confidence intervals of the limits.

	if(out.method == "dixon"){
		output = dixon.outliers(data);
	}
	else if(out.method == "cook"){
		output = cook.outliers(data);
	}
	else if(out.method == "vanderLoo"){
		output = vanderLoo.outliers(data);
	}
	else{
		output = horn.outliers(data);
	}
	if(out.rm == TRUE){
		data = output$subset;
	}

  if(!bootStat %in% c("basic", "norm", "perc", "stud", "bca")) {
		bootStat = "basic";
	}

	outliers = output$outliers;
  n = length(data);
  mean = mean(data, na.rm = TRUE);
  sd = sd(data, na.rm = TRUE);
  norm = NULL;

#	Calculate a nonparametric reference interval.
    if(RI == "n"){

    	methodRI = "Reference Interval calculated nonparametrically";

        data = sort(data);
		holder = nonparRI(data, indices = 1:length(data), refConf);
        lowerRefLimit = holder[1];
        upperRefLimit = holder[2];
        if(CI == "p"){
        	CI = "n";
        }
    }

#	Calculate a reference interval using the robust algorithm method.
    if(RI == "r"){

    	methodRI = "Reference Interval calculated using Robust algorithm";

        holder = robust(data, 1:length(data), refConf);
        lowerRefLimit = holder[1];
        upperRefLimit = holder[2];
        CI = "boot";
    }

#	Calculate a reference interval parametrically, with parametric confidence interval
#	around the limits.
    if(RI == "p"){

#		http://www.statsdirect.com/help/parametric_methods/reference_range.htm
#		https://en.wikipedia.org/wiki/Reference_range#Confidence_interval_of_limit

		methodRI = "Reference Interval calculated parametrically";
		methodCI = "Confidence Intervals calculated parametrically";

		refZ = qnorm(1 - ((1 - refConf) / 2));
		limitZ = qnorm(1 - ((1 - limitConf) / 2));

		lowerRefLimit = mean - refZ * sd;
		upperRefLimit = mean + refZ * sd;
        se = sqrt(((sd^2)/n) + (((refZ^2)*(sd^2))/(2*n)));
        lowerRefLowLimit = lowerRefLimit - limitZ * se;
        lowerRefUpperLimit = lowerRefLimit + limitZ * se;
        upperRefLowLimit = upperRefLimit - limitZ * se;
        upperRefUpperLimit = upperRefLimit + limitZ * se;

        shap_normalcy = shapiro.test(data);
        shap_output = paste(c("Shapiro-Wilk: W = ", format(shap_normalcy$statistic,
        					digits = 6), ", p-value = ", format(shap_normalcy$p.value,
        					digits = 6)), collapse = "");
        ks_normalcy = suppressWarnings(ks.test(data, "pnorm", m = mean, sd = sd));
        ks_output = paste(c("Kolmorgorov-Smirnov: D = ", format(ks_normalcy$statistic,
        					digits = 6), ", p-value = ", format(ks_normalcy$p.value,
        					digits = 6)), collapse = "");
        if(shap_normalcy$p.value < 0.05 | ks_normalcy$p.value < 0.05){
        	norm = list(shap_output, ks_output);

        }
        else{
        	norm = list(shap_output, ks_output);
        }
    }

#	Calculate confidence interval around limits nonparametrically.
    if(CI == "n"){

    	if(n < 120){
    		cat("\nSample size too small for non-parametric confidence intervals,
    		bootstrapping instead\n");
    		CI = "boot";
    	}
    	else{

    		methodCI = "Confidence Intervals calculated nonparametrically";

    		ranks = nonparRanks[which(nonparRanks$SampleSize == n),];
  		  	lowerRefLowLimit = data[ranks$Lower];
    		lowerRefUpperLimit = data[ranks$Upper];
    		upperRefLowLimit = data[(n+1) - ranks$Upper];
    		upperRefUpperLimit = data[(n+1) - ranks$Lower];
		}
    }

#	Calculate bootstrapped confidence intervals around limits.
	if(CI == "boot" & (RI == "n" | RI == "r")){

		methodCI = "Confidence Intervals calculated by bootstrapping, R = 5000";

		if(RI == "n"){
			bootresult = boot::boot(data = data, statistic = nonparRI, refConf = refConf, R = 5000);
		}
		if(RI == "r"){
			bootresult = boot::boot(data = data, statistic = robust, refConf = refConf, R = 5000);
		}
    	bootresultlower = boot::boot.ci(bootresult, conf = limitConf, type=bootStat, index = c(1,2));
    	bootresultupper = boot::boot.ci(bootresult, conf = limitConf, type=bootStat, index = c(2,2));
			bootresultlength = length(bootresultlower[[4]]);
    	lowerRefLowLimit = bootresultlower[[4]][bootresultlength - 1];
    	lowerRefUpperLimit = bootresultlower[[4]][bootresultlength];
    	upperRefLowLimit = bootresultupper[[4]][bootresultlength - 1];
    	upperRefUpperLimit = bootresultupper[[4]][bootresultlength];
    }

    RVAL = list(size = n, dname = dname, out.method = out.method, out.rm = out.rm,
    			outliers = outliers, methodRI = methodRI, methodCI = methodCI,
    			norm = norm, refConf = refConf, limitConf = limitConf,
    			Ref_Int = c(lowerRefLimit = lowerRefLimit, upperRefLimit = upperRefLimit),
    			Conf_Int = c(lowerRefLowLimit = lowerRefLowLimit,
    						lowerRefUpperLimit = lowerRefUpperLimit,
        					upperRefLowLimit = upperRefLowLimit,
        					upperRefUpperLimit = upperRefUpperLimit));
    class(RVAL) = "interval";
    return(RVAL);
}

RI.mean <<- function(data, gamma = 0.95) {
    refLimit(data, refConf = gamma)$Ref_Int
}
ciloRI.mean <<- function(data, gamma = c(0.95, 0.90) ) {
    refLimit(data, refConf = gamma[1], limitConf = gamma[2] )$Conf_Int[1:2]
}
cihiRI.mean <<- function(data, gamma = c(0.95, 0.90) ) {
    refLimit(data, refConf = gamma[1], limitConf = gamma[2] )$Conf_Int[3:4]
}



## ---- message=FALSE, echo=TRUE------------------------------------------------
glucoselevels <- GRD(BSFactors = "concentration(A,B,C,D,E)", 
                    SubjectsPerGroup = 100,
                    RenameDV = "gl",
                    Effects = list("concentration" = extent(10) ),
                    Population = list(mean = 100, stddev = 20) ) 

## ---- message=FALSE, echo=FALSE-----------------------------------------------
# as the package referenceIntervals cannot accommodate numbers below zero, lets remove them
glucoselevels$gl[glucoselevels$gl<10]<-10

## -----------------------------------------------------------------------------
head(glucoselevels)

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 1**. Mean glucose level as a function of concentration."----
superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "CI",
            gamma     = 0.95,
            plotStyle = "line")

## -----------------------------------------------------------------------------
min(glucoselevels$gl)
max(glucoselevels$gl)

## ---- eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE---------------------
#  RI.mean <- function(data, gamma = 0.95) {
#      refLimit(data, refConf = gamma)$Ref_Int
#  }

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 2**. Mean glucose level and 95% reference intervals as a function of concentration."----
superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", # mean is what RI is attached to
            errorbar  = "RI",   # RI calls the function above
            gamma     = 0.95,   # select the coverage desired
            plotStyle = "line" )

## ---- eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE---------------------
#  ciloRI.mean <- function(data, gamma = c(0.95, 0.90) ) {
#      refLimit(data, refConf = gamma[1], limitConf = gamma[2] )$Conf_Int[1:2]
#  }
#  cihiRI.mean <- function(data, gamma = c(0.95, 0.90) ) {
#      refLimit(data, refConf = gamma[1], limitConf = gamma[2] )$Conf_Int[3:4]
#  }

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 3**. Mean glucose level and 90% confidence intervals of the upper RI tips."----
superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", errorbar = "cihiRI",
            gamma     = c(0.95, 0.90),
            plotStyle = "line" ) 

## -----------------------------------------------------------------------------
ornate = list(
        labs(title =paste("(tick)     95% reference intervals (RI)",
                        "\n(red)      90% confidence intervals of upper 95% RI",
                        "\n(purple) 90% confidence intervals of lower 95% RI",
                        "\n(blue)    95% confidence intervals of the mean")),
        coord_cartesian( ylim = c(000,200) ),
        theme_light(base_size=10) # smaller font
)

## ---- message=FALSE-----------------------------------------------------------
plt1 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "RI",
            gamma     = 0.95,
            errorbarParams = list(width = 0.0, linewidth = 1.5,
                                  position = position_nudge( 0.0) ),
            plotStyle = "line" ) + ornate
plt2 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "cihiRI",
            gamma     = c(0.95, 0.90),
            errorbarParams = list(width = 0.2, linewidth = 0.2, color = "red",
                                  direction = "left",
                                  position = position_nudge(-0.15) ),
            plotStyle = "line" ) + ornate + makeTransparent()
plt3 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "ciloRI",
            gamma     = c(0.95, 0.90),
            errorbarParams = list(width = 0.2, linewidth = 0.2, color = "purple",
                                  direction = "left",
                                  position = position_nudge(-0.15) ),
            plotStyle = "line" ) + ornate + makeTransparent()

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 3a**. Mean glucose level and 95% reference intervals with 95% confidence intervals."----
# transform the three plots into visual objects
plt1 <- ggplotGrob(plt1)
plt2 <- ggplotGrob(plt2)
plt3 <- ggplotGrob(plt3)

# superimpose the grobs onto an empty ggplot 
ggplot() + 
    annotation_custom(grob=plt1) + 
    annotation_custom(grob=plt2) + 
    annotation_custom(grob=plt3)

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 3b**. Jittered dots showing mean glucose level and 95% reference intervals with 95% confidence intervals."----
# redo plt1; the other 2 are still in memory
plt1 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "RI",
            gamma     = 0.95,
            errorbarParams = list(width = 0.0, linewidth = 1.5,
                                  position = position_nudge( 0.0) ),
            plotStyle = "pointjitter" ) + ornate

# transform the new plot into a visual object
plt1 <- ggplotGrob(plt1)

# superimpose the grobs onto an empty ggplot 
ggplot() + 
    annotation_custom(grob=plt1) + 
    annotation_custom(grob=plt2) + 
    annotation_custom(grob=plt3)

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 3c**. Jittered dots and violins showing mean glucose level and 95% reference intervals with 95% confidence intervals of the tips' position."----
# redo plt1; the other 2 are still in memory
plt1 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "RI",
            gamma     = 0.95,
            errorbarParams = list(width = 0.0, linewidth = 1.5,
                                  position = position_nudge( 0.0) ),
            plotStyle = "pointjitterviolin" ) + ornate

# transform the three plots into visual objects
plt1 <- ggplotGrob(plt1)

# you may superimpose the grobs onto an empty ggplot 
#ggplot() + 
#    annotation_custom(grob=plt1) + 
#    annotation_custom(grob=plt2) + 
#    annotation_custom(grob=plt3)

## ---- message=FALSE-----------------------------------------------------------
plt4 <- superbPlot(glucoselevels, 
            BSFactors = "concentration",  
            variables = "gl", 
            statistic = "mean", 
            errorbar  = "CI",  # just the regular CI of the mean
            errorbarParams = list(width = 0.2, linewidth = 1.5, color = "blue",
                                  position = position_nudge( 0.00) ),
            gamma     = 0.95,
            plotStyle = "line" ) + ornate + makeTransparent()

## ---- message=FALSE, echo=TRUE, fig.width = 4, fig.cap="**Figure 3d**. Jittered dots and violins showing mean glucose level +-95% confidence intervals of the mean, and 95% reference intervals with 95% confidence intervals."----
# transform that plot too into visual objects
plt4 <- ggplotGrob(plt4)

# superimpose the grobs onto an empty ggplot 
ggplot() + 
    annotation_custom(grob=plt1) + 
    annotation_custom(grob=plt2) + 
    annotation_custom(grob=plt3) +
    annotation_custom(grob=plt4)

