
# This file is a generated template, your changes will not be overwritten

resOptimizationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "resOptimizationClass",
    inherit = resOptimizationBase,
    private = list(
        .init = function() {
            self$results$usage$setContent(
            '<html>
                <head>
                </head>
                <body>
                    <div class="usage" style="color:black">
                        <h5>R code</h5>
                        <div style="background-color:#f8f9fa; padding:1rem 1.5rem;">
                            <code>optim(par, fn, method = "Nelder-Mead", lower = -Inf, upper = Inf, control = list())</code><br />
                            <code>desirability(response, low, high, target = "max", scale = c(1, 1), importance = 1, constraints)</code>
                        </div>
                        <div>
                            R package: 
                            
                            <a href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html" target="_blank">stats::optim</a>
                            <a href="https://mran.microsoft.com/snapshot/2017-02-04/web/packages/qualityTools/qualityTools.pdf" target="_blank">qualityTools::desirability</a>
                            <a href="https://cran.r-project.org/web/packages/desirability/desirability.pdf" target="_blank">desirability</a>
                            <br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Example</h5>
                            Suppose that there 7 variables (A, B, C, D, E, F and G).
                            <ul>
                                <li>A, B and C are independent variables</li>
                                <li>D, E, F and G are response variables</li>
                            </ul>

                            <div>Please enter the following inputs.</div>
                            <ul>
                                <li>Number of factors: 3</li>
                                <li>Target goals of responses: 1.5, max, min, 5</li>
                                <li>
                                    Lower and upper bounds of resopnses: 1,3; NA,NA; NA,NA; 5,6 <br />
                                    where NA means that floor(minimum), ceiling(maximum)
                                </li>
                            </ul>
                            <h5>Note</h5>
                            <ul>
                                <li>If there are NA values in the Linear Regression coefficients, the result would not be accurate</li>                                
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            if (nrow(self$data) < 1 || is.null(self$options$deps) || is.null(self$options$indeps))
                return()

            data <- self$data
            deps <- self$options$deps
            indeps <- self$options$indeps

            for (i in 1:length(deps)) {
                data[[ deps[i] ]] <- jmvcore::toNumeric(data[[ deps[i] ]])
            }

            if (is.null(self$options$numFactors) || nchar(self$options$numFactors) < 1)
                jmvcore::reject("Please enter a number of factors")

            numFactors <- as.double(self$options$numFactors)
            factors <- indeps[1:numFactors]

            actualValues <- NULL
            if ( !is.null(self$options$actualValues) && nchar(self$options$actualValues) > 0) {
                actualValues <- strsplit(self$options$actualValues, ";")[[1]]
            }

            if (is.null(self$options$targetGoals) || nchar(self$options$targetGoals) < 1)
                jmvcore::reject("Please enter Target goals of responses")
            
            if (is.null(self$options$lowerUpperBounds) || nchar(self$options$lowerUpperBounds) < 1)
                jmvcore::reject("Please enter Lower and upper bounds of responses")
            
            
            targetGoals <- strsplit(self$options$targetGoals, ",")[[1]]
            lowerUpperBounds <- strsplit(self$options$lowerUpperBounds, ";")[[1]]

            # Reference: qualityTools::optimum 
            # https://github.com/cran/qualityTools/blob/master/R/des_e.r
            constraints <- list()
            for (i in 1:length(factors)) {
                constraints[[i]] <- c(min(data[, i]), max(data[, i]))
            }

            start = as.numeric(lapply(constraints, mean))
            formulas <- private$.customFormula()

            fits <- list()
            for (i in 1:length(formulas)) {
                fits[[ deps[i] ]] <- lm(formula = as.formula(formulas[i]), data = data)
            }

            desires <- list()
            desirabilities <- list()
            for (i in 1:length(deps)) {
                lowerUpper <- strsplit(lowerUpperBounds[i], ",")[[1]]
                lower <- lowerUpper[1]
                upper <- lowerUpper[2]
                d <- as.vector( data[, deps[i]] )
                
                min <- min(d)
                max <- max(d)
                if ( check.numeric(targetGoals[i]) ) {
                    if ( check.numeric(lower) == FALSE ) { lower <- min }
                    if ( check.numeric(upper) == FALSE ) { upper <- max }
                    lower <- as.double(lower)
                    upper <- as.double(upper)
                    
                    desires[[ deps[i] ]] <- desirability(d, lower, upper, target=as.double(targetGoals[i]))
                    desirabilities[[ deps[i] ]] <- dTarget(lower, as.double(targetGoals[i]), upper)
                } else {
                    if ( check.numeric(lower) == FALSE ) { lower <- floor(min) }
                    if ( check.numeric(upper) == FALSE ) { upper <- ceiling(max) }
                    lower <- as.double(lower)
                    upper <- as.double(upper)
                    
                    target <- trimws(targetGoals[i])
                    desires[[ deps[i] ]] <- desirability(d, lower, upper, target=target)
                    
                    if (identical(tolower(target), "min")) {
                        desirabilities[[ deps[i] ]] <- dMin(lower, upper)
                    } else if (identical(tolower(target), "max")) {
                        desirabilities[[ deps[i] ]] <- dMax(lower, upper)
                    }
                }
            }
            
            desireFun <- function(low, high, target = "max", scale = c(1, 1), importance = 1) {
                if (is.numeric(target)) {
                    out <- function(y) {
                    d <- rep(0, length(y))
                    d[y >= low & y <= target] = ( (y[y >= low & y <= target] - low) / (target - low) )^scale[1]
                    d[y >= target & y <= high] = ( (y[y >= target & y <= high] - high) / (target - high) )^scale[2]
                    return(d^importance)
                    }
                    return(out)
                }
                if (identical(tolower(target), "min")) {
                    out <- function(y) {
                    
                    d <- rep(0, length(y))
                    d[y > high] = 0
                    d[y < low] = 1
                    d[y >= low & y <= high] = ( (y[y >= low & y <= high] - high) / (low - high) )^scale[1]
                    return(d^importance)
                    }
                    return(out)
                }
                if (identical(tolower(target), "max")) {
                    out <- function(y) {
                        d <- rep(0, length(y))
                        d[y < low] = 0
                        d[y > high] = 1
                        d[y >= low & y <= high] = ( (y[y >= low & y <= high] - low) / (high - low) )^scale[1]
                        return(d^importance)
                    }
                    return(out)
                }
            }

            dHelp <- function(model, dFun) {
                lm1 <- model
                d1 <- dFun
                out <- function(newdata) {
                    return( d1( stats::predict(lm1, newdata = newdata) ) )
                }
                return(out)
            }

            dList <- list()
            importances <- list()
            for (y in deps) {
                obj <- desires[[y]]
                
                importances[[y]] <- obj@importance
                lm.y <- fits[[y]]

                dFun <- desireFun(obj@low, obj@high, obj@target, obj@scale, obj@importance)
                dList[[y]] <- dHelp(lm.y, dFun)
            }

            geomFac <- 1 / sum(unlist(importances))

            dAll <- function(X) {
                newdata <- data.frame(t(X))
                names(newdata) <- factors
                return( prod( unlist(lapply(dList, do.call, list(newdata = newdata))) )^geomFac )
            }

            code2real = function(low, high, codedValue) {
                return( (diff(c(low, high)) / 2) * codedValue + mean(c(low, high)) )
            }

            # Run an optim function with a Nelder-Mead simplex method
            result <- optim(par = start, dAll, method = "Nelder-Mead", control = list(fnscale = -1, maxit = 1000))

            actualResult <- list()
            for (i in 1:length(factors)) {
                if ( is.null(actualValues) ) {
                    actualResult[[i]] <- result$par[i]
                } else {
                    lowHigh <- strsplit(actualValues[i], ",")[[1]]
                    actualResult[[i]] <- code2real(as.double(lowHigh[1]), as.double(lowHigh[2]), result$par[i])
                }
            }

            # Get a global solution
            globalSolution <- list()
            for (i in 1:length(factors)) {
                globalSolution[[ factors[i] ]] <- actualResult[[i]]
            }
            self$results$globalSolution$setContent(globalSolution)

            # Get predicted responses
            newdata <- list()
            for (i in 1:length(result$par)) {
                newdata[[ factors[i] ]] <- result$par[i]
            }

            predictedResponses <- list()
            for (i in 1:length(deps)) {
                predictedResponses[[ deps[i] ]] <- predict( lm(as.formula(formulas[i]), data=data), newdata=data.frame(newdata) )[[1]]
            }
            self$results$predResponses$setContent(predictedResponses)

            # Get individual desirability
            indDes <- list()
            for (i in 1:length(deps)) {
                indDes[[ deps[i] ]] <- predict(desirabilities[[i]], as.double(predictedResponses[i]))
            }
            self$results$individualDes$setContent(indDes)

            # Display composite desirability
            self$results$compositeDes$setContent(result$value)
        },
        .customFormula = function() {
            deps <- self$options$deps
            terms <- self$options$modelTerms

            if (is.null(terms))
                terms <- private$.ff()

            terms <- jmvcore::composeTerms(terms)

            rhs <- paste0(terms, collapse=' + ')
            formulas <- vector()
            for (i in 1:length(deps)) {
                dep <- jmvcore::toNumeric(deps[i])
                lhs <- jmvcore::composeTerm(dep)
                formula <- paste0(lhs, ' ~ ', rhs)
                formulas <- c(formulas, gsub("`", "", formula, fixed=TRUE))
            }

            formulas
        },
        .ff = function() {
            fixedFactors <- self$options$indeps

            if (length(fixedFactors) > 1) {
                formula <- as.formula(paste('~', paste(paste0('`', fixedFactors, '`'), collapse='*')))
                terms   <- attr(stats::terms(formula), 'term.labels')
                modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
            } else {
                modelTerms <- as.list(fixedFactors)
            }

            for (i in seq_along(modelTerms)) {
                term <- modelTerms[[i]]
                quoted <- grepl('^`.*`$', term)
                term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
                modelTerms[[i]] <- term
            }
            modelTerms
        }
    )
)
