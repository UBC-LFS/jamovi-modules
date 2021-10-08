
# This file is a generated template, your changes will not be overwritten

linModelClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "linModelClass",
    inherit = linModelBase,
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
                            <code>lm(formula, data)</code> <br />
                            <code>Anova(model, type)</code>
                        </div>

                        <div>
                            R package: 
                            <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm" target="_blank">stats::lm</a>
                            <a href="https://cran.r-project.org/web/packages/car/car.pdf" target="_blank">car</a>
                            <br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Example</h5>
                            <div>C is an independent variable and A and B are dependent variables.</div>
                            <ul>
                                <li>Main Effects Formula: C ~ A</li>
                                <li>
                                    Interaction Plot:
                                    <ul>
                                        <li>Factor X: A</li>
                                        <li>Trace Factor: B</li>
                                        <li>Reponse: C</li>
                                    </ul>
                                </li>
                                <li>Contour Formula: ~ A + B (or B ~ A)</li>
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
            if (is.null(self$options$indeps) || is.null(self$options$dep) || base::identical(self$options$modelTerms, character()) || nrow(self$data) < 1)
                return()
            
            data <- self$data
            
            anovaType <- self$options$anovaType
            paretoSwitch <- self$options$paretoSwitch

            data[[self$options$dep]] <- jmvcore::toNumeric(data[[self$options$dep]])
            formula <- private$.formula()
            formula <- as.formula(formula)

            model <- lm(formula = formula, data = data)
            self$results$linearReg$setContent( summary(model) )

            anova <- Anova(model, type = anovaType)
            self$results$anova$setContent(anova)

            if (!is.null(self$options$norVar) && nchar(self$options$norVar) > 0) {
                norVar <- self$options$norVar
                self$results$normalPlot$setState(data[norVar][[1]])
            }

            if (!is.null(self$options$mainEffectsFormula) && nchar(self$options$mainEffectsFormula) > 0) {
                mainEffectsFormula <- as.formula(self$options$mainEffectsFormula)
                mainEffectsData <- list(data = data, mainEffectsFormula = mainEffectsFormula)
                self$results$mainEffectsPlot$setState(mainEffectsData)
            }

            if (!is.null(self$options$interactionFactorX) && nchar(self$options$interactionFactorX) > 0 
            && !is.null(self$options$interactionTraceFactor) && nchar(self$options$interactionTraceFactor) > 0 
            && !is.null(self$options$interactionFactorY) && nchar(self$options$interactionFactorY) > 0) {
                interactionFactorX <- self$options$interactionFactorX
                interactionTraceFactor <- self$options$interactionTraceFactor
                interactionFactorY <- self$options$interactionFactorY

                interactionData <- list(
                    X = data[interactionFactorX][[1]], 
                    traceFactor = data[interactionTraceFactor][[1]], 
                    response = data[interactionFactorY][[1]],
                    xLabel = interactionFactorX,
                    traceLabel = interactionTraceFactor,
                    yLabel = paste("mean of", interactionFactorY)
                )
                self$results$interactionPlot$setState(interactionData)
            }

            if (paretoSwitch == TRUE) {
                self$results$paretoPlot$setState(model)
            }

            if (!is.null(self$options$contourFormula) && nchar(self$options$contourFormula) > 0) {
                contourFormula <- as.formula(self$options$contourFormula)
                contourData <- list(model = model, contourFormula = contourFormula)
                self$results$contourPlot$setState(contourData)
            }
        },
        .formula=function() {
            terms <- self$options$modelTerms
            if (is.null(terms))
                terms <- private$.ff()

            terms <- jmvcore::composeTerms(terms)
            rhs <- paste0(terms, collapse=' + ')
            lhs <- jmvcore::composeTerm(self$options$dep)
            formula <- paste0(lhs, ' ~ ', rhs)
            formula
        },
        .ff=function() {
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
        },
        .normalPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- qqnorm(image$state)
            qqline(image$state)
            print(plot)
            TRUE
        },
        .mainEffectsPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- plotmeans(formula = image$state$mainEffectsFormula, data = image$state$data)
            print(plot)
            TRUE
        },
        .interactionPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- interaction.plot(
                image$state$X, 
                image$state$traceFactor,
                image$state$response, 
                fun = mean,
                xlab = image$state$xLabel,
                trace.label = image$state$traceLabel,
                ylab = image$state$yLabel
            )
            print(plot)
            TRUE
        },
        .paretoPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- paretoPlot(image$state) 
            print(plot)
            TRUE
        },
        .contourPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- contour(image$state$model, image$state$contourFormula, image = TRUE) 
            print(plot)
            TRUE
        }
    )
)
