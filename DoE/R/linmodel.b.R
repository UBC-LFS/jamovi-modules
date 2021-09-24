
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
                            <ul>
                                <li>
                                    Linear Regression Formula: C ~ A + B + I(A^2) + I(B^2) + A:B <br />
                                    where C is an independent variable and A and B are dependent variables
                                </li>
                                <li>Significance Level: 0.05</li>
                                <li>Main Effects Formula: C ~ A</li>
                                <li>
                                    Interaction Plot:
                                    <ul>
                                        <li>Factor X: A</li>
                                        <li>Trace Factor: B</li>
                                        <li>Reponse: C</li>
                                    </ul>
                                </li>
                                <li>
                                    Contour Formula: ~ A + B (or B ~ A) <br />
                                    where A and B are dependent variables
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
            if (is.null(self$options$indep) || is.null(self$options$deps) || is.null(self$options$formula) || nchar(self$options$formula) == 0 || nrow(self$data) < 1)
                return()
            
            data <- self$data
            formula <- as.formula(self$options$formula)
            anovaType <- self$options$anovaType

            anovaSwitch <- self$options$anovaSwitch
            paretoSwitch <- self$options$paretoSwitch

            model <- lm(formula = formula, data = data)

            if (nchar(self$options$formula) > 0) {
                self$results$linearReg$setContent( summary(model) )
            }
            
            if (anovaSwitch == TRUE) {
                anova <- Anova(model, type = anovaType)
                self$results$anova$setContent(anova)
            }

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
