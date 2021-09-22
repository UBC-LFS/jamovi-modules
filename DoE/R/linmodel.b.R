
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

            linRegSwitch <- self$options$linRegSwitch
            anovaSwitch <- self$options$anovaSwitch

            paretoSwitch <- self$options$paretoSwitch
            normalSwitch <- self$options$normalSwitch
            contourSwitch <- self$options$contourSwitch
            mainEffectsSwitch <- self$options$mainEffectsSwitch
            interactionSwitch <- self$options$interactionSwitch

            model <- lm(formula = formula, data = data)

            if (linRegSwitch == TRUE) {
                self$results$linearReg$setContent( summary(model) )
            }
            
            if (anovaSwitch == TRUE) {
                anova <- Anova(model, type = anovaType)
                self$results$anova$setContent(anova)
            }

            if (paretoSwitch == TRUE) {
                self$results$paretoPlot$setState(model)
            }

            if (normalSwitch == TRUE) {
                if (is.null(self$options$alpha) || nchar(self$options$alpha) == 0)
                    jmvcore::reject("Please enter the significance level in the Normal Plot section")

                alpha <- as.double(self$options$alpha)
                isHalfNormal <- self$options$isHalfNormal
                normalData <- list(model = model, alpha = alpha, isHalfNormal = isHalfNormal)
                self$results$normalPlot$setState(normalData)
            }

            if (mainEffectsSwitch == TRUE) {
                self$results$mainEffectsPlot$setState(model)
            }
            if (interactionSwitch == TRUE) {
                self$results$interactionPlot$setState(model)
            }

            if (is.null(self$options$contourFormula) || nchar(self$options$contourFormula) == 0)
                return()

            if (contourSwitch == TRUE) {
                contourFormula <- as.formula(self$options$contourFormula)
                contourData <- list(model = model, contourFormula = contourFormula)
                self$results$contourPlot$setState(contourData)
            }
        },
        .paretoPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- paretoPlot(image$state) 
            print(plot)
            TRUE
        },
        .normalPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- DanielPlot(
                image$state$model, 
                alpha = image$state$alpha, 
                half = image$state$isHalfNormal,
                code = TRUE, 
                autolab = FALSE
            )

            print(plot)
            TRUE
        },
        .mainEffectsPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- MEPlot(image$state)
            print(plot)
            TRUE
        },
        .interactionPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- IAPlot(image$state, show.alias=TRUE)
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
