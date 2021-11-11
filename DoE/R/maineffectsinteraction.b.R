
# This file is a generated template, your changes will not be overwritten

mainEffectsInteractionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mainEffectsInteractionClass",
    inherit = mainEffectsInteractionBase,
    private = list(
                .init = function() {
            self$results$usage$setContent(
            '<html>
                <head>
                </head>
                <body>
                    <div class="usage" style="color:black">
                        <h5>Example</h5>
                        <div>C is a response variable, and A and B are independent variables.</div>
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
                        </ul>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            if (nrow(self$data) < 1)
                return()

            data <- self$data

            if (length(self$options$mainEffectsFormula) > 0) {
                mainEffectsFormula <- as.formula(self$options$mainEffectsFormula)
                mainEffectsData <- list(data = data, mainEffectsFormula = mainEffectsFormula)
                self$results$mainEffectsPlot$setState(mainEffectsData)
            }

            if (length(self$options$interactionFactorX) > 0 && length(self$options$interactionTraceFactor) > 0 && length(self$options$interactionFactorY) > 0) {
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
        }
    )
)
