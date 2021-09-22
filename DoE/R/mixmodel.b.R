
# This file is a generated template, your changes will not be overwritten

mixModelClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mixModelClass",
    inherit = mixModelBase,
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
                            <code>MixModel(data.frame, response, mixcomps, model.type)</code>
                        </div>

                        <div>
                            R package: 
                            <a href="https://cran.r-project.org/web/packages/mixexp/mixexp.pdf" target="_blank">mixexp</a><br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Details of plots</h5>
                            <ul>
                                <li><strong>Model contour plot:</strong> This function makes contour plots of a user-supplied model in the simplex mixture space.</li>
                                <li>
                                    <strong>Effect plot:</strong> This function makes effect plots using the Cox or Piepel directions in constrained mixture space.
                                    <div>ModelEff(ufunc, nfac, mod, nproc, dir, dimensions)</div>
                                    <ul>
                                        <li>ufunc: A user function, this should an lm object created by the MixModel function</li>
                                        <li>mod: An integer representing the model to be traced</li>
                                        <li>nfac: The number of mixture components in the model (a number between 2 and 12)</li>
                                        <li>dir: an integer representing the direction for which the effect plot is made: 1 for Piepel direction, 2 for Cox direction</li>
                                        <li>nproc: The number of process variables in the model (a number between 1 and 3 for models 5 and 6)</li>
                                    </ul>
                                </li>
                                <li><strong>Mixture contour plot:</strong> This function makes contour plots in the simplex mixture space, it also can draw constraint lines and show design points.</li>
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            if (is.null(self$options$indep) || is.null(self$options$deps) || nrow(self$data) < 1)
                return()

            data <- self$data
            indep <- self$options$indep
            deps <- self$options$deps
            mixModelType <- as.double(self$options$mixModelType)

            modelContourPlotSwitch <- self$options$modelContourPlotSwitch
            effectPlotSwitch <- self$options$effectPlotSwitch
            mixtureContourPlotSwitch <- self$options$mixtureContourPlotSwitch
            
            model <- MixModel(data, indep, deps, mixModelType)
            self$results$mixModel$setContent( summary(model) )

            if (modelContourPlotSwitch == TRUE) {
                modelPlotData <- list(
                    model = model,
                    dimensions = list(x1 = deps[3], x2 = deps[1], x3 = deps[2]), 
                    cornerLabs = c(deps[3], deps[1], deps[2])
                )

                self$results$modelContourPlot$setState(modelPlotData)
            }

            if (effectPlotSwitch == TRUE) {
                effectPlotModelType <- as.double(self$options$effectPlotModelType)
                numFactors <- as.double(self$options$numFactors)
                direction <- as.double(self$options$direction)
                numProcessVar <- as.double(self$options$numProcessVar)

                effectPlotData <- list(
                    model = model,
                    effectPlotModelType = effectPlotModelType,
                    numFactors = numFactors,
                    direction = direction,
                    numProcessVar = numProcessVar
                )

                self$results$effectPlot$setState(effectPlotData)
            }

            if (mixtureContourPlotSwitch == TRUE) {
                if (length(self$data) != 4)
                    jmvcore::reject("One independent variable and three dependent variables are required to display this Mixture Plot")

                mixturePlotModelType <- as.double(self$options$mixturePlotModelType)
                numBreaks <- as.double(self$options$numBreaks)
                tempData <- data
                names(tempData) <- c("x1", "x2", "x3", "y")

                mixturePlotData <- list(
                    x3 = tempData$x3,
                    x2 = tempData$x2,
                    x1 = tempData$x1,
                    y = tempData$y,
                    cornerLabs = list(x3 = deps[3], x2 = deps[2], x1 = deps[1]), 
                    mod = mixturePlotModelType,
                    numBreaks = numBreaks
                )
                #self$results$temp$setContent(mixturePlotData)
                self$results$mixtureContourPlot$setState(mixturePlotData)
            }
        },
        .modelContourPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- ModelPlot(
                model = image$state$model,
                dimensions = image$state$dimensions,
                constraints = FALSE, 
                contour = TRUE, 
                fill = TRUE, 
                cornerlabs = image$state$cornerLabs,
                pseudo = FALSE
            )
            print(plot)
            TRUE
        },
        .effectPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- ModelEff(
                ufunc = image$state$model,
                mod = image$state$effectPlotModelType,
                nfac = image$state$numFactors,
                dir = image$state$direction,
                nproc = image$state$numProcessVar
            )
            print(plot)
            TRUE
        },
        .mixtureContourPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- MixturePlot(
                x = image$state$x3,
                y = image$state$x2,
                z = image$state$x1,
                w = image$state$y,
                corner.labs = image$state$cornerLabs,
                mod = image$state$mod,
                n.breaks = image$state$numBreaks,
                cols = TRUE
            )
            print(plot)
            TRUE
        }
    )
)
