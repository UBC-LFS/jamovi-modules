
# This file is a generated template, your changes will not be overwritten

simplexCentroidClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "simplexCentroidClass",
    inherit = simplexCentroidBase,
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
                            <code>SCD(fac)</code>
                        </div>

                        <div>
                            R package: <a href="https://cran.r-project.org/web/packages/mixexp/mixexp.pdf" target="_blank">mixexp</a><br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Arguments</h5>
                            <ul>
                                <li>fac: This is the number of factors</li>
                            </ul>
                            <h5>Example</h5>
                            <ul>
                                <li>Number of factors: 3</li>
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            fac <- as.double(self$options$fac)

            if (fac < 1)
                return()

            design <- SCD(fac)
            self$results$text$setContent(design)

            if (self$options$designPoints) {
                self$results$designPointsPlot$setState(design)
            }
        },
        .designPointsPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- DesignPoints(image$state)
            print(plot)
            TRUE
        }
    )
)
