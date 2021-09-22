
# This file is a generated template, your changes will not be overwritten

simplexLatticeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "simplexLatticeClass",
    inherit = simplexLatticeBase,
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
                            <code>SLD(fac, lev)</code>
                        </div>

                        <div>
                            R package: <a href="https://cran.r-project.org/web/packages/mixexp/mixexp.pdf" target="_blank">mixexp</a><br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Arguments</h5>
                            <ul>
                                <li>fac: This is the number of factors, this must be between 2 and 12</li>
                                <li>lev: This is the number of levels, this must be between 2, and 5</li>
                            </ul>
                            <h5>Example</h5>
                            <ul>
                                <li>Number of factors: 3</li>
                                <li>Number of levels: 3</li>
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            fac <- as.double(self$options$fac)
            lev <- as.double(self$options$lev)
            
            if (fac < 1 || lev < 1) {
                return()
            }

            if (fac < 2 || fac > 12) {
                jmvcore::reject("The number of factors must be between 2 and 12")
            }

            if (lev < 2 || lev > 5) {
                jmvcore::reject("The number of levels must be between 2 and 5")
            }

            design <- SLD(fac, lev)
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
