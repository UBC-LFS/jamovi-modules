
# This file is a generated template, your changes will not be overwritten

extremeVerticesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "extremeVerticesClass",
    inherit = extremeVerticesBase,
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
                            <code>Xvert(nfac=2, lc=c(0,0), uc=c(0,0))</code>
                        </div>

                        <div>
                            R package: <a href="https://cran.r-project.org/web/packages/mixexp/mixexp.pdf" target="_blank">mixexp</a><br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Arguments</h5>
                            <ul>
                                <li>nfac: an integer representing the number of mixture variables in the design. Maximum nfac = 12.</li>
                            </ul>
                        </div>
                        <div>
                            <h5>Example</h5>
                            Constraints of factors: [0, 0.5], [0.1, 0.2], [0.2, 0.6], [0.08, 0.15]
                            <ul>
                                <li>Number of factors: 4 (Note: Maximum is 12)</li>
                                <li>Lower constraints: 0, 0.1, 0.2, 0.08</li>
                                <li>Upper constraints: 0.5, 0.2, 0.6, 0.15</li>
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            nFactors <- as.double(self$options$numberFactors)
            
            if (nFactors < 1 || is.empty(self$options$lowerConstraints) == TRUE || is.empty(self$options$upperConstraints) == TRUE)
                return()

            if (nFactors > 12)
                jmvcore::reject("The maximum of factors is 12")
            
            lowerConsts <- strsplit(self$options$lowerConstraints, ',')            
            if (length(lowerConsts[[1]]) != nFactors)
                jmvcore::reject("Lower constraints are not equal to the number of factors")

            upperConsts <- strsplit(self$options$upperConstraints, ',')
            if (length(upperConsts[[1]]) != nFactors)
                jmvcore::reject("Upper constraints are not equal to the number of factors")
            
            lc <- vector()
            for (i in 1:nFactors) {
                lc <-c(lc, lowerConsts[[1]][i])
            }

            uc <- vector()
            for (j in 1:nFactors) {
                uc <- c(uc, upperConsts[[1]][j])
            }

            design <- Xvert(
                nfac = nFactors,
                lc = as.double(lc),
                uc = as.double(uc)
            )

            self$results$text$setContent(design)
        }
    )
)
