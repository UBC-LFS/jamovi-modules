
# This file is a generated template, your changes will not be overwritten

boxBehnkenClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "boxBehnkenClass",
    inherit = boxBehnkenBase,
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
                            <code>bbd.design(nfactors, ncenter=4, factor.names = NULL, default.levels=c(-1,1), randomize=TRUE, seed=NULL)</code>
                        </div>

                        <div>
                            R package: <a href="https://cran.r-project.org/web/packages/DoE.wrapper/DoE.wrapper.pdf" target="_blank">DoE.wrapper</a><br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Example</h5>
                            <ul>
                                <li>Number of center points: 3</li>
                                <li>Number of factors: 3</li>
                                <li>Name of factors: A, B, C</li>
                                <li>Default Levels: -1, 1</li>
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            
            nCenter <- as.double(self$options$numberCenter)
            nFactors <- as.double(self$options$numberFactors)

            # To check whether intpus are valid or not
            if (nCenter < 1 || nFactors < 1 || nchar(self$options$nameFactors) == 0 || nchar(self$options$defaultLevels) == 0)
                return()

            nameFactors <- strsplit(self$options$nameFactors, ',')
            if (length(nameFactors[[1]]) < 1)
                return()

            defaultLevels <- strsplit(self$options$defaultLevels, ',')
            if (length(defaultLevels[[1]]) < 1)
                return()

            randomize <- self$options$randomize
            seed <- as.double(self$options$seed)

            factorNames <- vector()
            for (i in 1:length(nameFactors[[1]])) {
                factorNames <- ( c(factorNames, trimws(nameFactors[[1]][i])) )
            }

            design <- bbd.design(
                nfactors = nFactors,
                factor.names = factorNames, 
                default.levels = as.double( c(defaultLevels[[1]][1], defaultLevels[[1]][2]) ),
                nCenter <- nCenter,
                block.name = NULL,
                randomize <- randomize,
                seed <- seed
            )
            
            order <- run.order(design)
            df <- data.frame(subset(order, select=c(run.no.in.std.order, run.no)), design)
            self$results$text$setContent( df )
        }
    )
)
