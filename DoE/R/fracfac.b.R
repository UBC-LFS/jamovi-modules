
# This file is a generated template, your changes will not be overwritten

fracFacClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "fracFacClass",
    inherit = fracFacBase,
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
                            <code>FrF2(nruns = NULL, nfactors = NULL, factor.names = NULL, default.levels = c(-1, 1), ncenter = 0, blocks = 1, block.name = "Blocks", alias.block.2fis = FALSE,  alias.info = 2, replications = 1, repeat.only = FALSE, randomize = TRUE, seed = NULL, resolution = NULL)</code>
                        </div>

                        <div>
                            R package: <a href="https://cran.r-project.org/web/packages/FrF2/FrF2.pdf" target="_blank">FrF2</a><br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Example</h5>
                            <ul>
                                <li>Number of runs: 8</li>
                                <li>Number of factors: 7</li>
                                <li>Name of factors: A, B, C, D, E, F, G</li>
                                <li>Default Levels: -1, 1</li>
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {

            nRuns <- self$options$numberRuns
            nFactors <- self$options$numberFactors

            # To check whether intpus are valid or not
            if (nRuns < 1 || nFactors < 1 || nchar(self$options$nameFactors) == 0 || nchar(self$options$defaultLevels) == 0)
                return()

            nameFactors <- strsplit(self$options$nameFactors, ',')
            if (length(nameFactors[[1]]) < 1)
                return()

            defaultLevels <- strsplit(self$options$defaultLevels, ',')
            if (length(defaultLevels[[1]]) < 1)
                return()

            nCenter <- self$options$numberCenter
            resolution <- self$options$resolution
            nBlocks <- self$options$numberBlocks
            nameBlocks <- self$options$nameBlocks
            replications <- self$options$replications
            repeatOnly <- self$options$repeatOnly
            randomize <- self$options$randomize
            seed <- self$options$seed
            aliased2fis <- self$options$aliased2fis
            aliasInfo <- self$options$aliasInfo

            factorNames <- list()
            for (i in 1:nFactors) {
                factorNames[[i]] <- as.double( c(defaultLevels[[1]][1], defaultLevels[[1]][2]) )
            }

            design <- FrF2(
                nruns = as.double(nRuns),
                nfactors = as.double(nFactors),
                factor.names = factorNames, 
                ncenter = as.double(nCenter),
                resolution = as.double(resolution),
                blocks = as.double(nBlocks),
                block.name = nameBlocks,
                replications = as.double(replications),
                repeat.only = repeatOnly,
                seed = as.double(seed),
                randomize = randomize,
                alias.block.2fis = aliased2fis,
                alias.info = as.double(aliasInfo)
            )
            
            names <- vector()
            for (i in 1:nFactors) {
                names <- c( names, trimws(nameFactors[[1]][i]) )
            }
            
            names(design) <- names
            order <- run.order(design)
            df <- data.frame(subset(order, select=c(run.no.in.std.order, run.no)), design)
            self$results$text$setContent( df )
        }
    )
)
