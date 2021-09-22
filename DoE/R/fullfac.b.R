
# This file is a generated template, your changes will not be overwritten

fullFacClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "fullFacClass",
    inherit = fullFacBase,
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
                            <code>fac.design(nfactors = NULL, nlevels = NULL, factor.names = NULL, blocks = 1, block.name = "Blocks", replications = 1, repeat.only = FALSE, seed = NULL, randomize = TRUE)</code>
                        </div>

                        <div>
                            R package: <a href="https://cran.r-project.org/web/packages/DoE.base/DoE.base.pdf" target="_blank">DoE.base</a><br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Example</h5>
                            <ul>
                                <li>Number of factors: 3</li>
                                <li>Name of factors: Brand, Time, Power</li>
                                <li>Number of factor levels: 2, 2, 2</li>
                                <li>
                                    Each of factor levels: -1, 1; 4, 6; 75, 100 <br />
                                    where -1 = Cheap and 1 = Costly in the Brand factor
                                </li>
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {

            nFactors <- self$options$numberFactors

            # To check whether intpus are valid or not
            if (nFactors < 1 || is.empty(self$options$nameFactors) == TRUE || is.empty(self$options$numberFactorLevels) == TRUE || is.empty(self$options$eachFactorLevels) == TRUE)
                return()
            
            nameFactors <- strsplit(self$options$nameFactors, ',')
            if (length(nameFactors[[1]]) != nFactors)
                jmvcore::reject("The number of factor names is not equal to the number of factors")

            numberFactorLevels <- strsplit(self$options$numberFactorLevels, ',')
            if (length(numberFactorLevels[[1]]) != nFactors)
                jmvcore::reject("The number of each factor level is not equal to the number of factors")
            
            eachFactorLevels <- strsplit(self$options$eachFactorLevels, ';')
            if (length(eachFactorLevels[[1]]) != nFactors)
                jmvcore::reject("The number of each of factor levels is not equal to the number of factors")

            nBlocks <- self$options$numberBlocks
            nameBlocks <- self$options$nameBlocks
            replications <- self$options$replications
            repeatOnly <- self$options$repeatOnly
            seed <- self$options$seed
            randomize <- self$options$randomize

            nLevels <- vector()
            for (level in numberFactorLevels) {
                nLevels <- c( nLevels, level )
            }

            factorNames <- list()
            for (i in 1:length(eachFactorLevels[[1]])) {
                eachLevels <- strsplit(eachFactorLevels[[1]][i], ',')
                eachLevelVec <- vector()
                for (lvl in eachLevels) {
                    value <- as.double(lvl)
                    if (is.na(value)) {
                        eachLevelVec <- c( eachLevelVec, trimws(lvl) )
                    } else {
                        eachLevelVec <- c( eachLevelVec, value)
                    }
                }
                factorNames[[i]] <- eachLevelVec
            }

            design <- fac.design(
                nfactors = as.double(nFactors),
                nlevels = as.double(nLevels),
                factor.names = factorNames,
                blocks = as.double(nBlocks),
                block.name = nameBlocks,
                replications = as.double(replications),
                repeat.only = repeatOnly,
                seed = as.double(seed),
                randomize = randomize
            )

            names <- vector()
            if ( length(names(design)) > nFactors ) {
                names <- c(names, nameBlocks)
            }
            
            for (i in 1:nFactors) {
                names <- c( names, trimws(nameFactors[[1]][i]) )
            }
            
            names(design) <- names
            order <- run.order(design)
            df <- data.frame( subset(order, select=c(run.no.in.std.order, run.no)), design )

            self$results$text$setContent(df)
        }
    )
)
