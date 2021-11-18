
# This file is a generated template, your changes will not be overwritten

centralCompClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "centralCompClass",
    inherit = centralCompBase,
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
                            <code>ccd.design(nfactors=NULL, factor.names=NULL, default.levels=c(-1,1), ncube=NULL, ncenter = 4, alpha = "orthogonal", replications=1, block.name="Block.ccd", blocks=1, randomize=TRUE, seed=NULL)</code>
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
                                <li>Each of default Levels: -1,1; -1,1; -1,1</li>
                            </ul>
                            <p>Note: a face-centered design is created when alpha is equal to 1.</p>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            
            # To check whether intpus are valid or not
            if (self$options$numberCenter < 1 || self$options$numberFactors < 1 || is.null(self$options$nameFactors) || nchar(self$options$nameFactors) == 0 || is.null(self$options$eachDefaultLevels) || nchar(self$options$eachDefaultLevels) == 0)
                return()

            nCenter <- as.double(self$options$numberCenter)
            nFactors <- as.double(self$options$numberFactors)

            nameFactors <- strsplit(self$options$nameFactors, ',')[[1]]
            if (length(nameFactors) < 1)
                return()

            eachDefaultLevels <- strsplit(self$options$eachDefaultLevels, ';')[[1]]
            if (length(eachDefaultLevels) < 1)
                return()

            nCube <- as.double(self$options$numberCube)
            alpha <- self$options$alpha
            nBlocks <- as.double(self$options$numberBlocks)
            nameBlocks <- self$options$nameBlocks
            replications <- as.double(self$options$replications)
            randomize <- self$options$randomize
            seed1 <- as.double(self$options$seed1)
            seed2 <- as.double(self$options$seed2)

            if (nCube == 0) nCube = NULL
            if (alpha == "1") alpha <- as.double(alpha)

            cube <- ceiling(nCenter / 2.0)
            temp <- list()
            order <- list()
            for (i in 1:length(eachDefaultLevels)) {
                levels <- strsplit(eachDefaultLevels[i], ',')[[1]]

                # nBlocks shouble be double in this function
                design <- ccd.design(
                    nfactors = nFactors,
                    default.levels = c(as.double(levels[1]), as.double(levels[2])),
                    ncenter = c(cube, nCenter - cube),
                    ncube = nCube,
                    alpha = alpha,
                    blocks = nBlocks,
                    block.name = nameBlocks,
                    replications = replications,
                    seed = c(seed1, seed2),
                    randomize = randomize
                )
                
                if (i == 1) order <- run.order(design)
                temp[[i]] <- design[, i + 1]
            }

            df <- data.frame(temp)
            names(df) <- trimws(nameFactors)
            result <- cbind(select(order, c(run.no.in.std.order, run.no)), df)
            self$results$text$setContent(result)
        }
    )
)
