
# This file is a generated template, your changes will not be overwritten

resMeansClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "resMeansClass",
    inherit = resMeansBase,
    private = list(
        .init = function() {
            self$results$usage$setContent(
            '<html>
                <head>
                </head>
                <body>
                    <div class="usage" style="color:black">
                        <div>
                            <h5>Example</h5>
                            <div>Suppose that there are 6 variables such as A, B, C, D, E and F.</div>
                            <ul>
                                <li>A, B and C are independent variables.</li>
                                <li>D, E and F are response variables</li>
                            </ul>
                            <div>Please enter <strong>D, E, F</strong> in the Response Table for Means.</div>
                            <div>Please enter <strong>A</strong> in the Main Effects Plot for Means.</div>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            if ( nrow(self$data) < 1 || is.null(self$options$deps) || is.null(self$options$indeps) )
                return()

            data <- self$data
            indeps <- self$options$indeps

            # Make factor ranks
            if (length(self$options$resVars) > 0) {
                resVars = strsplit(self$options$resVars, ',')[[1]]
                df <- data.frame(data)
                
                numUniqueItems <- length(as.character(unlist(unique(df[1]))))
                
                dataList <- list()
                deltas <- vector()
                temp <- vector()
                for (i in 1:length(indeps)) {
                    by <- as.character(unlist(df[indeps[i]]))
                    y <- as.data.frame(by, stringsAsFactors = FALSE)
                    splited <- split(df, y)
                    uniqueVars <- unique(by)

                    newSplited <- list()
                    for (k in 1:length(uniqueVars)) {
                        newSplited[[ uniqueVars[k] ]] <- splited[[ uniqueVars[k] ]]
                    }
                    
                    temp <- vector()
                    for (j in 1:length(newSplited)) {
                        selected <- select(newSplited[[j]], resVars)
                        merged <- lapply(selected, sum)
                        summed <- colSums(do.call(rbind, merged))
                        result <- summed / (nrow(newSplited[[j]]) * length(resVars))
                        temp <- c(temp, result)
                    }
                    
                    delta <- max(temp) - min(temp)
                    deltas <- c(deltas, delta)
                    dataList[[ indeps[i] ]] <- c(temp, delta)
                }

                meanTable <- data.frame(dataList)
                ranks <- rank(-deltas)

                result <- rbind(meanTable, ranks)
                
                rowNames <- vector()
                for (i in 1:numUniqueItems) {
                    rowNames <- c(rowNames, paste("Level", i))
                }
                rowNames <- c(rowNames, c("Delta", "Rank"))
                row.names(result) <- rowNames

                self$results$resTableMeans$setContent(result)
            }

            # Main Effects Plot for Means
            if (length(self$options$indepPlot) > 0) {
                indep <- self$options$indepPlot
                resVars = strsplit(self$options$resVars, ',')[[1]]
                df <- data.frame(data)

                resCols <- select(df, resVars)
                df$Means <- rowMeans(resCols)
                formula <- jmvcore::constructFormula('Means', indep)

                plotMeansData <- list(formula = as.formula(formula), data = df)
                self$results$mainEffectsPlotMeans$setState(plotMeansData)
            }
        },
        .mainEffectsPlotMeans = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- plotmeans(formula = image$state$formula, data = image$state$data)
            print(plot)
            TRUE
        }
    )
)
