
# This file is a generated template, your changes will not be overwritten

taguchiClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "taguchiClass",
    inherit = taguchiBase,
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
                            <code>taguchiDesign(design, randomize = TRUE, replicates = 1)</code>
                        </div>

                        <div>
                            R package: <a href="https://mran.microsoft.com/snapshot/2017-02-04/web/packages/qualityTools/qualityTools.pdf" target="_blank">qualityTools</a><br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Example</h5>
                            <ul>
                                <li>Number of factors: 3</li>
                                <li>Name of factors: Rice, Fish, Meat</li>
                                <li>Number of factor levels: 3</li>
                                <li>Each of column index:1, 2, 3</li>
                                <li>Each of factor levels: 200,250,300; 100,125,150; 150,180,195</li>
                                <li>Orthogonal Arrays: L4_2</li>
                            </ul>
                            <h5>Orthogonal Arrays</h5>
                            <div>
                                To see more details of arrays and column indices, click this <a href="https://www.york.ac.uk/depts/maths/tables/taguchi_table.htm" target="_blank">Table of Taguchi Designs</a> at The University of York in the U.K.
                            </div>
                            <ul>
                                <li><strong>L4_2</strong> for three two-level factors</li>
                                <li><strong>L8_2</strong> for seven two-level factors</li>
                                <li><strong>L9_3</strong> for four three-level factors</li>
                                <li><strong>L12_2</strong> for 11 two-level factors</li>
                                <li><strong>L16_2</strong> for 16 two-level factors</li>
                                <li><strong>L16_4</strong> for 16 four-level factors</li>
                                <li><strong>L18_2_3</strong> for one two-level and seven three-level factors</li>
                                <li><strong>L25_5</strong> for six five-level factors</li>
                                <li><strong>L27_3</strong> for 13 three-level factors</li>
                                <li><strong>L32_2</strong> for 32 two-level factors</li>
                                <li><strong>L32_2_4</strong> for one two-level factor and nine four-level factors</li>
                                <li><strong>L36_2_3_a</strong> for 11 two-level factors and 12 three-level factors</li>
                                <li><strong>L36_2_3_b</strong> for three two-level factors and 13 three-level factors</li>
                                <li><strong>L50_2_5</strong> for one two-level factor and eleven five-level factors</li>
                                <li><strong>L8_4_2</strong> for one four-level factor and four two-level factors</li>
                                <li><strong>L16_4_2_a</strong> for one four-level factor and 12 two-level factors</li>
                                <li><strong>L16_4_2_b</strong> for two four-level factors and nine two-level factors</li>
                                <li><strong>L16_4_2_c</strong> for three four-level factors and six two-level factors</li>
                                <li><strong>L16_4_2_d</strong> for five four-level factors and two two-level factors</li>
                                <li><strong>L18_6_3</strong> for one six-level factors and six three-level factors</li>
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            
            if (is.empty(self$options$numberFactors) == TRUE || is.empty(self$options$nameFactors) == TRUE || is.empty(self$options$numberFactorLevels) == TRUE || is.empty(self$options$eachColumnIndex) == TRUE || is.empty(self$options$eachFactorLevels) == TRUE)
                return()

            nFactors <- self$options$numberFactors
            if (nFactors < 1)
                return()

            nameFactors <- strsplit(self$options$nameFactors, ',')[[1]]
            if (length(nameFactors) != nFactors)
                jmvcore::reject("The number of factor names is not equal to the number of factors")
            
            numberFactorLevels <- self$options$numberFactorLevels

            eachColumnIndex <- self$options$eachColumnIndex

            eachFactorLevels <- strsplit(self$options$eachFactorLevels, ';')[[1]]
            if (length(eachFactorLevels) != nFactors)
                jmvcore::reject("The number of each of factor levels is not equal to the number of factors")

            design <- self$options$design
            replicates <- self$options$replicates
            randomize <- self$options$randomize

            tdo <- taguchiDesign(
                design, 
                randomize = randomize, 
                replicates = as.double(replicates)
            )

            df <- data.frame(tdo)
            columnIndices <- strsplit(eachColumnIndex, ',')[[1]]
            maxNumFactors <- length(df) - 4

            for (i in 1:length(columnIndices[[1]])) {
                if (as.integer(columnIndices[i]) > maxNumFactors) {
                    print( paste("Column Index cannot be greater than", maxNumFactors) )
                }
            }

            nRows = 0
            nCols = nFactors

            items <- vector()
            for (i in 1:length(eachFactorLevels)) {
                eachLevels <- strsplit(eachFactorLevels[i], ',')
                if (length(eachLevels[[1]]) != numberFactorLevels) {
                    print("The number of each of factor levels is not equal to the number of factor levels")
                }
                nRows <- length(eachLevels[[1]])
                for (j in 1:length(eachLevels[[1]])) {
                    items <- c( items, as.character( trimws(eachLevels[[1]][j]) ) )
                }
            }

            array <- array(items, dim=c(nRows,nCols))
            columnIndices <- as.integer(columnIndices)
            sortedcolumnIndices <- sort(columnIndices)

            indexNames <- list()
            for (i in 1:length(nameFactors)) {
                indexNames[[ as.character(columnIndices[i]) ]] <- trimws(nameFactors[i])
            }

            newList <- list()
            for (i in 1:length(sortedcolumnIndices)) {
                idx <- sortedcolumnIndices[i]
                val <- indexNames[[as.character(idx)]]
                lo <- match(columnIndices[i], sortedcolumnIndices)

                temp <- vector()
                for (j in 1:length(df[[idx + 3]])) {
                    temp <- c(temp, array[ df[[idx + 3]][j], lo ])
                }
                newList[[ val ]] <- temp
            }

            result <- data.frame(select(df, c("StandOrder", "RunOrder", "Replicate")), newList)
            self$results$text$setContent(result)
        }
    )
)
