
# This file is a generated template, your changes will not be overwritten

linModelClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "linModelClass",
    inherit = linModelBase,
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
                            <code>lm(formula, data)</code> <br />
                            <code>Anova(model, type)</code>
                        </div>

                        <div>
                            R package: 
                            <a href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm" target="_blank">stats::lm</a>
                            <a href="https://cran.r-project.org/web/packages/car/car.pdf" target="_blank">car</a>
                            <br />
                            Please click to see more details of arguments and examples.
                        </div>
                        <br />
                        <div>
                            <h5>Example</h5>
                            <ul>
                                <li>C is a dependent variable and A and B are independent variables, and squared terms would be <strong>AA</strong> and <strong>BB</strong>.</li>
                                <li>Contour Formula: ~ A + B (or B ~ A)</li>
                            </ul>
                            <h5>Note</h5>
                            <ul>
                                <li>If there are NA values in the Linear Regression coefficients, the result would not be accurate</li>                                
                            </ul>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            if (is.null(self$options$dep) || is.null(self$options$indeps) || base::identical(self$options$modelTerms, character()) || nrow(self$data) < 1)
                return()

            data <- self$data
            dep <- self$options$dep
            indeps <- self$options$indeps
            anovaType <- self$options$anovaType

            # Make a formula
            # Reference: https://github.com/jamovi/jmvbaseR/blob/master/R/regression.b.R
            data[[dep]] <- jmvcore::toNumeric(data[[dep]])
            formula <- private$.formula()
            formula <- as.formula(formula)

            model <- lm(formula = formula, data = data)
            self$results$linearReg$setContent( summary(model) )

            anova <- Anova(model, type = anovaType)
            self$results$anova$setContent(anova)

            # Make factor levels
            # Reference: https://github.com/cran/pid/blob/master/R/paretoPlot.R
            coef(model)
            coeff.full <- coef(model)[2:length(coef(model))]
            coeff.full <- na.omit(coeff.full)
            coeff.abs <- unname(abs(coeff.full))
            coeff <- sort(coeff.abs, index.return = TRUE, method = "shell")

            temp <- names(coeff.full)[coeff$ix]
            fnames <- factor(temp, levels = temp, ordered = TRUE)

            self$results$factorLevels$setContent(fnames)

            # Pareto plot
            self$results$paretoPlot$setState(model)

            if (!is.null(self$options$contourFormula) && nchar(self$options$contourFormula) > 0) {
                contourFormula <- as.formula(self$options$contourFormula)
                contourData <- list(model = model, contourFormula = contourFormula)
                self$results$contourPlot$setState(contourData)
            }
        },
        .formula = function() {
            terms <- self$options$modelTerms
            if (is.null(terms))
                terms <- private$.ff()

            terms <- jmvcore::composeTerms(terms)
            rhs <- paste0(terms, collapse=' + ')
            lhs <- jmvcore::composeTerm(self$options$dep)
            formula <- paste0(lhs, ' ~ ', rhs)
            formula
        },
        .ff = function() {
            fixedFactors <- self$options$indeps

            if (length(fixedFactors) > 1) {
                formula <- as.formula(paste('~', paste(paste0('`', fixedFactors, '`'), collapse='*')))
                terms   <- attr(stats::terms(formula), 'term.labels')
                modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
            } else {
                modelTerms <- as.list(fixedFactors)
            }

            for (i in seq_along(modelTerms)) {
                term <- modelTerms[[i]]
                quoted <- grepl('^`.*`$', term)
                term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
                modelTerms[[i]] <- term
            }

            modelTerms
        },
        .paretoPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- paretoPlot(image$state) 
            print(plot)
            TRUE
        },
        .contourPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- contour(image$state$model, image$state$contourFormula, image = TRUE) 
            print(plot)
            TRUE
        }
    )
)
