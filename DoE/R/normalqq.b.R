
# This file is a generated template, your changes will not be overwritten

normalQQClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "normalQQClass",
    inherit = normalQQBase,
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
                            <div>Please enter any variable in the textbox in the settings.</div>
                        </div>
                    </div>
                </body>
            </html>')
        },
        .run = function() {
            if (nrow(self$data) < 1) 
                return()

            data <- self$data

            if (length(self$options$norVar) > 0) {
                norVar <- self$options$norVar
                self$results$normalQQPlot$setState(data[norVar][[1]])
            }

        },
        .normalQQPlot = function(image, ...) {
            if (is.null(image$state))
                return(FALSE)

            plot <- qqnorm(image$state)
            qqline(image$state)
            print(plot)
            TRUE
        }
    )
)
