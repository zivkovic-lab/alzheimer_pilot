import::here(FormulaPanel, .from = "../modules/FormulaPanel.R")
import::here(StatsTable, .from = "../modules/StatsTable.R")
import::here(VolcanoPlotPanel, .from = "../modules/VolcanoPlotPanel.R")
import::here(BoxplotPanel, .from = "../modules/BoxplotPanel.R")

LinearModelPage = R6Class(
    "LinearModelPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "linear-model",
        formulaPanel = NULL,
        statsTable = NULL,
        volcanoPlotPanel = NULL,
        boxplotPanel = NULL,
        
        # initializer
        initialize = function(){
            self$formulaPanel = FormulaPanel$new("formula-panel", self$id)
            self$statsTable = StatsTable$new("lm-table", self$id)
            self$volcanoPlotPanel = VolcanoPlotPanel$new("volcano", self$id)
            self$boxplotPanel = BoxplotPanel$new("boxplot", self$id)
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            
            tagList(
                column(
                    width = 6,
                    # formula builder panel
                    box(
                        width = NULL,
                        self$formulaPanel$ui()
                    ),
                    # stats output panel
                    box(
                        width = NULL,
                        self$statsTable$ui()
                    )
                ),
                column(
                    width = 6,
                    # volcano plot
                    box(
                        width = NULL,
                        self$volcanoPlotPanel$ui()
                    ),
                    # boxplot
                    box(
                        width = NULL,
                        self$boxplotPanel$ui()
                    )
                )
            )
        },
        
        # server
        #' @props dataset, character
        server = function(input, output, session, props){
            states = reactiveValues(
                statsResult = NULL,
                selected = NULL
            )
            
            designData = self$formulaPanel$call(props = reactiveValues(
                dataset = props$dataset
            ))
            
            observeEvent({
                props$dataset
                designData$submitted
            }, {
                if(!is.null(designData$design) & !is.null(designData$coef)){
                    states$statsResult = self$fit(
                        data[[props$dataset]],
                        designData$design,
                        designData$transform,
                        designData$coef
                    )
                    selectedData = self$statsTable$call(props = reactiveValues(
                        table = states$statsResult,
                        sortby = 4
                    ))
                    observeEvent(selectedData$selected, {
                        states$selected = selectedData$selected
                    })
                }
            })

            observeEvent({
                states$statsResult
                states$selected
            }, {
                if(!is.null(states$selected)) {
                    # call volcanoplot
                    self$volcanoPlotPanel$call(props = reactiveValues(
                        data = states$statsResult,
                        selected = states$selected
                    ))

                    # call boxplot
                    self$boxplotPanel$call(props = reactiveValues(
                        data = data[[props$dataset]],
                        feature = states$selected
                    ))
                }
            })
        },
        
        #' @param object mSet
        #' @param design design matrix
        #' @param coef character
        fit = function(object, design, transform, coef){
            transform = switch(
                transform,
                'none' = I,
                'log' = log,
                'log(x+1)' = function(x){ log(x+1) },
                'square' = function(x){ x^2 },
                'cube' = function(x){ x^3 },
                'square root' = sqrt
            )
            mset = subset_samples(object, rownames(design))
            mSet_limma(mset, design, transform = transform, coef = coef)
        }
    )
)