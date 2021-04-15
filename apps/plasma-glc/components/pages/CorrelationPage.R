import::here(StatsTable, .from = "../modules/StatsTable.R")
import::here(ScatterPlotPanel, .from = "../modules/ScatterPlotPanel.R")

CorrelationPage = R6Class(
    "CorrelationPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "correlation",
        statsTable = NULL,
        scatterPlot = NULL,
        
        # initializer
        initialize = function(){
            self$statsTable = StatsTable$new("stats", self$id)
            self$scatterPlot = ScatterPlotPanel$new("scatter", self$id)
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            
            tagList(
                column(
                    width = 6,
                    box(
                        width = NULL,
                        tags$div(
                            class = "col-md-6",
                            selectInput(
                                ns("co-var"), "Select a co-variable",
                                choices = c("AGE", "BRAAK"),
                                selected = "AGE"
                            )
                        ),
                        tags$div(
                            class = "col-md-6",
                            selectInput(
                                ns("method"), "Select a correlation method",
                                choices = c("pearson", "spearman"),
                                selected = "pearson"
                            )
                        )
                    ),
                    box(
                        width = NULL,
                        self$statsTable$ui()
                    )
                ),
                column(
                    width = 6,
                    box(
                        width = NULL,
                        self$scatterPlot$ui()   
                    )
                )
            )
        },
        
        # server
        #' @props dataset string
        server = function(input, output, session, props){
            states = reactiveValues(
                selected = NULL
            )
            
            observeEvent({
                input$`co-var`
                input$method
                props$dataset
            }, {
                X = data[[props$dataset]]$sample_table[, input$`co-var`] %>% t
                rownames(X) = input$`co-var`
                Y = data[[props$dataset]]$conc_table
                res = MatCorR::MatCor(X, Y, method = input$method)[[1]]
                
                tableData = self$statsTable$call(props = reactiveValues(
                    table = res,
                    sortby = 3
                ))
                
                observeEvent(tableData$selected, {
                    states$selected = tableData$selected
                    data = data.frame(
                        x = data$peptides$sample_table[,input$`co-var`],
                        y = as.numeric(data[[props$dataset]]$conc_table[states$selected,])
                    )
                    self$scatterPlot$call(props = reactiveValues(
                        data = data,
                        xlab = input$`co-var`,
                        ylab = states$selected
                    ))
                })
            })
        }
    )
)