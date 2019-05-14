import::here(StatsTable, .from = "../modules/StatsTable.R")
import::here(VolcanoPlotPanel, .from = "../modules/VolcanoPlotPanel.R")
import::here(BoxplotPanel, .from = "../modules/BoxplotPanel.R")


E3E4SubsetPage = R6Class(
    "E3E4SubsetPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = 'e3e4-subset',
        data = NULL,
        statsTable = NULL,
        volcanoPlotPanel = NULL,
        
        # initializer
        initialize = function(){
            self$data = lapply(data, function(subset){
                subset = subset_samples(
                    subset, subset$sample_table$apoE %in% c('3-3', '3-4')
                )
                subset$sample_table$apoeGroup = interaction(
                    subset$sample_table$apoE, 
                    subset$sample_table$`Clinical Group`
                )
                return(subset)
            })
            self$statsTable = StatsTable$new("lm-table", self$id)
            self$volcanoPlotPanel = VolcanoPlotPanel$new("volcano", self$id)
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
                            class = 'col-sm-4',
                            selectInput(
                                ns('ref'), 'Reference', 
                                choices = c('3-3.normal', '3-3.AD', '3-4.AD'),
                                selected = '3-3.normal'
                            )
                        ),
                        tags$div(
                            class = 'col-sm-4',
                            selectInput(
                                ns('case'), 'Case',
                                choices = c('3-3.normal', '3-3.AD', '3-4.AD'),
                                selected = '3-3.AD'
                            )
                        ),
                        tags$div(
                            class = 'col-sm-4',
                            selectInput(
                                ns('transform'), 'Transform method',
                                choices = transform_choices,
                                selected = transform_choices[2]
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
                    # volcano plot
                    box(
                        width = NULL,
                        self$volcanoPlotPanel$ui()
                    ),
                    # boxplot
                    box(
                        width = NULL,
                        plotlyOutput(ns('boxplot'))
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session, props){
            states = reactiveValues(
                statsResult = NULL,
                selected = NULL
            )
            observeEvent(
                c(props$dataset, input$ref, input$case, input$transform),
                {
                    states$statsResult = self$fit(
                        data = self$data[[props$dataset]],
                        ref = input$ref, case = input$case,
                        transform = input$transform
                    )
                    selectedData = self$statsTable$call(props = reactiveValues(
                        table = states$statsResult,
                        sortby = 4
                    ))
                    observeEvent(selectedData$selected, {
                        states$selected = selectedData$selected
                    })
                }
            )
            
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
                    
                    #boxplot
                    output$boxplot = renderPlotly({
                        plot_boxplot(
                            self$data[[props$dataset]], 
                            feature = states$selected,
                            x = 'apoeGroup'
                        )
                    })
                }
            })
            
        },
        
        fit = function(data, ref, case, transform){
            data = subset_samples(
                data, data$sample_table$apoeGroup %in% c(ref, case)
            )
            data$sample_table$apoeGroup = factor(
                data$sample_table$apoeGroup, levels = c(ref, case)
            )
            design = model.matrix(
                data = as(data$sample_table, 'data.frame'),
                ~ apoeGroup
            )
            
            transform = switch(
                transform,
                'none' = I,
                'log' = log,
                'log(x+1)' = function(x){ log(x+1) },
                'square' = function(x){ x^2 },
                'cube' = function(x){ x^3 },
                'square root' = sqrt
            )
            
            mSet_limma(data, design, transform = transform, coef = 2)
        }
    )
)