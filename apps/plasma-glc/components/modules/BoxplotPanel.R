BoxplotPanel = R6Class(
    "BoxplotPanel",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = NULL,
        parent_id = NULL,
        
        # initializer
        initialize = function(id, parent_id){
            self$id = id
            self$parent_id = parent_id
            
        },
        
        # UI
        ui = function(){
            ns = NS(NS(self$parent_id)(self$id))
            column(
                width =  12,
                tags$div(
                    class = "row",
                    tags$div(
                        class = "col-md-6",
                        self$selectInput(
                            ns("x"), "x variable",
                            colnames(data$peptides$sample_table)
                        )
                    ),
                    tags$div(
                        class = "col-md-6",
                        self$selectInput(
                            ns("col"), "Column facet",
                            colnames(data$peptides$sample_table)
                        )
                    ),
                    tags$div(
                        class = "col-md-6",
                        self$selectInput(
                            ns("row"), "Row facet",
                            colnames(data$peptides$sample_table)
                        )
                    ),
                    tags$div(
                        class = "col-md-6",
                        self$selectInput(
                            ns("color"), "Color Variable",
                            colnames(data$peptides$sample_table)
                        )
                    ),
                    tags$div(
                        class = "col-md-6",
                        checkboxInput(
                            ns("points"), "Show points", value = TRUE
                        )
                    ),
                    tags$div(
                        class = "col-md-6",
                        checkboxInput(
                            ns("hide-na"), "Hide NAs", value = FALSE
                        )
                    )
                ),
                plotlyOutput(ns("plot"))
            )
        },
        
        # server
        server = function(input, output, session, props){
            
            output$plot = renderPlotly({
                if(!is.null(props$feature) & input$x != "null" ){
                    object = props$data
                    if(input$`hide-na`){
                        object = subset_samples(
                            object,
                            !is.na(object@sample_table[, input$x])
                        )
                    }
                    args = list(
                        object = object,
                        x = input$x,
                        feature = props$feature,
                        rows = input$row,
                        cols = input$col,
                        color = input$color
                    )
                    args = args[args != "null"]
                    do.call(plot_boxplot, args)
                }
            })
        },
        
        # methods
        selectInput = function(inputId, label, choices){
            choices_list = list("-- Please select --" = "null")
            for(x in choices){
                choices_list[[x]] = x
            }
            selectInput(inputId, label, choice = choices_list)
        }
    )
)