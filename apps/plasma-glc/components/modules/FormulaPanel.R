FormulaPanel = R6Class(
    "FormulaPanel",
    inherit = ShinyModule,
    public = list(
        # attributes
        parent_id = NULL,
        
        # initializer
        initialize = function(id, parent_id){
            self$id = id
            self$parent_id = parent_id
        },
        
        # UI
        ui = function(){
            ns = NS(NS(self$id)(self$id))
            tagList(
                fluidRow(
                    # Button to add a variable
                    column(
                        width = 12,
                        actionButton(
                            ns("add-var"),
                            "Add Variable",
                            class = "btn-primary"
                        )
                    ),
                    # Variable selector
                    uiOutput(ns("var-inputs"))
                )
            )
        },
        
        # server
        server = function(input, output, session){
            
            states = reactiveValues(
                varNum = 0
            )
            
            # var-inputs
            output$`var-inputs` = renderUI({
                tags$table(
                    class = "table",
                    tags$tr(
                        tags$th(scope = "col", "Variable"),
                        tags$th(scope = "col", "data type"),
                        tags$th(scope = "col")
                    )
                )
            })
        }
    )
)