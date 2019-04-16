FormulaVarSelector = R6Class(
    "FormulaVarSelector",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = NULL,
        parent_id = NULL,
        varChoices = NULL,
        varName = NULL,
        varType = NULL,
        # initializer
        initialize = function(id, parent_id, varChoices, varName, varType){
            self$id = id
            self$parent_id = parent_id
            self$varChoices = varChoices
            self$varName = varName
            self$varType = varType
        },
        
        # UI
        ui = function(){
            ns = NS(NS(self$parent_id)(self$id))
            tags$tr(
                tags$td(
                    selectInput(
                        ns("varName"), label = NULL,
                        choices = self$varChoices,
                        selected = if(!is.null(self$varName)) self$varName
                                    else self$varChoices[1]
                    )
                ),
                tags$td(
                    selectInput(
                        ns("varType"), label = NULL,
                        choices = c("numeric", "factor"),
                        selected = if(!is.null(self$varType)) self$varType
                                    else "factor"
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            return(reactiveValues(
                varName = input$varName,
                varType = input$varType
            ))
        }
    )
)