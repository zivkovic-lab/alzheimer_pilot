StatsTable = R6Class(
    "StatsTable",
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
            ns = NS(NS(self$parent_id)(self$id))
            
            DTOutput(ns("table"))
        },
        
        # server
        #' @props table: data.frame with statistics result
        #' @props sortby: numeric
        server = function(input, output, session, props){
            emit = reactiveValues(
                selected = NULL
            )
            
            output$table = renderDT({
                datatable(
                    props$table,
                    selection = list(mode = "single", selected = 1),
                    options = list(
                        order = list(props$sortby, "asc")
                    )
                ) %>%
                    formatSignif(columns = seq_len(ncol(props$table)), digits = 4)
            })
            
            observeEvent(input$table_rows_selected, {
                emit$selected = rownames(props$table)[input$table_rows_selected]
            })
            
            return(emit)
        },
        
        # call
        call = function(input, ouput, session, props){
            callModule(self$server, self$id, props)
        }
    )
)