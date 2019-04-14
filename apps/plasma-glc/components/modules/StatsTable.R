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
            ns = NS(NS(self$id)(self$parent_id))
            
            DTOutput("table")
        },
        
        # server
        #' @props table: data.frame with statistics result
        server = function(input, output, session, props){
            observeEvent(props$table, {
                output$table = renderDT({
                    datatable(props$table)
                })
            })
            
        },
        
        # call
        call = function(input, ouput, session, props){
            callModule(self$server, self$id, props)
        }
    )
)