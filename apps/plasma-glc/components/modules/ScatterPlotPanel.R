ScatterPlotPanel = R6Class(
    "ScatterPlotPanel",
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
            
            tagList(
                plotlyOutput(ns("plot"))
            )
        },
        
        # server
        server = function(input, output, session, props){
            output$plot = renderPlotly({
                ggmetaplots::ggscatterplot(
                    props$data, x = "x", y = "y", point.size = 2, point.alpha = 0.6
                ) +
                    labs(x = props$xlab, y = props$ylab)
            })
        },
        
        # call
        call = function(input, output, session, props) {
            callModule(self$server, self$id, props)
        }
    )
)