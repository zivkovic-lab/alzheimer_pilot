VolcanoPlotPanel = R6Class(
    "VolcanoPlotPanel",
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
                width = 12,
                plotlyOutput(ns("plot"))
            )
        },
        
        # server
        #' @props data
        #' @props selected
        server = function(input, output, session, props){
        
            output$plot = renderPlotly({
                if(!is.na(props$data)){
                    props$data %>%
                        tibble::rownames_to_column("feature") %>%
                        ggplot(aes(x = logFC, y = -log(pvalue), pvalue = pvalue))+
                        geom_hline(yintercept = -log(0.05), linetype='dashed') +
                        geom_point(aes(feature = feature), color = "steelblue",
                                   alpha = 0.6) +
                        geom_point(
                            data = function(x){subset(x, feature == props$selected)},
                            aes(feature = feature),
                            color = "firebrick"
                        )
                }
            })
            
        },
        
        # call
        call = function(input, output, session, props){
            callModule(self$server, self$id, props)
        }
    )
)