PageOverview = R6Class(
    "PageOverview",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "overview",
        
        # initializer
        initialize = function(){},
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                column(
                    width = 12,
                    box(
                        width = NULL,
                        tabsetPanel(
                            tabPanel(
                                "sample metadata",
                                DTOutput(ns("samples"))
                            ),
                            tabPanel(
                                "feature data",
                                DTOutput(ns("features"))
                            )
                        )
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$samples = renderDT({
                .DATA$data$abs_abund$pdata
            })
            output$features = renderDT({
                .DATA$data$abs_abund$fdata
            })
        }
    )
)