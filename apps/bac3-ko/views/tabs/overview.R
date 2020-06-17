OverviewPage = R6Class(
    "OverviewPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "overview",
        
        # initializer
        initialize = function(){
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            fluidRow(
                column(
                    width = 12,
                    box(
                        width = NULL,
                        tabsetPanel(
                            tabPanel(
                                "Feature Data",
                                DTOutput(ns("fdata"))
                            ),
                            tabPanel(
                                "Sample Data",
                                DTOutput(ns("pdata"))
                            )
                        )
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$pdata = renderDT({
                datatable(.DATA$getPdata())
            })
            output$fdata = renderDT({
                datatable(.DATA$getFdata(), rownames = FALSE)
            })
        }
    )
)