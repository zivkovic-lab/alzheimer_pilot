PagePCA = R6Class(
    "PagePCA",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "page_pca",
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                fluidRow(
                    column(
                        width = 12,
                        box(
                            width = NULL,
                            radioButtons(
                                ns("color"),
                                "color by:",
                                choices = c("region", "group"),
                                selected = "group",
                                inline = TRUE
                            )
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 6,
                        box(width = NULL, plotlyOutput(ns("plot_abs")))
                    ),
                    column(
                        width = 6,
                        box(width = NULL, plotlyOutput(ns("plot_rel")))
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$plot_abs = renderPlotly({
                .DATA$plot_pca(rel_abund = FALSE, color = input$color)
            })
            output$plot_rel = renderPlotly({
                .DATA$plot_pca(rel_abund = TRUE, color = input$color)
            })
        }
    )
)