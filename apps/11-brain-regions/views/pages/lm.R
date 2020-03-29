PageLM = R6Class(
    "PageLM",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "page_lm",
        # initializer
        initialize = function(){},
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                column(
                    width = 6,
                    box(
                        width = NULL,
                        column(
                            width = 6,
                            radioButtons(
                                inputId = ns("data_type"),
                                label = "Data Type",
                                choices = names(.DATA$data),
                                inline = TRUE
                            )
                        ),
                        column(
                            width = 6,
                            selectInput(
                                inputId = ns("region"),
                                label = "Select brain region",
                                choices = unique(.DATA$data$rel_abund$pdata$region)
                            )
                        )
                    ),
                    box(
                        width = NULL,
                        DTOutput(ns("table"))
                    )
                ),
                column(
                    width = 6,
                    box(
                        width = NULL,
                        tabsetPanel(
                            tabPanel("Volcano Plot", plotlyOutput(ns("volcano"))),
                            tabPanel("MA Plot", plotlyOutput(ns("ma"))),
                            tabPanel("Histogram", plotlyOutput(ns("hist")))
                        )
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$table = renderDT({
                table = .DATA$lm_table(
                    data_type = input$data_type,
                    region = input$region
                )
                datatable(
                    table,
                    selection = list(mode = "single", selected = 1),
                    options = list(order = list(5, "asc"))
                ) %>%
                    formatSignif(
                        columns = c("logFC", "mean", "stat", "pval", "padj"),
                        digits = 4
                    )
            })
            output$volcano = renderPlotly({
                .DATA$plot_volcano(
                    data_type = input$data_type,
                    region = input$region,
                    selected = input$table_rows_selected
                )
            })
            output$ma = renderPlotly({
                .DATA$plot_ma(
                    data_type = input$data_type,
                    region = input$region,
                    selected = input$table_rows_selected
                )
            })
            output$hist = renderPlotly({
                .DATA$plot_lm_hist(
                    data_type = input$data_type,
                    region = input$region
                )
            })
        }
    )
)