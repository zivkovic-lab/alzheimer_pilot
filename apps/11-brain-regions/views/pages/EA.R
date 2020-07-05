PageEA = R6Class(
    "PageEA",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "pag_ea",
        
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
                        column(
                            width = 2,
                            radioButtons(
                                inputId = ns("test_type"),
                                label = "Test Type",
                                choiceNames = c(
                                    "Fisher's Exact Test",
                                    "Kilmogorov-Smirov Test"
                                ),
                                choiceValues = c("fet", "kst"),
                                selected = "fet"
                            )
                        ),
                        column(
                            width = 2,
                            radioButtons(
                                inputId = ns("test_alt"),
                                label = "Test Alternative",
                                choices = c("greater", "less", "two.sided"),
                                selected = "greater"
                            )
                        ),
                        column(
                            width = 3,
                            numericInput(
                                inputId = ns("pcutoff"),
                                label = "Pval cutoff",
                                min = 0, max = 1, value = 1, step = 0.01
                            ),
                            tags$div(
                                class = "help-tip float-right",
                                tags$p(paste0(
                                    "The p-value cutoff is only used when ",
                                    "when the test type is set as Fisher",
                                    "'s Exact test. If the cutoff is set ",
                                    "to 1, if only compares the mean of ",
                                    "AD and NonAD."
                                ))
                            )
                        ),
                        column(
                            width = 2,
                            radioButtons(
                                inputId = ns("data_type"),
                                label = "Data Type",
                                choices = names(.DATA$data)
                            )
                        ),
                        column(
                            width = 3,
                            selectInput(
                                inputId = ns("region"),
                                label = "Select brain region",
                                choices = unique(.DATA$data$rel_abund$pdata$region)
                            )
                        ),
                    )
                ),
                column(
                    width = 6,
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
                            tabPanel(
                                "Enrichment",
                                uiOutput(ns("plot-ui")),
                                uiOutput(ns("help_plot"))
                            ),
                            tabPanel(
                                "Barplot",
                                plotlyOutput(ns("barplot"))
                            )
                            # tabPanel(
                            #     "Heatmap",
                            #     plotlyOutput(ns("heatmap"))
                            # )
                        )
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$table = renderDT({
                .DATA$enrichment_test(
                    data_type = input$data_type,
                    region = input$region,
                    test_type = input$test_type,
                    alternative = input$test_alt,
                    cutoff = input$pcutoff
                )
                table = .DATA$get_enrichment_table()
                if(input$test_type == "fet"){
                    options = list(order = list(6, "asc"))
                    round = c("pval", "odds_ratio")
                }else {
                    options = list(order = list(2, "asc"))
                    round = c("d", "pval")
                }
                datatable(
                    table,
                    selection = list(mode = "single", selected = 1),
                    options = options
                ) %>%
                    formatSignif(round, digits = 4)
            })
            
            output$`plot-ui` = renderUI({
                height = if (input$test_type == "fet") "550px" else "400px"
                plotlyOutput(session$ns("plot"), height = height)
            })
            
            output$plot = renderPlotly({
                list(
                    input$data_type,input$region, input$test_type,
                    input$test_alt, input$pcutoff
                )
                if(!is.null(.DATA$ea))
                    .DATA$plot_enrichment(selected = input$table_rows_selected)
            })
            
            output$barplot = renderPlotly({
                list(
                    input$test_type, input$test_alt, input$pcutoff
                )
                if(!is.null(.DATA$ea))
                    .DATA$plot_enrichment_barplot(
                        data_type = input$data_type,
                        region = input$region,
                        selected = input$table_rows_selected
                    )
            })
            
            # output$heatmap = renderPlotly({
            #     .DATA$plot_enrichment_heatmap(
            #         data_type = input$data_type
            #     )
            # })
            
            output$help_plot = renderUI({
                if(input$test_type == "fet"){
                    text = paste0(
                        "The barplot is a visulization of the result of ",
                        "Fisher's exact test result. The portion filled in ",
                        "red correspondes to the x value in the table to the ",
                        "right, while the gray portion corespondes to m-x. ",
                        "Using the default settings, while Test Type is set ",
                        "to Fisher's Exact Test, there are 87 fucosylated ",
                        "glycans that are greater in AD comparing to NonAD"
                    )
                } else {
                    text = paste0(
                        "The ecdf (empirical cumulative distribution function",
                        ") is a representation of the result of Kilmogorov-",
                        "Smirov Test. The Kilmogorov-Smirov test tests whether",
                        " two sets of data come from different distributions. ",
                        "If the sample distribution (red) is above the ",
                        "reference (black), we say there are more smaller p ",
                        "values."
                    )
                }
                tags$div(
                    class = "help-tip float-right",
                    style = "margin-top: 1rem;",
                    tags$p(text)
                )
            })
        }
    )
)