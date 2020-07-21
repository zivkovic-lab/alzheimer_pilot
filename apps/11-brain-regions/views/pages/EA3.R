PageEA3 = R6Class(
    "PageEA3",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "page_ea3",
        data = reactiveValues(),
        helper_text = list(
            "In this enrichment analysis, we are interested in seeing wheather
            a particular group of glycans were enriched in a brain region
            regardless of the disease condition. I first calculate the mean of
            each glycan of each patient across all 11 brian regions. And I then
            treat this average value as a reference, and compare the brian
            region of interest to it, using a paired t-test. The paired-test
            results were then used to do Fishier's exact test."
        ),
        
        # initializer
        initialize = function(){},
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                column(
                    width = 12,
                    tags$div(
                        class = "alert alert-info",
                        #style = "opacity:0.4",
                        role = "alert",
                        self$helper_text[[1]],
                        tags$button(
                            type = "button", class = "close", 
                            `data-dismiss` = "alert", `aria-label` = "Close",
                            tags$span(`aria-hidden` = "true", HTML("&times;"))
                        )
                    )
                ),
                column(
                    width = 12,
                    box(
                        width = NULL,
                        column(
                            width = 3,
                            radioButtons(
                                inputId = ns("test_alt"),
                                label = "Test Alternative",
                                choices = c("greater", "less"),
                                selected = "greater"
                            )
                        ),
                        column(
                            width = 3,
                            numericInput(
                                inputId = ns("cutoff"),
                                label = "P-value cutoff",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0.1
                            )
                        ),
                        column(
                            width = 3,
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
                        )
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
                                plotlyOutput(ns("plot"), height = "550px")
                            ),
                            tabPanel(
                                "Barplot",
                                plotlyOutput(ns("barplot"))
                            )
                        )
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$table = renderDT({
                self$data$data = .DATA$fet_with_region_data(input$data_type, input$region)
                self$data$ea = .DATA$fet_with_region(self$data$data, input$test_alt, input$cutoff)
                datatable(
                    self$data$ea,
                    selection = list(mode = "single", selected = 1),
                    options = list(order = list(6, "asc"))
                ) %>%
                    formatSignif(c("pval", "odds_ratio"), digits = 4)
            })
            
            output$plot = renderPlotly({
                if(input$test_alt == "less"){
                    labels = c("not less", "less")
                } else{
                    labels = c("not greater", "greater")
                }
                .DATA$plot_fet(self$data$ea, self$data$ea[,"pval"], labels)
            })
            
            output$barplot = renderPlotly({
                glycan_type = rownames(self$data$ea)[input$table_rows_selected]
                data = self$data$data
                edata = data$edata[sapply(data$fdata$subtype, function(x) glycan_type %in% x),]
                df = data.frame(
                    value = colSums(edata),
                    group = data$pdata$region,
                    patient = data$pdata$patient[1:4]
                )
                ggplot(df) +
                    geom_col(
                        aes(patient, value, fill = group), position = "dodge",
                        width = 0.75, color = "black"
                    ) +
                    theme_bw()
            })
        }
    )
)