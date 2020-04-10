PageEA2 = R6Class(
    "PageEA2",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "page_ea2",
        ea = reactiveValues(),
        
        # initializer
        initialize = function(){},
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                column(
                    width = 12,
                    tags$div(
                        class = "alert alert-danger",
                        #style = "opacity:0.4",
                        role = "alert",
                        "This is the enrichment analysis of glycans subtypes in 
                        specific age group in AD vs NonAD. Because there is only
                        at most one patient in each group/age, we can not run
                        any linear model. The KS-test is not applicable. And the 
                        FE test is don solely compaing the value between two
                        patients. Thus please take great caution when interpreting
                        this result.",
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
                        column(
                            width = 3,
                            radioButtons(
                                inputId = ns("age"),
                                label = "Age group",
                                choices = c("70s", "90s"),
                                selected = "70s"
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
                        plotlyOutput(ns("plot"), height = "550px")
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$table = renderDT({
                self$ea$mat = .DATA$fet_with_age(input$data_type, input$region, input$age, input$test_alt)
                datatable(
                    self$ea$mat,
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
                .DATA$plot_fet(self$ea$mat, self$ea$mat[,"pval"], labels)
            })
        }
    )
)