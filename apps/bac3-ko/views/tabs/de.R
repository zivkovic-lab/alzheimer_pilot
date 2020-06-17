DEPage = R6Class(
    "DEPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "de",
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                fluidRow(
                    column(
                        width = 6,
                        box(
                            width = NULL,
                            column(
                                width = 6,
                                selectInput(
                                    ns("region"), "Brain Region",
                                    choices = unique(.DATA$data$count$pdata$region)
                                )
                            ),
                            column(
                                width = 6,
                                selectInput(
                                    ns("comp"), "Comparison",
                                    choices = .DATA$getAllComparisons(),
                                    selected = 1
                                ),
                            ),
                            column(
                                width = 12,
                                uiOutput(ns("pathways-ui"))
                            ),
                            column(
                                width = 12,
                                radioButtons(
                                    ns("preset"), "preset",
                                    choices = c("none", "mito-pathways", "apop-pathways"),
                                    inline = TRUE
                                )
                            )
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            width = NULL,
                            plotlyOutput(ns("plot"))
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 12,
                        box(
                            width = NULL,
                            DTOutput(ns("table"))
                        )
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            states = reactiveValues(de = NULL)
            observe({
                req(!is.null(input$region))
                req(!is.null(input$comp))
                states$de = .DATA$getDETable(input$region, input$comp, input$pathways)
            })
            
            output$`pathways-ui` = renderUI({
                if(input$preset == "none") 
                    selected = NULL
                else if(input$preset == "mito-pathways")
                    selected = mito_ptws
                else if(input$preset == "apop-pathways")
                    selected = apop_ptws
                selectInput(
                    session$ns("pathways"), "Pathways",
                    selected = selected, choices = .DATA$getAllPathways(),
                    multiple = TRUE, selectize = TRUE
                )
            })
            
            observe({print(input$selected)})
            
            output$table = renderDT({
                datatable(
                    states$de,
                    selection = list(mode = "single", selected = 1),
                    options = list(order = list(4, "asc")),
                    rownames = FALSE
                ) %>%
                    formatSignif(columns = 2:6, digits = 4)
            })
            output$plot = renderPlotly({
                gene = states$de$ensembl[input$table_rows_selected]
                .DATA$boxplot(gene)
            })
        }
    )
)