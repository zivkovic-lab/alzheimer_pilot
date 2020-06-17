FETPage = R6Class(
    "FETPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "fet",
        
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
                                width = 6,
                                numericInput(
                                    ns("cutoff"), "P-value Cutoff",
                                    min = 0, max = 1, step = 0.01, value = 0.1
                                )
                            ),
                            column(
                                width = 6,
                                radioButtons(
                                    ns("dir"), "Direction",
                                    choices = c("none", "increase", "decrease"),
                                    inline = TRUE
                                )
                            ),
                            column(
                                width = 12,
                                uiOutput(ns("pathways-ui")),
                            ),
                            column(
                                width = 12,
                                radioButtons(
                                    ns("preset"), "preset",
                                    choices = c("none", "mito-pathways", "apop-pathways"),
                                    inline = TRUE
                                )
                            ),
                            column(
                                width = 6,
                                numericInput(
                                    ns("nptw"), "# pathways to plot",
                                    min = 0, max = 50, step = 1,
                                    value = 10
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
            
            states = reactiveValues()
        
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
            
            observe({
                req(input$region)
                req(input$comp)
                req(input$cutoff)
                req(input$dir)
                states$ke = .DATA$enrichKEGG(
                    input$region, input$comp, input$cutoff, input$dir
                )
            })
            
            output$table = renderDT({
                df = states$ke@result[,1:7]
                if(!is.null(input$pathways)){
                    df = df[input$pathways,]
                }
                datatable(
                    df,
                    selection = list(mode = "single", selected = 1),
                    options = list(order = list(4, "asc")),
                    rownames = FALSE
                ) %>%
                    formatSignif(columns = 5:7, digits = 4)
            })
            
            output$plot = renderPlotly({
                .DATA$dotplot(states$ke, input$pathways, input$nptw)
            })
        }
    )
)