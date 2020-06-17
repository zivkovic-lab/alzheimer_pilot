GSEAPage = R6Class(
    "GSEAPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "gsea",
        
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
                            plotOutput(ns("plot"))
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
            
            observeEvent(c(input$region, input$comp), {
                states$gsea = .DATA$gseaKEGG(input$region, input$comp)
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
            
            observe({
                dat = states$gsea[,1:10]
                if(!is.null(input$pathways)){
                    dat = dat[input$pathways,] 
                }
                states$table = dat
            })
            
            output$table = renderDT({
                datatable(
                    states$table,
                    selection = list(mode = "single", selected = 1),
                    options = list(order = list(6, "asc")),
                    rownames = FALSE
                ) %>%
                    formatSignif(columns = 4:8, digits = 4)
            }) 
            
            output$plot = renderPlot({
                geneSetID = rownames(states$table)[input$table_rows_selected]
                .DATA$gseaplot(states$gsea, geneSetID)
            })
        }
    )
)