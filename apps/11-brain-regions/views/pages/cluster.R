PageCluster = R6Class(
    "PageCluster",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "page_cluster",
        help_texts = list(
            paste0(
                "Kmeans needs to know how many number of clusters the dataset ",
                "has ahead of time."
            ),
            paste0(
                "Kmeans is a heuristic method, which means it does not give ",
                "the exact same result each time. Mostly, same poitns are ",
                "still groupped in the same cluss, but just the identifiers ",
                "are different. Thus the random seed is just a way to fix ",
                "this. Using the same random seed will always generate the ",
                "random variables thus the predure is reproducible."
            ),
            paste0(
                "The three plots here are used to decide the number of ",
                "clusters exists in the dataset. The blue vertical line is ",
                "suggested by the algorithm and the red is the current value ",
                "used from the select input above."
            )
        ),
        
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
                                width = 12,
                                radioButtons(
                                    ns("data-type"),
                                    "Datat Type",
                                    choices = names(.DATA$data),
                                    inline = TRUE
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 6,
                                    selectInput(
                                        ns("kms-transform"),
                                        "Transform",
                                        choices = c(
                                            "z-score",
                                            "abs-scale"
                                        )
                                    )
                                ),
                                column(
                                    width = 6,
                                    numericInput(
                                        ns("kms-centers"),
                                        "# Centers",
                                        min = 2, max = 10, step = 1, value = 4
                                    ),
                                    tags$div(
                                        class = "help-tip float-right",
                                        tags$p(style = "z-index: 100;", self$help_texts[[1]])
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    width = 6,
                                    selectInput(
                                        ns("kms-algorithm"),
                                        "Algorithm",
                                        choices = c(
                                            "Hartigan-Wong", "Forgy", "MacQueen"
                                        )
                                    )
                                ),
                                column(
                                    width = 6,
                                    numericInput(
                                        ns("kms-seed"),
                                        "Random seed",
                                        min = 1, max = 10000, step = 1, value = 123
                                    ),
                                    tags$div(
                                        class = "help-tip float-right",
                                        tags$p(style = "z-index: 100;", self$help_texts[[2]])
                                    )
                                )
                            )
                        ),
                        box(
                            width = NULL,
                            tabsetPanel(
                                tabPanel(
                                    "Silhouette",
                                    plotOutput(ns("kms-tuning-plot-1"))
                                ),
                                tabPanel(
                                    "Elbow",
                                    plotOutput(ns("kms-tuning-plot-2"))
                                ),
                                tabPanel(
                                    "Gap Statistic",
                                    plotOutput(ns("kms-tuning-plot-3"))
                                ),
                                tags$div(
                                    class = 'help-tip float-right',
                                    tags$p(style = "z-index: 100;", self$help_texts[[3]])
                                )
                            )
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            width = NULL,
                            plotlyOutput(ns("pca"))
                        )
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            stats = reactiveValues(
                update = 0
            )
            
            output$`kms-tuning-plot-1` = renderPlot({
                .DATA$plot_nbclust(
                    input$`data-type`, input$`kms-transform`, "silhouette",
                    input$`kms-centers`, algorithm = input$`kms-algorithm`
                )
            })
            
            output$`kms-tuning-plot-2` = renderPlot({
                .DATA$plot_nbclust(
                    input$`data-type`, input$`kms-transform`, "wss",
                    input$`kms-centers`, algorithm = input$`kms-algorithm`
                )
            })
            
            output$`kms-tuning-plot-3` = renderPlot({
                .DATA$plot_nbclust(
                    input$`data-type`, input$`kms-transform`, "gap_stat",
                    input$`kms-centers`, algorithm = input$`kms-algorithm`
                )
            })
            
            observeEvent(c(
                input$`data-type`, input$`kms-transform`,
                input$`kms-transform`, input$`kms-centers`, input$`kms-seed`
            ), {
                req(!is.null(input$`kms-transform`))
                print(input$`kms-transform`)
                .DATA$compute_clusters(
                    input$`data-type`, "kmeans",
                    transform = input$`kms-transform`,
                    centers = input$`kms-centers`,
                    algorithm = input$`kms-algorithm`,
                    seed = input$`kms-seed`
                )
                stats$update = stats$update + 1
            })
            
            observeEvent(stats$update, {
                if(stats$update == 0) return()
                output$pca = renderPlotly({
                    .DATA$plot_pca(input$`data-type` == "rel_abund", "cluster")
                })
            })
        }
    )
)