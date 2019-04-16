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
            
            tagList(
                column(
                    width = 8,
                    box(
                        width = NULL,
                        dataTableOutput(ns("pdata-table"))
                    )
                ),
                column(
                    width = 4,
                    box(
                        width = NULL,
                        verbatimTextOutput(ns("summary"))
                    ),
                    box(
                        width = NULL,
                        plotlyOutput(ns("plot"))
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            
            pdata = data$peptides$sample_table
        
            output$`pdata-table` = renderDataTable(
                pdata,
                selection = list(
                    target = "column",
                    mode = "single",
                    selected = 1
                ),
                options = list(
                    pageLength = 48
                )
            )
            
            # variable summary
            output$summary = renderText({
                out = capture.output(summary(pdata[,input$`pdata-table_columns_selected`]))
                paste(out, collapse = "\n")
            })
            
            # histogram/barplot
            output$plot = renderPlotly({
                if(!is.null(input$`pdata-table_columns_selected`)){
                    selected = pdata[,input$`pdata-table_columns_selected`]
                    if(is.numeric(selected)) {
                        data.frame(
                            value = selected
                        ) %>%
                            ggplot(aes(x = value)) +
                            geom_histogram(bins = 25, color = "white", fill = "steelblue") 
                    } else {
                        data.frame(
                            value = selected
                        ) %>%
                            ggplot(aes(x = value)) +
                            geom_bar(fill = "steelblue")
                    }   
                }
            })
        }
    )
)