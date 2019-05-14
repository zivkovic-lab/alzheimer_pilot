Sidebar = R6Class(
    "Sidebar",
    inherit = ShinyModule,
    public = list(
        # attributes
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            dashboardSidebar(
                sidebarMenu(
                    id = "tabs",
                    menuItem("Overview", tabName = "overview"),
                    menuItem("Linear Models", tabName = "linear-model"),
                    menuItem("Correlations", tabName = "correlation"),
                    menuItem('E3-E4 Subset', tabName = 'e3e4-subset')
                ),
                tags$br(),
                tags$hr(),
                sidebarMenu(
                    selectInput(
                        "dataset", "Select a dataset",
                        choices = names(data),
                        selected = names(data)[1]
                    )
                )
            )
        },
        
        # server
        #' @emit current_tab
        #' @emit dataset
        server = function(input, output, session){
            emit = reactiveValues(
                current_tab = NULL,
                dataset = NULL
            )
            
            observeEvent(input$tab, {
                emit$current_tab = input$tab
            })
            
            observeEvent(input$dataset, {
                emit$dataset = input$dataset
            })
            
            return(emit)
        }
    )
)