import::here(OverviewPage, .from="../pages/OverviewPage.R")
import::here(LinearModelPage, .from="../pages/LinearModelPage.R")
import::here(CorrelationPage, .from="../pages/CorrelationPage.R")
import::here(E3E4SubsetPage, .from='../pages/E3E4SubsetPage.R')

Body = R6Class(
    "Body",
    inherit = ShinyModule,
    public = list(
        # attributes
        overviewPage = NULL,
        normalityPage = NULL,
        linearModelPage = NULL,
        correlationPage = NULL,
        e3e4SubsetPage = NULL,
        
        # initializer
        initialize = function(){
            self$overviewPage = OverviewPage$new()
            self$linearModelPage = LinearModelPage$new()
            self$correlationPage = CorrelationPage$new()
            self$e3e4SubsetPage = E3E4SubsetPage$new()
        },
        
        # UI
        ui = function(){
            dashboardBody(
                tags$link(href="styles.css", rel="stylesheet"),
                shinyjs::useShinyjs(),
                fluidPage(
                    tabItems(
                        tabItem("overview", self$overviewPage$ui()),
                        tabItem("linear-model", self$linearModelPage$ui()),
                        tabItem("correlation", self$correlationPage$ui()),
                        tabItem('e3e4-subset', self$e3e4SubsetPage$ui())
                    )
                )
            )
        },
        
        # server
        #' @props current_tab
        #' @props dataset
        server = function(input, output, session, props){
            
            self$overviewPage$call()
            self$linearModelPage$call(props = reactiveValues(
                dataset = props$dataset
            ))
            self$correlationPage$call(props = reactiveValues(
                dataset = props$dataset
            ))
            self$e3e4SubsetPage$call(props = reactiveValues(
                dataset = props$dataset
            ))
        }
    )
)