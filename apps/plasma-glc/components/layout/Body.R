import::here(OverviewPage, .from="../pages/OverviewPage.R")
#import::here(NormalityPage, .from="../pages/NormalityPage.R")
import::here(LinearModelPage, .from="../pages/LinearModelPage.R")
import::here(CorrelationPage, .from="../pages/CorrelationPage.R")

Body = R6Class(
    "Body",
    inherit = ShinyModule,
    public = list(
        # attributes
        overviewPage = NULL,
        normalityPage = NULL,
        linearModelPage = NULL,
        correlationPage = NULL,
        
        # initializer
        initialize = function(){
            self$overviewPage = OverviewPage$new()
            #self$normalityPage = NormalityPage$new()
            self$linearModelPage = LinearModelPage$new()
            self$correlationPage = CorrelationPage$new()
        },
        
        # UI
        ui = function(){
            dashboardBody(
                tags$link(href="styles.css", rel="stylesheet"),
                shinyjs::useShinyjs(),
                fluidPage(
                    tabItems(
                        tabItem("overview", self$overviewPage$ui()),
                        #tabItem("normality", self$normalityPage$ui()),
                        tabItem("linear-model", self$linearModelPage$ui()),
                        tabItem("correlation", self$correlationPage$ui())
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
        },
        
        call = function(input, output, session, props){
            callModule(self$server, self$id, props)
        }
    )
)