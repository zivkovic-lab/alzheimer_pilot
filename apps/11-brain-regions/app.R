source("global.R")
import::here(Header, .from="views/layout/header.R")
import::here(Sidebar, .from="views/layout/sidebar.R")
import::here(Body, .from="views/layout/body.R")

App = R6Class(
    "App",
    inherit = ShinyModule,
    public = list(
        # attributes
        header = Header$new(),
        sidebar = Sidebar$new(),
        body = Body$new(),
        
        # initializer
        initialize = function(){},
        
        # UI
        ui = function(){
            dashboardPage(
                header = self$header$ui(),
                sidebar = self$sidebar$ui(),
                body = self$body$ui(),
            )
        },
        
        # server
        server = function(input, output, session){
            self$sidebar$call()
            self$body$call()
        }
    )
)

app = App$new()

shinyApp(ui = app$ui(), server = app$server)
