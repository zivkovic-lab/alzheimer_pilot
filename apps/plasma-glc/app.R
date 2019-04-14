source("global.R")
import::here(Header, .from="components/layout/Header.R")
import::here(Sidebar, .from="components/layout/Sidebar.R")
import::here(Body, .from="components/layout/Body.R")

App = R6Class(
    "App",
    inherit = ShinyModule,
    public = list(
        # initialize
        header = Header$new(),
        sidebar = Sidebar$new(),
        body = Body$new(),
        
        # UI
        ui = function(){
            dashboardPage(
                header = self$header$ui(),
                sidebar = self$sidebar$ui(),
                body = self$body$ui()
            )
        },
        
        # server
        server = function(input, output, session){
            self$body$call()
            # shinyjs remove btn-default class
            shinyjs::removeClass(selector = ".btn", class = "btn-default")
        }
    )
)

app = App$new()

shiny::shinyApp(app$ui(), app$server)