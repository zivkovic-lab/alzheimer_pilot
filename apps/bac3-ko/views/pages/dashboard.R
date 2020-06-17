import::here(Header, .from = "../layout/header.R")
import::here(Sidebar, .from = "../layout/sidebar.R")
import::here(Body, .from = "../layout/body.R")

Dashboard = R6Class(
    "Dashboard",
    public = list(
        header = NULL,
        sidebar = NULL,
        body = NULL,
        
        initialize = function() {
            self$header = Header$new()
            self$sidebar = Sidebar$new()
            self$body = Body$new()
        },
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
        }
    )
)