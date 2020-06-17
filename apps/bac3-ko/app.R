source("global.R")
import::here(Dashboard, .from = "views/pages/dashboard.R")
import::here(Auth, .from = "views/pages/auth.R")

App = R6Class(
    "App",
    inherit = ShinyModule,
    public = list(
        # attributes
        auth = NULL,
        dashboard = NULL,
        router = NULL,
        
        # initializer
        initialize = function(){
            #self$auth = Auth$new()
            self$dashboard = Dashboard$new()
            self$auth = Auth$new()
            self$router = make_router(
                default = route("login", self$auth$ui(), self$auth$server),
                route("dashboard", self$dashboard$ui(), self$dashboard$server)
            )
        },
        
        # UI
        ui = function(){
            router_ui()
        },
        
        # server
        server = function(input, output, session){
            self$router(input, output, session)
            
            observe({
                if(is.null(.AUTH$credentials)){
                    change_page("/login", mode = "push")
                }else if (is.null(.AUTH$credentials()$user_auth)) {
                    change_page("/login", mode = "push")
                }else if(.AUTH$credentials()$user_auth){
                    change_page("/dashboard", mode = "push")
                } else {
                    print(.AUTH$credentials()$user_auth)
                    change_page("/login", mode = "push")
                }
            })
            observe({
                if(is_page("dashboard")) {
                    session$onFlushed(function(){
                        shinyjs::addClass(selector = "html body", class = "skin-blue")
                    }, once = FALSE)
                }
            })
        }
    )
)

app = App$new()
shinyApp(ui = app$ui(), server = app$server)