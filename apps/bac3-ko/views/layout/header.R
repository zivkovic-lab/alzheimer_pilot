Header = R6Class(
    "Header",
    inherit = ShinyModule,
    public = list(
        # attributes
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            dashboardHeader(
                title = "BAC KO Mice",
                tags$li(
                    class = "nav-item",
                    tags$a(
                        class = "nav-link",
                        href = "#",
                        textOutput("greeting")
                    )
                ),
                tags$li(
                    class = "nav-item",
                    tags$a(
                        class = "nav-link",
                        href = "http://www.chenghaozhu.net/study-docs/fasting/",
                        tags$span(
                            icon("home"), "HOME"
                        )
                    )
                ),
                tags$li(
                    class = "nav-item",
                    tags$a(
                        class = "btn btn-danger action-button",
                        id = "logout-button",
                        type = "button",
                        icon("sign-out-alt"), "Log out"
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
        
        }
    )
)