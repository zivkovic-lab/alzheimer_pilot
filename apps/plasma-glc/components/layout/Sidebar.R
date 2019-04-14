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
                    menuItem("AD vs Normal", tabName = "ad-normal")
                )
            )
        },
        
        # server
        server = function(input, output, session){
        
        }
    )
)