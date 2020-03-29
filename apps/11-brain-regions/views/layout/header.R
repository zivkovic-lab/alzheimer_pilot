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
            dashboardHeader(title = "11 Brain Regions")
        },
        
        # server
        server = function(input, output, session){
        
        }
    )
)