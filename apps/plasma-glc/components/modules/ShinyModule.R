ShinyModule = R6Class(
    "ShinyModule",
    public = list(
        # attributes
        id = NULL,
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
        
        },
        
        # server
        server = function(input, output, session){
        
        },
        
        call = function(input, output, session){
            callModule(self$server, self$id)
        }
    )
)